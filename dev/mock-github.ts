// A stand-in for api.github.com during development, served by Vite under /__github. The app
// calls api.github.com; this plugin points the compiled Elm at the mock instead.
// It replays recorded pages of stars, so working on the app doesn't spend the real
// 60-requests-an-hour budget, and it can misbehave on purpose:
//
//   /__github/__control?limit=5&window=60   a primary limit of 5 requests per minute (the
//                                           default is 5000 an hour, so syncing never stops)
//   /__github/__control?retryAfter=3        answer the next request with 429, retry-after 3
//   /__github/__control?hide=7              pretend the 7 newest stars don't exist yet
//   /__github/__control?latency=400         slow every response down
//   /__github/__control?offline=1           drop every request
//
// Fixtures are <dir>/<login>/user.json and page-<n>.json, recorded with
// `bun run record <login>`. Any other login is served from the first fixture; "nobody" is
// a 404.

import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import type { Plugin } from "vite";

type State = {
  limit: number;
  window: number;
  used: number;
  resetAt: number;
  retryAfter: number;
  hide: number;
  latency: number;
  offline: boolean;
};

// The settings /__control takes as numbers.
const numeric = ["limit", "window", "retryAfter", "hide", "latency"] satisfies (keyof State)[];

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const readJson = (path: string): unknown => JSON.parse(readFileSync(path, "utf8"));

const pageNumber = (file: string) => Number(/^page-(\d+)\.json$/.exec(file)?.[1]);

export const mockGitHub = (dir: string): Plugin => {
  const state: State = {
    // Plenty by default, like an authenticated client: lower it to test rate limits.
    limit: 5000,
    window: 3600,
    used: 0,
    resetAt: 0,
    retryAfter: 0,
    hide: 0,
    latency: 150,
    offline: false,
  };

  const fixtureLogin = () => {
    const login = readdirSync(dir).find((name) => existsSync(join(dir, name, "user.json")));
    if (login === undefined) throw new Error(`No fixtures in ${dir}: record some with bun run record <login>`);
    return login;
  };

  const user = (login: string): Record<string, unknown> => {
    const value = readJson(join(dir, login, "user.json"));
    if (!isRecord(value)) throw new Error(`${login}/user.json isn't a user`);
    return value;
  };

  // Every page of a fixture, oldest page first, read once.
  const cache = new Map<string, unknown[]>();
  const load = (login: string): unknown[] =>
    readdirSync(join(dir, login))
      .filter((file) => Number.isInteger(pageNumber(file)))
      .sort((a, b) => pageNumber(a) - pageNumber(b))
      .flatMap((file): unknown[] => {
        const page = readJson(join(dir, login, file));
        if (!Array.isArray(page)) throw new Error(`${login}/${file} isn't a page of stars`);
        return page;
      });

  const stars = (login: string) => {
    const source = existsSync(join(dir, login, "user.json")) ? login : fixtureLogin();
    const all = cache.get(source) ?? load(source);
    cache.set(source, all);
    return { source, all: all.slice(state.hide) };
  };

  return {
    name: "mock-github",
    apply: "serve",
    transform(code, id) {
      return id.endsWith(".elm") || id.includes(".elm?") ? code.replaceAll("https://api.github.com", "/__github") : null;
    },
    configureServer(server) {
      server.middlewares.use("/__github", (req, res) => {
        const url = new URL(req.url ?? "/", "http://localhost");
        const send = (status: number, body: unknown, headers: Record<string, string> = {}) => {
          setTimeout(() => {
            res.writeHead(status, {
              "content-type": "application/json",
              "access-control-allow-origin": "*",
              "access-control-allow-headers": "Accept, If-None-Match, X-GitHub-Api-Version",
              "access-control-expose-headers": "ETag, Link, Retry-After, X-RateLimit-Remaining, X-RateLimit-Reset",
              ...headers,
            });
            res.end(body === null ? undefined : JSON.stringify(body));
          }, state.latency);
        };

        if (req.method === "OPTIONS") return send(204, null);

        if (url.pathname === "/__control") {
          for (const key of numeric) {
            const value = url.searchParams.get(key);
            if (value !== null) state[key] = Number(value);
          }
          if (url.searchParams.has("offline")) state.offline = url.searchParams.get("offline") === "1";
          if (url.searchParams.has("limit") || url.searchParams.has("window")) {
            state.used = 0;
            state.resetAt = 0;
          }
          return send(200, state);
        }

        if (state.offline) {
          req.socket.destroy();
          return;
        }

        const now = Math.floor(Date.now() / 1000);
        if (now >= state.resetAt) {
          state.used = 0;
          state.resetAt = now + state.window;
        }
        const rate = {
          "x-ratelimit-limit": String(state.limit),
          "x-ratelimit-remaining": String(Math.max(0, state.limit - state.used)),
          "x-ratelimit-reset": String(state.resetAt),
        };

        if (state.retryAfter > 0) {
          const seconds = state.retryAfter;
          state.retryAfter = 0;
          return send(429, { message: "secondary rate limit" }, { ...rate, "retry-after": String(seconds) });
        }
        if (state.used >= state.limit) return send(403, { message: "API rate limit exceeded" }, rate);
        state.used++;

        const route = /^\/users\/([^/]+)(\/starred)?$/.exec(url.pathname);
        const login = route?.[1];
        if (login === undefined || login === "nobody") return send(404, { message: "Not Found" }, rate);

        if (route?.[2] === undefined) {
          const { source } = stars(login);
          const profile = user(source);
          const body = { ...profile, login: source === login ? profile["login"] : login };
          const etag = `W/"user-${login}"`;
          if (req.headers["if-none-match"] === etag) return send(304, null, { ...rate, etag });
          return send(200, body, { ...rate, etag });
        }

        const { all } = stars(login);
        const perPage = Number(url.searchParams.get("per_page") ?? 30);
        const page = Number(url.searchParams.get("page") ?? 1);
        const last = Math.max(1, Math.ceil(all.length / perPage));
        const body = all.slice((page - 1) * perPage, page * perPage);
        const etag = `W/"${login}-${page}-${all.length}-${state.hide}"`;
        const link = (n: number) =>
          `<http://${req.headers.host}/__github/users/${login}/starred?per_page=${perPage}&page=${n}>`;
        const links: string[] = [];
        if (page < last) links.push(`${link(page + 1)}; rel="next"`, `${link(last)}; rel="last"`);
        if (page > 1) links.push(`${link(1)}; rel="first"`);

        if (req.headers["if-none-match"] === etag) return send(304, null, { ...rate, etag });
        return send(200, body, { ...rate, etag, ...(links.length > 0 ? { link: links.join(", ") } : {}) });
      });
    },
  };
};
