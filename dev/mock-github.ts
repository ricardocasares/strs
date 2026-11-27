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
// Fixtures: <dir>/<login>/user.json and page-<n>.json, recorded with
//   gh api -H "Accept: application/vnd.github.star+json" "users/<login>/starred?per_page=100&page=<n>"
// Any other login is served from the first fixture; "nobody" is a 404.

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
  const cache = new Map<string, unknown[]>();
  const fixtureLogin = () => readdirSync(dir).find((name) => existsSync(join(dir, name, "user.json")))!;

  const stars = (login: string) => {
    const source = existsSync(join(dir, login, "user.json")) ? login : fixtureLogin();
    if (!cache.has(source)) {
      const pages = readdirSync(join(dir, source))
        .filter((name) => name.startsWith("page-"))
        .sort((a, b) => parseInt(a.slice(5)) - parseInt(b.slice(5)));
      cache.set(source, pages.flatMap((page) => JSON.parse(readFileSync(join(dir, source, page), "utf8"))));
    }
    return { source, all: cache.get(source)!.slice(state.hide) };
  };

  return {
    name: "mock-github",
    apply: "serve",
    transform(code, id) {
      if (id.endsWith(".elm") || id.includes(".elm?")) return code.replaceAll("https://api.github.com", "/__github");
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
          const numeric: ("limit" | "window" | "retryAfter" | "hide" | "latency")[] = ["limit", "window", "retryAfter", "hide", "latency"];
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

        if (state.offline) return req.socket.destroy();

        const now = Math.floor(Date.now() / 1000);
        if (now >= state.resetAt) {
          state.used = 0;
          state.resetAt = now + state.window;
        }
        const rate = () => ({
          "x-ratelimit-limit": String(state.limit),
          "x-ratelimit-remaining": String(Math.max(0, state.limit - state.used)),
          "x-ratelimit-reset": String(state.resetAt),
        });

        if (state.retryAfter > 0) {
          const seconds = state.retryAfter;
          state.retryAfter = 0;
          return send(429, { message: "secondary rate limit" }, { ...rate(), "retry-after": String(seconds) });
        }
        if (state.used >= state.limit) return send(403, { message: "API rate limit exceeded" }, rate());
        state.used++;

        const match = url.pathname.match(/^\/users\/([^/]+)(\/starred)?$/);
        if (!match || match[1] === "nobody") return send(404, { message: "Not Found" }, rate());
        const login = match[1];

        if (!match[2]) {
          const { source } = stars(login);
          const user = JSON.parse(readFileSync(join(dir, source, "user.json"), "utf8"));
          const body = { ...user, login: source === login ? user.login : login };
          const etag = `W/"user-${login}"`;
          if (req.headers["if-none-match"] === etag) return send(304, null, { ...rate(), etag });
          return send(200, body, { ...rate(), etag });
        }

        const { all } = stars(login);
        const perPage = Number(url.searchParams.get("per_page") ?? 30);
        const page = Number(url.searchParams.get("page") ?? 1);
        const last = Math.max(1, Math.ceil(all.length / perPage));
        const body = all.slice((page - 1) * perPage, page * perPage);
        const etag = `W/"${login}-${page}-${all.length}-${state.hide}"`;
        const link = (n: number) =>
          `<http://${req.headers.host}/__github/users/${login}/starred?per_page=${perPage}&page=${n}>`;
        const links = [
          page < last ? `${link(page + 1)}; rel="next"` : null,
          page < last ? `${link(last)}; rel="last"` : null,
          page > 1 ? `${link(1)}; rel="first"` : null,
        ].filter(Boolean);

        if (req.headers["if-none-match"] === etag) return send(304, null, { ...rate(), etag });
        return send(200, body, { ...rate(), etag, ...(links.length ? { link: links.join(", ") } : {}) });
      });
    },
  };
};
