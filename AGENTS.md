# AGENTS.md

stRs makes a GitHub user's stars searchable as you type. It is an Elm app styled with
[elmcn](https://github.com/ricardocasares/elmcn): shadcn/ui components for Elm. Like
shadcn/ui, elmcn is not a package. The CLI copies a component's source into
`src/UI/<Name>.elm`, and from then on the code is ours to read and change.

## Product rules

- It works without a GitHub token. Never fetch anything that needs one, such as star lists.
- Search runs on the local index only. Never call the API while the user types.
- Sync incrementally and spend the rate limit carefully. Respect 403 and 429 responses and
  `Retry-After`, and tell the user without alarm.
- `/strs/` shows ricardocasares's stars and `/strs/<login>` shows anyone else's. The UI has
  no user switcher.
- The query, filters and sort live in the URL, so any view can be shared.
- It should feel fast and native, on phones as well as desktops. Performance is a feature.

## UI

- Copy elmcn's docs examples (`gh api repos/ricardocasares/elmcn/contents/docs/Examples`):
  their structure, classes and component defaults. Don't invent styling: no boxed icons and
  no custom text weights or colours. Menu icons use `text-muted-foreground`.
- For each piece of state, choose the elmcn component that represents it best.
- Global notifications are elmcn toasts. Each is one line that never wraps, with a Retry
  action where retrying makes sense.
- Prefer skeletons shaped like the final content to spinners, so nothing shifts when data
  arrives. A control that has sensible defaults shows the defaults instead of a skeleton.
  While stars sync, a progress bar moves smoothly.
- Keep copy short and never redundant. Avoid the tells of AI-generated UI.
- Icons are Phosphor icons, used through `src/Icon.elm`. All text is set in Inter, with
  ui.shadcn.com's body text settings.
- When your class must override a component's own class, check in the browser that it does.
  Add `!` if it doesn't.
- The top bar sits on a half-width sticky rail so that iOS Safari shows its blur behind the
  status bar. Keep that structure (see `viewTopBar`).

## Elm

- Design types so that impossible states can't be represented. Follow the `elm-patterns`
  skill in `.agents/skills/`.
- Elm's debugger runs in dev and walks the model one stack frame per element. Store large
  collections as `Dict`s, not `List`s or `Array`s.
- Call `elm()` in `vite.config.ts` with no options. Its defaults turn the debugger on in dev
  and keep it out of production.
- Keep each main-thread task short, so a slow phone keeps answering taps. Lighthouse runs
  mobile at a 4× slower CPU and counts every task over 50ms. Work that walks the whole
  library either runs in batches (`Index.settle`) or is spaced out (`refreshWhileSyncing`).

## elmcn components

Search and inspect the registry with the shadcn CLI. No `components.json` is needed.

```sh
bunx shadcn@latest search ricardocasares/elmcn -q dialog
bunx shadcn@latest view ricardocasares/elmcn/dialog
bunx shadcn@latest add ricardocasares/elmcn/card ricardocasares/elmcn/input
```

- `add` writes each item, and the components it uses, to `src/UI/<Name>.elm`. For example,
  `alert-dialog` becomes `UI.AlertDialog`. Read the notes it prints, because some items need
  an Elm package or a script that `src/main.ts` must import.
- Before overwriting a component you changed, run `add ... --diff`.
- Never write a component by hand if the registry has it. Read an installed module to learn
  its API.
- Components are builders: `X.new`, then content setters, then variant and size setters, then
  `toHtml`. Required content is enforced by the type. Break the line after `X.new` and put
  each `|>` on its own line:

```elm
Button.new
    |> Button.text "Save"
    |> Button.outline
    |> Button.small
    |> Button.toHtml [ onClick Save ]
```

## Architecture

All behaviour lives in Elm. TypeScript (`src/main.ts`, `src/idb.ts`) only wraps browser APIs:
IndexedDB, localStorage, `window.open` and the `<in-view>` IntersectionObserver element. Don't move logic
there. When a feature seems to need JavaScript, keep the logic in Elm and expose only the
browser capability through a typed port.

- `GitHub.elm` covers the two unauthenticated endpoints (`/users/{login}` and
  `/users/{login}/starred`) and what each response means for the rate limit. The API URL is
  a constant, so it needs no env var.
- `Sync.elm` does incremental sync as pure functions. A full sync walks every page and
  prunes. A delta sync stops at the first star already known. The cursor is saved with every
  page.
- `Index.elm` is BM25F with prefix matching. Length normalisation happens when a query is
  scored, so the index never needs rebuilding. New repos are listed at once but indexed for
  search in batches, and the index keeps facet counts for the unfiltered view. `Tokens.elm`
  splits words and highlights matches.
- `Link.elm` keeps the URL's query string in sync with the app. The URL is rewritten with
  `replaceUrl` once typing pauses, never on each keystroke.
- `Main.elm` holds the state machine (`Sync`: `Idle | Running | Paused | Stalled`), the
  rate-limit budget and the views.

## Project

- Use `bun` and `bunx`, never npm, npx or node. `bun run dev` starts Vite.
- `src/main.ts` starts `src/Main.elm`. `src/main.css` is the only stylesheet, holding
  Tailwind and the theme. `index.html` links it, so don't import CSS from TypeScript.
- After you change `src/InteropDefinitions.elm`, run `bun run types`. `dev` and `build`
  run it for you.
- `index.html` sets `<base href="%BASE_URL%">` and the app uses relative URLs. Don't set
  `base` in `vite.config.ts` or pass it as a flag. The Pages workflow builds with
  `--base=/strs/`.
- Pushing to `main` deploys the app to ricardocasares.github.io/strs/. `vite.config.ts`
  reads the commit from `git rev-parse HEAD` and passes it to Elm as a flag. The user
  menu's Release group links to that commit.
- Check UI changes in the running app with agent-browser before you call them done.

## Developing without spending the rate limit

Without a token, GitHub allows 60 requests an hour per IP. `bun run dev:mock` serves stars
recorded in `dev/fixtures` instead. Record more with `bun run record <login>`, which uses
the authenticated `gh` CLI. `dev/mock-github.ts` documents the settings at
`/__github/__control`, which simulate rate limits, 429s, new stars and going offline.

## Logo

The wordmark is "stRs" in Major Mono Display, whose lowercase letters are thin capitals and
whose uppercase R has the triangle. Both the wordmark (`src/Logo.elm`) and the favicon
(`public/favicon.svg`) are that font's outlines, baked into SVG, so the app loads no logo
font. Keep it that way.

## TypeScript

All scripts are TypeScript run by Bun, including the ones in `dev/`. `bun run check` must
pass with the strict settings in `tsconfig.json`.

- Never use type assertions: no `as`, including `as const`, and no non-null `!`. Narrow
  with type guards, or type values through annotations.
- `JSON.parse` and other `any` sources go into `unknown`, which you then narrow.
- Every switch over a union ends with `default: x satisfies never`. A new variant from Elm
  then fails to type-check until it is handled.
- An untyped dependency gets a `declare module` covering just the parts we use.
