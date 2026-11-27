# AGENTS.md

This is an Elm app styled with [elmcn](https://github.com/ricardocasares/elmcn): shadcn/ui
components for Elm. Like shadcn/ui, elmcn is not a package. The CLI copies a component's
source into `src/UI/<Name>.elm`, and from then on the code is ours to read and change.

## Find components

Search the registry with the shadcn CLI. No `components.json` is needed.

```sh
npx shadcn@latest search ricardocasares/elmcn
npx shadcn@latest search ricardocasares/elmcn -q dialog
```

Each result is an item name such as `button` or `alert-dialog`. To see what an item
installs and which Elm packages it needs, view it:

```sh
npx shadcn@latest view ricardocasares/elmcn/dialog
```

The installed components are the modules in `src/UI/`. Read one to learn its API before
using it.

## Install components

```sh
npx shadcn@latest add ricardocasares/elmcn/card ricardocasares/elmcn/input
```

- Each item is written to `src/UI/<Name>.elm`, together with the components it uses.
  `alert-dialog` becomes `UI.AlertDialog`.
- Read the notes the CLI prints after the install. Some items need an Elm package, for
  example `elm install elm/json`, or a script that `src/main.ts` must import.
- Run `npx shadcn@latest add ... --diff` to compare an installed component with the
  registry before overwriting local changes.
- Do not write a component by hand if the registry has it.

## Use components

Components are builders: `X.new`, then content setters, then variant and size setters,
then `toHtml`. Break the line after `X.new` and put each `|>` on its own line.

```elm
import UI.Button as Button

Button.new
    |> Button.text "Save"
    |> Button.outline
    |> Button.small
    |> Button.toHtml [ onClick Save ]
```

Required content is enforced by the type, so an incomplete builder does not compile.

## Project

- `src/Main.elm` is the app, started by `src/main.ts`.
- `src/main.css` is the one stylesheet: Tailwind and the theme. `index.html` links it,
  so do not import CSS from TypeScript.
- After changing `src/InteropDefinitions.elm`, regenerate the types with
  `npm run types`. `npm run dev` and `npm run build` do it for you.
- `npm run dev` starts Vite.

## Architecture

All behaviour lives in Elm. TypeScript (`src/main.ts`, `src/idb.ts`) only wraps browser
APIs: IndexedDB reads and writes, localStorage, `window.open`, and the `<in-view>`
IntersectionObserver element. Don't move logic there.

- `GitHub.elm`: the two unauthenticated endpoints (`/users/{login}` and
  `/users/{login}/starred`), and what each response means for the rate limit.
- `Sync.elm`: incremental sync as pure functions. Full syncs walk every page and prune;
  deltas stop at the first star already known. The cursor is saved with every page.
- `Index.elm`: BM25F with prefix matching. `Tokens.elm` splits words and highlights.
- `Main.elm`: the state machine (`Sync`: `Idle | Running | Paused | Stalled`), the
  rate-limit budget, and the views.

Never call the API while the user types; search runs on the local index only. Never fetch
anything that needs a token.

## Developing without spending the rate limit

The unauthenticated limit is 60 requests an hour per IP. `npm run dev:mock` serves
recorded stars from `dev/fixtures` instead (record them with `dev/record.sh <login>`, which
uses the authenticated `gh` CLI). `dev/mock-github.ts` documents the knobs at
`/__github/__control` for simulating rate limits, 429s, new stars and going offline.
