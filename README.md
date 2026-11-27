# stRs

Search anyone's GitHub stars as you type.

**[ricardocasares.github.io/strs](https://ricardocasares.github.io/strs/)** shows my stars.
Add a login to the path, as in `/strs/<login>`, to see anyone else's.

- **Instant search.** Every star is indexed in your browser, in IndexedDB, and ranked with
  BM25F and prefix matching. Typing never calls the API.
- **Filters.** Filter by topic, language, owner and stargazers, active or archived, and
  source or fork.
- **Sorting.** Sort by relevance, date starred, stargazers or last updated, in either
  direction.
- **Shareable views.** The query, filters and sort are kept in the URL.
- **No token.** Uses GitHub's public API, which allows 60 requests an hour. The first sync
  fetches 100 stars per request. After that, a sync stops at the first star it already has.
  If the limit runs out, syncing pauses and resumes once GitHub allows more requests.

Press `/` or `⌘K` to search.

## Develop

Needs [Bun](https://bun.sh).

```sh
bun install
bun run dev
```

`bun run dev:mock` serves recorded stars instead of calling GitHub, so development
doesn't spend the rate limit. Record stars with `dev/record.sh <login>`, which needs the
`gh` CLI.

`bun run build` writes the site to `dist/`. Pushing to `main` deploys it to GitHub Pages.

Built with Elm and [elmcn](https://github.com/ricardocasares/elmcn) on Vite and Tailwind.
[AGENTS.md](AGENTS.md) covers the architecture and conventions.
