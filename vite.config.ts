import elm from "vite-plugin-elm";
import css from "@tailwindcss/vite";
import { copyFile } from "node:fs/promises";
import { join } from "node:path";
import { defineConfig, type Plugin } from "vite";
import { mockGitHub } from "./dev/mock-github.ts";

// GitHub Pages has no rewrites: every /strs/<login> is a 404 that serves the app.
const spaFallback = (): Plugin => ({
  name: "spa-fallback",
  apply: "build",
  async writeBundle({ dir }) {
    if (dir) await copyFile(join(dir, "index.html"), join(dir, "404.html"));
  },
});

// Elm's debugger is on in development. It keeps every model, and merging its view of a search
// index with thousands of entries can overflow the stack: ELM_DEBUG=0 turns it off.
const debug = process.env.ELM_DEBUG !== "0";

// MOCK_GITHUB=<fixtures dir> serves recorded stars instead of calling api.github.com.
const fixtures = process.env.MOCK_GITHUB;

export default defineConfig({
  base: "/strs/",
  plugins: [css(), elm({ debug }), spaFallback(), ...(fixtures ? [mockGitHub(fixtures)] : [])],
  define: fixtures ? { "import.meta.env.VITE_GITHUB_API": JSON.stringify("/__github") } : {},
});
