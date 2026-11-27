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

// MOCK_GITHUB=<fixtures dir> serves recorded stars instead of calling api.github.com.
const fixtures = process.env.MOCK_GITHUB;

export default defineConfig({
  plugins: [css(), elm(), spaFallback(), ...(fixtures ? [mockGitHub(fixtures)] : [])],
});
