// Draws the favicon from the R of Major Mono Display, the triangle R of the wordmark:
// bun run favicon. Browsers don't load web fonts in favicons, so the outline is baked in.
import { join, relative } from "node:path";
import { parse } from "opentype.js";

const root = join(import.meta.dir, "..");
const source = join(root, "node_modules/@fontsource/major-mono-display/files/major-mono-display-latin-400-normal.woff");
const target = join(root, "public/favicon.svg");

const glyph = parse(await Bun.file(source).arrayBuffer()).charToGlyph("R");

// Fit the glyph in a 32 × 32 box, with a little room for the stroke that thickens it.
const size = 32;
const pad = 3;
const { x1, y1, x2, y2 } = glyph.getPath(0, 0, 1000).getBoundingBox();
const scale = Math.min((size - 2 * pad) / (x2 - x1), (size - 2 * pad) / (y2 - y1));
const fontSize = 1000 * scale;
const box = glyph.getPath(0, 0, fontSize).getBoundingBox();
const dx = (size - (x2 - x1) * scale) / 2 - box.x1;
const dy = (size - (y2 - y1) * scale) / 2 - box.y1;
const d = glyph.getPath(dx, dy, fontSize).toPathData(2);

await Bun.write(
  target,
  `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${size} ${size}">
  <style>
    path { fill: #0a0a0a; stroke: #0a0a0a; }
    @media (prefers-color-scheme: dark) { path { fill: #fafafa; stroke: #fafafa; } }
  </style>
  <path d="${d}" stroke-width="0.9" stroke-linejoin="miter" />
</svg>
`,
);
console.log(relative(root, target));
