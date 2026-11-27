// Draws the favicon from the R of Major Mono Display, the triangle R of the wordmark:
// npm run favicon. Browsers don't load web fonts in favicons, so the outline is baked in.
import { readFile, writeFile } from "node:fs/promises";
import opentype from "opentype.js";

const file = await readFile("node_modules/@fontsource/major-mono-display/files/major-mono-display-latin-400-normal.woff");
const font = opentype.parse(file.buffer.slice(file.byteOffset, file.byteOffset + file.byteLength));
const glyph = font.charToGlyph("R");
const { x1, y1, x2, y2 } = glyph.getPath(0, 0, 1000).getBoundingBox();

// Fit the glyph in a 32 × 32 box with a little room for the stroke that thickens it.
const pad = 3;
const scale = Math.min((32 - 2 * pad) / (x2 - x1), (32 - 2 * pad) / (y2 - y1));
const width = (x2 - x1) * scale;
const height = (y2 - y1) * scale;
const path = glyph.getPath(0, 0, 1000 * scale);
const box = path.getBoundingBox();
const dx = (32 - width) / 2 - box.x1;
const dy = (32 - height) / 2 - box.y1;
const d = glyph.getPath(dx, dy, 1000 * scale).toPathData(2);

await writeFile(
  "public/favicon.svg",
  `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32">
  <style>
    path { fill: #0a0a0a; stroke: #0a0a0a; }
    @media (prefers-color-scheme: dark) { path { fill: #fafafa; stroke: #fafafa; } }
  </style>
  <path d="${d}" stroke-width="0.9" stroke-linejoin="miter" />
</svg>
`,
);
console.log("public/favicon.svg");
