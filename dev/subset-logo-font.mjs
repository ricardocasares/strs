// Cuts Major Mono Display down to the glyphs of the wordmark, "stRs": npm run font:logo
import { readFile, writeFile } from "node:fs/promises";
import subsetFont from "subset-font";

const source = await readFile("node_modules/@fontsource/major-mono-display/files/major-mono-display-latin-400-normal.woff2");
const subset = await subsetFont(source, "stRs", { targetFormat: "woff2" });
await writeFile("src/fonts/major-mono-display-logo.woff2", subset);
console.log(`${source.length} → ${subset.length} bytes`);
