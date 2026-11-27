// Cuts Major Mono Display down to the glyphs of the wordmark, "stRs": bun run font:logo
import { join } from "node:path";
import subsetFont from "subset-font";

const root = join(import.meta.dir, "..");
const source = join(root, "node_modules/@fontsource/major-mono-display/files/major-mono-display-latin-400-normal.woff2");
const target = join(root, "src/fonts/major-mono-display-logo.woff2");

const font = Buffer.from(await Bun.file(source).arrayBuffer());
const subset = await subsetFont(font, "stRs", { targetFormat: "woff2" });
await Bun.write(target, subset);
console.log(`${font.length} → ${subset.length} bytes`);
