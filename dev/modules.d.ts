// Types for the untyped dev dependencies, covering only what the scripts use.

declare module "opentype.js" {
  export type BoundingBox = { x1: number; y1: number; x2: number; y2: number };

  export interface Path {
    getBoundingBox(): BoundingBox;
    /** SVG path data, its numbers rounded to `decimalPlaces`. */
    toPathData(decimalPlaces: number): string;
  }

  export interface Glyph {
    /** The outline at `fontSize` pixels, with its baseline origin at (x, y). */
    getPath(x: number, y: number, fontSize: number): Path;
  }

  export interface Font {
    charToGlyph(char: string): Glyph;
  }

  /** Reads a TrueType, OpenType or WOFF font. */
  export function parse(buffer: ArrayBuffer): Font;
}

declare module "subset-font" {
  type Format = "sfnt" | "truetype" | "woff" | "woff2";

  /** Keeps only the glyphs `text` needs. */
  export default function subsetFont(
    font: Buffer,
    text: string,
    options?: { targetFormat?: Format },
  ): Promise<Buffer>;
}
