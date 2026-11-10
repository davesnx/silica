import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { brotliCompressSync, constants, gzipSync } from "node:zlib";

const compressible = /\.(js|mjs|css|svg|json|map|txt|html)$/;

for (const dir of process.argv.slice(2)) {
  for (const name of readdirSync(dir)) {
    if (!compressible.test(name)) continue;
    const path = join(dir, name);
    const data = readFileSync(path);
    const variants: Array<[string, Buffer]> = [
      [".gz", gzipSync(data, { level: 9 })],
      [
        ".br",
        brotliCompressSync(data, {
          params: {
            [constants.BROTLI_PARAM_QUALITY]: constants.BROTLI_MAX_QUALITY,
            [constants.BROTLI_PARAM_SIZE_HINT]: data.length,
          },
        }),
      ],
    ];
    for (const [extension, compressed] of variants) {
      if (compressed.length < data.length) writeFileSync(path + extension, compressed);
    }
  }
}
