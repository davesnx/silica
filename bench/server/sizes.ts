import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { brotliCompressSync, constants, gzipSync } from "node:zlib";

console.log("| file | raw | gzip -9 | brotli 11 |");
console.log("|---|---|---|---|");
for (const dir of process.argv.slice(2)) {
  for (const name of readdirSync(dir).filter((n) => /\.(js|css)$/.test(n)).sort()) {
    const data = readFileSync(join(dir, name));
    const gzip = gzipSync(data, { level: 9 }).length;
    const brotli = brotliCompressSync(data, {
      params: { [constants.BROTLI_PARAM_QUALITY]: constants.BROTLI_MAX_QUALITY },
    }).length;
    console.log(`| ${join(dir, name)} | ${data.length} | ${gzip} | ${brotli} |`);
  }
}
