import { spawnSync } from "node:child_process";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { brotliCompressSync, constants } from "node:zlib";
import { readFileSync } from "node:fs";

const brotli = (file: string) =>
  brotliCompressSync(readFileSync(join("fixture/dist", file)), {
    params: { [constants.BROTLI_PARAM_QUALITY]: constants.BROTLI_MAX_QUALITY },
  }).length;
const first = brotli("main-AAAAAAAA.js") + brotli("shared-BBBBBBBB.js");
const lazy = brotli("page-CCCCCCCC.js");
const css = brotli("app-DDDDDDDD.css");
const dir = mkdtempSync(join(tmpdir(), "budget-"));

const check = (name: string, budgets: object, minify: string, expected: number, output?: RegExp) => {
  const file = join(dir, name + ".json");
  writeFileSync(file, JSON.stringify(budgets));
  const r = spawnSync("node", ["budget.ts", file, minify, "fixture", "fixture/meta.json", "fixture/dist"], { encoding: "utf8" });
  const text = r.stdout + r.stderr;
  if (r.status !== expected) throw new Error(`${name}: exit ${r.status}, expected ${expected}\n${text}`);
  if (output && !output.test(text)) throw new Error(`${name}: output does not match ${output}\n${text}`);
};

check("at the budget", { fixture: { first_js: first, lazy_js: lazy, css } }, "true", 0, new RegExp(`\\| fixture \\| first_js \\| \\d+ \\| \\d+ \\| ${first} \\|`));
check("one byte over", { fixture: { first_js: first - 1, lazy_js: lazy, css } }, "true", 1, /fixture first_js: \d+ bytes brotli, budget/);
check("lazy over", { fixture: { first_js: first, lazy_js: lazy - 1, css } }, "true", 1, /fixture lazy_js/);
check("a part with no budget", { fixture: { first_js: first, css } }, "true", 1, /lazy_js: no budget/);
check("a budget with no bundle", { fixture: { first_js: first, lazy_js: lazy, css }, other: { first_js: 1 } }, "true", 1, /other first_js: budget for a bundle/);
check("debug build is skipped", { fixture: { first_js: 1, lazy_js: 1, css: 1 } }, "false", 0, /skipped/);
console.log("budget self test: 6 cases pass");
