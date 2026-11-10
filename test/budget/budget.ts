import { readFileSync } from "node:fs";
import { basename, join } from "node:path";
import { brotliCompressSync, constants, gzipSync } from "node:zlib";

type Output = { entryPoint?: string; imports?: Array<{ path: string; kind: string }> };
type Metafile = { outputs: Record<string, Output> };
type Sizes = { raw: number; gzip: number; brotli: number };
type Limits = Record<string, number>;

const [budgetsPath, minify, ...apps] = process.argv.slice(2);
if (!budgetsPath || !minify) throw new Error("usage: budget.ts budgets.json MINIFY (name meta.json dist | name file.js)...");
if (minify !== "true") {
  console.log(`budget: skipped, the bundles are not minified (SILICA_MINIFY=${minify})`);
  process.exit(0);
}
const budgets = JSON.parse(readFileSync(budgetsPath, "utf8")) as Record<string, Limits>;

const sizes = (files: string[]): Sizes => {
  const total: Sizes = { raw: 0, gzip: 0, brotli: 0 };
  for (const file of files) {
    const data = readFileSync(file);
    total.raw += data.length;
    total.gzip += gzipSync(data, { level: 9 }).length;
    total.brotli += brotliCompressSync(data, {
      params: { [constants.BROTLI_PARAM_QUALITY]: constants.BROTLI_MAX_QUALITY },
    }).length;
  }
  return total;
};

const measured: Record<string, Record<string, Sizes>> = {};

const bundle = (meta: Metafile, dist: string): Record<string, Sizes> => {
  const outputs = meta.outputs;
  const entry = Object.keys(outputs).find((path) => path.endsWith(".js") && outputs[path]?.entryPoint);
  if (!entry) throw new Error("no JavaScript entry in the metafile");
  const first = new Set<string>();
  const visit = (path: string) => {
    if (first.has(path)) return;
    first.add(path);
    for (const i of outputs[path]?.imports ?? []) if (i.kind === "import-statement") visit(i.path);
  };
  visit(entry);
  const file = (path: string) => join(dist, basename(path));
  const js = Object.keys(outputs).filter((path) => path.endsWith(".js"));
  return {
    first_js: sizes([...first].map(file)),
    lazy_js: sizes(js.filter((path) => !first.has(path)).map(file)),
    css: sizes(Object.keys(outputs).filter((path) => path.endsWith(".css")).map(file)),
  };
};

for (let i = 0; i < apps.length; ) {
  const name = apps[i] as string;
  const next = apps[i + 1] as string;
  if (next.endsWith(".json")) {
    measured[name] = bundle(JSON.parse(readFileSync(next, "utf8")) as Metafile, apps[i + 2] as string);
    i += 3;
  } else {
    measured[name] = { first_js: sizes([next]) };
    i += 2;
  }
}

const failures: string[] = [];
console.log("| bundle | part | raw | gzip | brotli | brotli budget |");
console.log("|---|---|---|---|---|---|");
for (const [name, parts] of Object.entries(measured)) {
  const limits = budgets[name];
  if (!limits) failures.push(`${name}: no budget`);
  for (const [part, s] of Object.entries(parts)) {
    const limit = limits?.[part];
    console.log(`| ${name} | ${part} | ${s.raw} | ${s.gzip} | ${s.brotli} | ${limit ?? "none"} |`);
    if (limit === undefined) failures.push(`${name} ${part}: no budget`);
    else if (s.brotli > limit) failures.push(`${name} ${part}: ${s.brotli} bytes brotli, budget ${limit}`);
  }
}
for (const [name, limits] of Object.entries(budgets))
  for (const part of Object.keys(limits))
    if (!measured[name]?.[part]) failures.push(`${name} ${part}: budget for a bundle that was not measured`);
if (failures.length > 0) {
  console.error("budget: over or missing:\n  " + failures.join("\n  "));
  process.exit(1);
}
