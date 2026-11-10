import { spawnSync } from "node:child_process";
import { readdirSync, statSync } from "node:fs";
import { join } from "node:path";

const cli = new URL("./node_modules/@informalsystems/quint/dist/src/cli.js", import.meta.url).pathname;

function quint(args: string[]): { code: number; out: string } {
  const result = spawnSync(process.execPath, [cli, ...args], { encoding: "utf8" });
  return { code: result.status ?? -1, out: `${result.stdout}${result.stderr}` };
}

const expand = (paths: string[]) =>
  paths.flatMap((path) =>
    statSync(path).isDirectory()
      ? readdirSync(path).filter((name) => name.endsWith(".qnt")).sort().map((name) => join(path, name))
      : [path],
  );

const split = process.argv.indexOf("--fail");
const passing = expand(process.argv.slice(2, split === -1 ? undefined : split));
const failing = expand(split === -1 ? [] : process.argv.slice(split + 1));
const errors: string[] = [];
let typechecked = 0;
let clean = 0;
let caught = 0;

for (const file of [...passing, ...failing]) {
  const typecheck = quint(["typecheck", file]);
  if (typecheck.code === 0) typechecked += 1;
  else errors.push(`${file}: typecheck failed\n${typecheck.out}`);
}

const runArgs = (file: string) => [
  "run", file, "--invariant=inv", "--max-steps=20", "--max-samples=500", "--seed=1", "--backend=typescript",
];

for (const file of passing) {
  const run = quint(runArgs(file));
  if (run.code === 0 && run.out.includes("[ok] No violation found")) clean += 1;
  else errors.push(`${file}: quint run found a violation or failed\n${run.out}`);
}

for (const file of failing) {
  const run = quint(runArgs(file));
  if (run.code !== 0 && run.out.includes("[violation]")) caught += 1;
  else errors.push(`${file}: quint run found no violation in a seeded bug\n${run.out}`);
}

console.log(
  `quint: ${typechecked} of ${passing.length + failing.length} modules typechecked, ` +
    `${clean} of ${passing.length} runs with no violation, ${caught} of ${failing.length} seeded violations found`,
);
if (errors.length > 0) {
  console.error(errors.join("\n"));
  process.exit(1);
}
