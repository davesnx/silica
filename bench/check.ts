import { spawnSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { cpus } from "node:os";
import { dirname, resolve } from "node:path";

type Stats = { median: number };
type Browser = {
  keyed?: Array<{ app: string; cpu: number; op: string; stats?: Stats; error?: string }>;
  memory?: Array<{ app: string; create1kBytes: Stats }>;
  filter?: Array<{ cpu: number; narrow: Stats; reset: Stats }>;
  loads?: Array<{ page: string; cpu: number; fcp: Stats; ready: Stats }>;
};
type Budgets = { max: Record<string, number>; min: Record<string, number> };

const [rootArg, budgetsArg] = process.argv.slice(2);
if (!rootArg || !budgetsArg) throw new Error("usage: check.ts SOURCE_ROOT budgets.json");
const root = resolve(rootArg);
const budgets = JSON.parse(readFileSync(budgetsArg, "utf8")) as Budgets;
const build = (p: string) => resolve(root, "_build/default", p);
const out = resolve(root, process.env["SILICA_BENCH_OUT"] ?? "bench/results/latest.md");

const run = (command: string, args: string[], env: Record<string, string> = {}): string => {
  const r = spawnSync(command, args, { cwd: root, encoding: "utf8", env: { ...process.env, ...env }, maxBuffer: 1 << 26 });
  if (r.status !== 0) throw new Error(`${command} ${args.join(" ")} failed (${r.status}):\n${r.stderr}`);
  return r.stdout;
};

const measured: Record<string, number> = {};
const lines: string[] = [];

const server = run(build("bench/server/bench.exe"), ["-n", "300"]);
for (const m of server.matchAll(/^\| (landing|doc) \| \d+ \| ([\d.]+) \| ([\d.]+) \|/gm))
  measured[`server render ${m[1]} median ms`] = Number(m[2]);

const trace = run(build("bench/trace/trace_bench.exe"), []);
for (const m of trace.matchAll(/^\| trace recorder (step|press) overhead \| (-?[\d.]+) % \|/gm))
  measured[`trace recorder ${m[1]} overhead %`] = Number(m[2]);

const wrk = run("bash", [resolve(root, "bench/server/wrk.sh")], { DURATION: process.env["SILICA_BENCH_WRK"] ?? "5s" });
for (const m of wrk.matchAll(/^\| (landing|doc \(\w+\)|health) \| ([\d.]+) \|/gm))
  measured[`server ${m[1]} requests/s`] = Number(m[2]);

const json = resolve(root, "bench/browser/results/bench.json");
run(
  "node",
  [
    "--experimental-strip-types",
    resolve(root, "bench/browser/run.ts"),
    "--apps", "silica,vanilla",
    "--suites", "keyed,memory,filter,load",
    "--cpu", "1,4",
    "--runs", process.env["SILICA_BENCH_RUNS"] ?? "5",
    "--warmup", "2",
    "--load-runs", "3",
    "--label", "dune build @bench",
    "--out", json,
  ],
  { TMPDIR: "/tmp" },
);
const browser = JSON.parse(readFileSync(json, "utf8")) as Browser;
for (const k of browser.keyed ?? [])
  if (k.stats) measured[`keyed ${k.app} ${k.op} ${k.cpu}x ms`] = k.stats.median;
  else measured[`keyed ${k.app} ${k.op} ${k.cpu}x failed`] = 1;
for (const m of browser.memory ?? []) measured[`heap ${m.app} after create 1000 MB`] = m.create1kBytes.median / 1048576;
for (const f of browser.filter ?? []) {
  measured[`filter keystroke ${f.cpu}x ms`] = f.narrow.median;
  measured[`filter clear ${f.cpu}x ms`] = f.reset.median;
}
for (const l of browser.loads ?? []) {
  measured[`load ${l.page} ${l.cpu}x first paint ms`] = l.fcp.median;
  measured[`load ${l.page} ${l.cpu}x ready ms`] = l.ready.median;
}

const failures: string[] = [];
lines.push(
  `# Silica bench, ${new Date().toISOString()}`,
  "",
  `Machine: ${cpus().length} CPUs, ${cpus()[0]?.model ?? "unknown CPU"}. Browser numbers: medians from bench/browser/run.ts (method in ${json}); server: bench/server/bench.exe -n 300 and bench/server/wrk.sh.`,
  "",
  "| measure | value | budget | ok |",
  "|---|---|---|---|",
);
for (const [name, value] of Object.entries(measured)) {
  const max = budgets.max[name];
  const min = budgets.min[name];
  const ok = (max === undefined || value <= max) && (min === undefined || value >= min) && !name.endsWith("failed");
  if (!ok) failures.push(`${name}: ${value.toFixed(2)} (budget ${max !== undefined ? "at most " + max : "at least " + min})`);
  const budget = max !== undefined ? `<= ${max}` : min !== undefined ? `>= ${min}` : "";
  lines.push(`| ${name} | ${value.toFixed(2)} | ${budget} | ${ok ? "yes" : "NO"} |`);
}
for (const name of [...Object.keys(budgets.max), ...Object.keys(budgets.min)])
  if (!(name in measured)) failures.push(`${name}: budget for a measure that did not run`);
lines.push("", failures.length === 0 ? "All budgets hold." : "Over budget:\n\n" + failures.map((f) => "- " + f).join("\n"), "");
mkdirSync(dirname(out), { recursive: true });
writeFileSync(out, lines.join("\n"));
console.log(lines.join("\n"));
console.log(`written to ${out}`);
if (failures.length > 0) process.exit(1);
