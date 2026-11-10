import { spawn, type ChildProcess } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { createServer, type Server } from "node:http";
import { createServer as createNetServer } from "node:net";
import { cpus } from "node:os";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";
import { brotliCompressSync, gzipSync, constants as zlib } from "node:zlib";
import { chromium, type Browser, type BrowserContext, type Page } from "playwright-core";

const here = dirname(fileURLToPath(import.meta.url));
const root = resolve(here, "../..");
const build = (p: string): string => resolve(root, "_build/default", p);

const appNames = ["silica", "vanilla", "react", "solid"] as const;
type AppName = (typeof appNames)[number];
const appDirs: Record<AppName, string> = {
  silica: build("bench/keyed"),
  vanilla: resolve(root, "bench/compare/dist/vanilla"),
  react: resolve(root, "bench/compare/dist/react"),
  solid: resolve(root, "bench/compare/dist/solid"),
};
const filterDir = build("bench/filter");

type Summary = {
  count: number;
  firstId: string;
  row2Id: string;
  row999Id: string;
  firstLabel: string;
  danger: string[];
};

type Op = {
  id: string;
  label: string;
  setup: string;
  act: string;
  check: (pre: Summary, post: Summary) => string | null;
};

const expectCount =
  (n: number) =>
  (_: Summary, post: Summary): string | null =>
    post.count === n ? null : `expected ${n} rows, got ${post.count}`;

const ops: Op[] = [
  { id: "create1k", label: "create 1,000 rows", setup: "#clear", act: "#run", check: expectCount(1000) },
  {
    id: "replace1k",
    label: "replace all 1,000 rows",
    setup: "#run",
    act: "#run",
    check: (pre, post) =>
      post.count === 1000 && post.firstId !== pre.firstId ? null : `rows not replaced (${pre.firstId} -> ${post.firstId})`,
  },
  {
    id: "update",
    label: "partial update (every 10th of 1,000)",
    setup: "#run",
    act: "#update",
    check: (_, post) => (post.firstLabel.endsWith(" !!!") ? null : `first label not updated: ${post.firstLabel}`),
  },
  {
    id: "select",
    label: "select row",
    setup: "#run",
    act: "tbody tr:nth-child(2) a.lbl",
    check: (pre, post) =>
      post.danger.length === 1 && post.danger[0] === pre.row2Id ? null : `selected ${post.danger.join(",")}, wanted ${pre.row2Id}`,
  },
  {
    id: "swap",
    label: "swap rows 2 and 999",
    setup: "#run",
    act: "#swaprows",
    check: (pre, post) =>
      post.row2Id === pre.row999Id && post.row999Id === pre.row2Id ? null : "rows 2 and 999 not swapped",
  },
  { id: "remove", label: "remove row", setup: "#run", act: "tbody tr:nth-child(4) a.remove", check: expectCount(999) },
  { id: "create10k", label: "create 10,000 rows", setup: "#clear", act: "#runlots", check: expectCount(10000) },
  { id: "append1k", label: "append 1,000 to 1,000 rows", setup: "#run", act: "#add", check: expectCount(2000) },
  { id: "clear", label: "clear 1,000 rows", setup: "#run", act: "#clear", check: expectCount(0) },
];

const suiteNames = ["bundle", "keyed", "memory", "filter", "load", "live"] as const;
type SuiteName = (typeof suiteNames)[number];

const list = <T extends string>(raw: string, allowed: readonly T[], what: string): T[] => {
  const items = raw.split(",").map((s) => s.trim()).filter((s) => s !== "");
  for (const item of items) {
    if (!(allowed as readonly string[]).includes(item)) throw new Error(`unknown ${what} "${item}"; known: ${allowed.join(", ")}`);
  }
  return items as T[];
};

const positive = (raw: string, what: string): number => {
  const n = Number(raw);
  if (!Number.isFinite(n) || n <= 0) throw new Error(`--${what} must be a positive number, got "${raw}"`);
  return n;
};

const { values: args } = parseArgs({
  options: {
    apps: { type: "string", default: appNames.join(",") },
    ops: { type: "string", default: ops.map((o) => o.id).join(",") },
    suites: { type: "string", default: suiteNames.join(",") },
    cpu: { type: "string", default: "1,4" },
    runs: { type: "string", default: "10" },
    warmup: { type: "string", default: "3" },
    "load-runs": { type: "string", default: "5" },
    "docs-page": { type: "string" },
    label: { type: "string", default: "" },
    out: { type: "string", default: resolve(here, "results/latest.json") },
    help: { type: "boolean", default: false },
  },
});

if (args.help) {
  console.log(`node --experimental-strip-types bench/browser/run.ts [options]
  --apps     ${appNames.join(",")}
  --ops      ${ops.map((o) => o.id).join(",")}
  --suites   ${suiteNames.join(",")}
  --cpu      CPU throttle rates, default 1,4
  --runs     measured runs per keyed operation and filter, default 10
  --warmup   untimed runs in the same page before each measured run, default 3
  --load-runs  page loads per page and rate, default 5
  --docs-page  docs page name for the load suite, default: the largest one
  --label    free text stored in the results
  --out      JSON output; the Markdown table goes next to it with .md`);
  process.exit(0);
}

const apps = list(args.apps, appNames, "app");
const selectedOps = list(args.ops, ops.map((o) => o.id), "op").map((id) => ops.find((o) => o.id === id)!);
const suites = list(args.suites, suiteNames, "suite");
const rates = args.cpu.split(",").map((r) => positive(r, "cpu"));
const runs = positive(args.runs, "runs");
const warmup = Number(args.warmup);
if (!Number.isInteger(warmup) || warmup < 0) throw new Error(`--warmup must be a whole number >= 0`);
const loadRuns = positive(args["load-runs"], "load-runs");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";

type Stats = { n: number; median: number; p25: number; p75: number; min: number; max: number; mean: number };

const quantile = (sorted: number[], q: number): number => {
  const pos = (sorted.length - 1) * q;
  const lo = Math.floor(pos);
  const hi = Math.ceil(pos);
  return sorted[lo]! + (sorted[hi]! - sorted[lo]!) * (pos - lo);
};

const stats = (samples: number[]): Stats => {
  if (samples.length === 0) throw new Error("no samples");
  const s = [...samples].sort((a, b) => a - b);
  return {
    n: s.length,
    median: quantile(s, 0.5),
    p25: quantile(s, 0.25),
    p75: quantile(s, 0.75),
    min: s[0]!,
    max: s[s.length - 1]!,
    mean: s.reduce((a, b) => a + b, 0) / s.length,
  };
};

const types: Record<string, string> = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
};

const serveStatic = (mounts: Record<string, string>): Promise<{ server: Server; base: string }> =>
  new Promise((ok) => {
    const server = createServer((req, res) => {
      const path = new URL(req.url ?? "/", "http://x").pathname;
      const [, mount, ...rest] = path.split("/");
      const dir = mount === undefined ? undefined : mounts[mount];
      const file = dir === undefined ? undefined : resolve(dir, rest.join("/") || "index.html");
      if (dir === undefined || file === undefined || !file.startsWith(dir + "/") || !existsSync(file) || !statSync(file).isFile()) {
        res.writeHead(404).end();
        return;
      }
      res.writeHead(200, { "content-type": types[extname(file)] ?? "application/octet-stream", "cache-control": "no-store" });
      res.end(readFileSync(file));
    });
    server.listen(0, "127.0.0.1", () => {
      const addr = server.address();
      const port = typeof addr === "object" && addr ? addr.port : 0;
      ok({ server, base: `http://127.0.0.1:${port}` });
    });
  });

const freePort = (): Promise<number> =>
  new Promise((ok, fail) => {
    const s = createNetServer();
    s.once("error", fail);
    s.listen(0, "127.0.0.1", () => {
      const addr = s.address();
      const p = typeof addr === "object" && addr ? addr.port : 0;
      s.close(() => ok(p));
    });
  });

const startServer = async (exe: string, extra: string[], clientDir: string): Promise<{ child: ChildProcess; base: string }> => {
  const dist = build(`${clientDir}/dist`);
  const metafile = build(`${clientDir}/meta.json`);
  for (const f of [build(exe), dist, metafile]) if (!existsSync(f)) throw new Error(`missing ${f}; run dune build @all`);
  const port = await freePort();
  let log = "";
  const child = spawn(build(exe), ["--port", String(port), "--static", dist, "--metafile", metafile, ...extra], { cwd: root });
  child.stdout?.on("data", (d: Buffer) => (log += d.toString()));
  child.stderr?.on("data", (d: Buffer) => (log += d.toString()));
  const base = `http://127.0.0.1:${port}`;
  for (let i = 0; i < 200; i++) {
    try {
      await fetch(base + "/");
      return { child, base };
    } catch {
      await new Promise((r) => setTimeout(r, 100));
    }
  }
  child.kill("SIGTERM");
  throw new Error(`${exe} did not start\n${log}`);
};

const setThrottle = async (page: Page, rate: number): Promise<void> => {
  const cdp = await page.context().newCDPSession(page);
  await cdp.send("Emulation.setCPUThrottlingRate", { rate });
};

const calibrate = (page: Page): Promise<number> =>
  page.evaluate(() => {
    const t0 = performance.now();
    let x = 0;
    for (let i = 0; i < 5_000_000; i++) x = (x + i * 7) % 1_000_003;
    return x >= 0 ? performance.now() - t0 : -1;
  });

const timedClick = (page: Page, selector: string): Promise<number> =>
  page.evaluate(
    (sel) =>
      new Promise<number>((done, fail) => {
        const el = document.querySelector<HTMLElement>(sel);
        if (!el) return fail(new Error(`no element ${sel}`));
        const t0 = performance.now();
        el.click();
        requestAnimationFrame(() => {
          const ch = new MessageChannel();
          ch.port1.onmessage = () => done(performance.now() - t0);
          ch.port2.postMessage(null);
        });
      }),
    selector,
  );

const summary = (page: Page): Promise<Summary> =>
  page.evaluate(() => {
    const rows = Array.from(document.querySelectorAll("tbody tr"));
    const id = (i: number): string => rows[i]?.querySelector("td")?.textContent ?? "";
    return {
      count: rows.length,
      firstId: id(0),
      row2Id: id(1),
      row999Id: id(998),
      firstLabel: rows[0]?.querySelector("a.lbl")?.textContent ?? "",
      danger: rows.filter((r) => r.classList.contains("danger")).map((r) => r.querySelector("td")?.textContent ?? ""),
    };
  });

const openApp = async (context: BrowserContext, url: string, ready: string): Promise<Page> => {
  const page = await context.newPage();
  const errors: string[] = [];
  page.on("pageerror", (e) => errors.push(e.message));
  await page.goto(url);
  await page.waitForSelector(ready, { timeout: 30_000 });
  if (errors.length > 0) throw new Error(`${url}: ${errors.join("; ")}`);
  return page;
};

type KeyedResult = { app: AppName; cpu: number; op: string; samples: number[]; stats?: Stats; error?: string };

const runKeyed = async (browser: Browser, base: string): Promise<KeyedResult[]> => {
  const results: KeyedResult[] = [];
  for (const cpu of rates) {
    for (const op of selectedOps) {
      for (const app of apps) {
        const samples: number[] = [];
        let error: string | undefined;
        for (let r = 0; r < runs && error === undefined; r++) {
          const context = await browser.newContext();
          try {
            const page = await openApp(context, `${base}/${app}/`, "#run");
            await setThrottle(page, cpu);
            for (let w = 0; w < warmup; w++) {
              await timedClick(page, op.setup);
              await timedClick(page, op.act);
            }
            await timedClick(page, op.setup);
            const pre = await summary(page);
            const ms = await timedClick(page, op.act);
            const failure = op.check(pre, await summary(page));
            if (failure !== null) throw new Error(failure);
            samples.push(ms);
          } catch (e) {
            const shown = await context.pages()[0]?.evaluate(() => document.body.innerText.slice(0, 200)).catch(() => "");
            error = `run ${r + 1}: ${e instanceof Error ? e.message.split("\n")[0] : String(e)}${shown ? ` | page: ${shown.replace(/\s+/g, " ")}` : ""}`;
          } finally {
            await context.close();
          }
        }
        if (error === undefined) {
          const s = stats(samples);
          results.push({ app, cpu, op: op.id, samples, stats: s });
          console.log(`keyed ${cpu}x ${op.id.padEnd(9)} ${app.padEnd(7)} median ${s.median.toFixed(1)} ms`);
        } else {
          results.push({ app, cpu, op: op.id, samples, error });
          console.log(`keyed ${cpu}x ${op.id.padEnd(9)} ${app.padEnd(7)} FAILED ${error}`);
        }
      }
    }
  }
  return results;
};

type MemoryResult = { app: AppName; readyBytes: Stats; create1kBytes: Stats };

const heapUsed = async (page: Page): Promise<number> => {
  const cdp = await page.context().newCDPSession(page);
  await cdp.send("HeapProfiler.collectGarbage");
  const { usedSize } = await cdp.send("Runtime.getHeapUsage");
  return usedSize;
};

const runMemory = async (browser: Browser, base: string): Promise<MemoryResult[]> => {
  const results: MemoryResult[] = [];
  const n = Math.min(runs, 5);
  for (const app of apps) {
    const ready: number[] = [];
    const created: number[] = [];
    for (let r = 0; r < n; r++) {
      const context = await browser.newContext();
      const page = await openApp(context, `${base}/${app}/`, "#run");
      ready.push(await heapUsed(page));
      await timedClick(page, "#run");
      const { count } = await summary(page);
      if (count !== 1000) throw new Error(`${app} memory: ${count} rows`);
      created.push(await heapUsed(page));
      await context.close();
    }
    const result = { app, readyBytes: stats(ready), create1kBytes: stats(created) };
    results.push(result);
    console.log(`memory ${app.padEnd(7)} ready ${mb(result.readyBytes.median)} after create 1k ${mb(result.create1kBytes.median)}`);
  }
  return results;
};

type FilterResult = { cpu: number; narrow: Stats; widen: Stats; reset: Stats };

const filterQueries = ["b", "bl", "bla", "blac", "black", "blac", "bla", "bl", "b", ""];

const typeQuery = (page: Page, q: string): Promise<number> =>
  page.evaluate(
    (query) =>
      new Promise<number>((done, fail) => {
        const input = document.querySelector<HTMLInputElement>("#filter");
        if (!input) return fail(new Error("no #filter"));
        const t0 = performance.now();
        input.value = query;
        input.dispatchEvent(new Event("input", { bubbles: true }));
        requestAnimationFrame(() => {
          const ch = new MessageChannel();
          ch.port1.onmessage = () => done(performance.now() - t0);
          ch.port2.postMessage(null);
        });
      }),
    q,
  );

const runFilter = async (browser: Browser, base: string): Promise<FilterResult[]> => {
  const results: FilterResult[] = [];
  for (const cpu of rates) {
    const narrow: number[] = [];
    const widen: number[] = [];
    const reset: number[] = [];
    for (let r = 0; r < runs; r++) {
      const context = await browser.newContext();
      const page = await openApp(context, `${base}/filter/`, "#filter");
      const labels = await page.$$eval("#rows li", (lis) => lis.map((li) => li.textContent ?? ""));
      if (labels.length !== 1000) throw new Error(`filter page has ${labels.length} rows`);
      await setThrottle(page, cpu);
      for (let w = 0; w <= warmup; w++) {
        let prev = "";
        for (const q of filterQueries) {
          const ms = await typeQuery(page, q);
          const shown = await page.$$eval("#rows li", (lis) => lis.length);
          const expected = labels.filter((l) => l.includes(q)).length;
          if (shown !== expected) throw new Error(`filter "${q}": ${shown} rows, expected ${expected}`);
          if (w === warmup) (q === "" ? reset : q.length > prev.length ? narrow : widen).push(ms);
          prev = q;
        }
      }
      await context.close();
    }
    const result = { cpu, narrow: stats(narrow), widen: stats(widen), reset: stats(reset) };
    results.push(result);
    console.log(`filter ${cpu}x narrow ${result.narrow.median.toFixed(1)} widen ${result.widen.median.toFixed(1)} reset ${result.reset.median.toFixed(1)} ms`);
  }
  return results;
};

type BundleResult = { name: string; files: string[]; raw: number; gzip: number; brotli: number };

const bundle = (name: string, dir: string): BundleResult => {
  if (!existsSync(dir)) throw new Error(`missing ${dir}; build it first`);
  const files = readdirSync(dir).filter((f) => [".js", ".mjs", ".css"].includes(extname(f))).sort();
  let raw = 0;
  let gzip = 0;
  let brotli = 0;
  for (const f of files) {
    const data = readFileSync(join(dir, f));
    raw += data.length;
    gzip += gzipSync(data, { level: 9 }).length;
    brotli += brotliCompressSync(data, { params: { [zlib.BROTLI_PARAM_QUALITY]: 11 } }).length;
  }
  return { name, files, raw, gzip, brotli };
};

const runBundles = (): BundleResult[] => {
  const dirs: [string, string][] = [
    ...apps.map((a): [string, string] => [`keyed ${a}`, appDirs[a]]),
    ...(apps.includes("silica") ? [["filter silica", filterDir] as [string, string]] : []),
    ["site client", build("site/client/dist")],
    ["linear client", build("linear/client/dist")],
  ];
  return dirs.map(([name, dir]) => bundle(name, dir));
};

type LoadResult = { page: string; url: string; cpu: number; fcp: Stats; ready: Stats; cpuCheckMs: number };

const readyProbe = (): void => {
  const w = window as unknown as { __silicaReady?: number };
  new MutationObserver((records, observer) => {
    for (const r of records) {
      if (r.target instanceof Element && r.target.hasAttribute("data-silica-ready")) {
        w.__silicaReady = performance.now();
        observer.disconnect();
        return;
      }
    }
  }).observe(document, { attributes: true, subtree: true, attributeFilter: ["data-silica-ready"] });
};

const measureLoad = async (browser: Browser, name: string, url: string, origin: string, cpu: number): Promise<LoadResult> => {
  const fcp: number[] = [];
  const ready: number[] = [];
  let cpuCheckMs = 0;
  for (let r = 0; r < loadRuns; r++) {
    const context = await browser.newContext();
    const page = await context.newPage();
    const errors: string[] = [];
    page.on("pageerror", (e) => errors.push(e.message));
    await page.addInitScript(readyProbe);
    await page.goto(origin + "/robots.txt");
    await setThrottle(page, cpu);
    await page.goto(url);
    await page.waitForSelector("#silica-root[data-silica-ready]", { timeout: 120_000 });
    const t = await page.evaluate(() => ({
      fcp: performance.getEntriesByName("first-contentful-paint")[0]?.startTime ?? -1,
      ready: (window as unknown as { __silicaReady?: number }).__silicaReady ?? -1,
    }));
    if (t.fcp < 0 || t.ready < 0) throw new Error(`${url}: no FCP or ready time (${JSON.stringify(t)})`);
    if (errors.length > 0) throw new Error(`${url}: ${errors.join("; ")}`);
    fcp.push(t.fcp);
    ready.push(t.ready);
    if (r === 0) cpuCheckMs = await calibrate(page);
    await context.close();
  }
  const result = { page: name, url, cpu, fcp: stats(fcp), ready: stats(ready), cpuCheckMs };
  console.log(`load ${cpu}x ${name.padEnd(14)} fcp ${result.fcp.median.toFixed(0)} ready ${result.ready.median.toFixed(0)} ms (cpu check ${cpuCheckMs.toFixed(0)} ms)`);
  return result;
};

const largestDocsPage = async (base: string): Promise<string> => {
  if (args["docs-page"]) return `/docs/${args["docs-page"]}`;
  let best = { path: "", size: -1 };
  for (const f of readdirSync(resolve(root, "docs")).filter((f) => f.endsWith(".mld"))) {
    const path = `/docs/${f.slice(0, -4)}`;
    const res = await fetch(base + path);
    if (!res.ok) continue;
    const size = (await res.text()).length;
    if (size > best.size) best = { path, size };
  }
  if (best.size < 0) throw new Error("no docs page answered 200");
  return best.path;
};

type LiveResult = { issues: number; cpu: number; latency: Stats };

const runLive = async (browser: Browser): Promise<LiveResult[]> => {
  const results: LiveResult[] = [];
  for (const issues of [30, 1000]) {
    const linear = await startServer("linear/server/main.exe", ["--issues", String(issues)], "linear/client");
    try {
      for (const cpu of rates) {
        const open = async (): Promise<Page> => {
          const context = await browser.newContext();
          const page = await context.newPage();
          await page.goto(linear.base + "/");
          await page.waitForSelector("#silica-root[data-silica-ready]", { timeout: 120_000 });
          await setThrottle(page, cpu);
          return page;
        };
        const watcher = await open();
        const writer = await open();
        await new Promise((r) => setTimeout(r, 1000));
        const samples: number[] = [];
        for (let r = 0; r < runs + warmup; r++) {
          const title = `Live probe ${issues} ${cpu} ${r} ${Date.now()}`;
          await watcher.evaluate((t) => {
            const w = window as unknown as { __seen?: number };
            w.__seen = undefined;
            new MutationObserver((_, observer) => {
              if (document.querySelector("#issue-list")?.textContent?.includes(t)) {
                w.__seen = Date.now();
                observer.disconnect();
              }
            }).observe(document.body, { childList: true, subtree: true, characterData: true });
          }, title);
          await writer.locator("#new-issue").click();
          await writer.locator("[role=dialog]").waitFor();
          await writer.getByLabel("Title").fill(title);
          const sent = await writer.evaluate(() => {
            const button = Array.from(document.querySelectorAll("[role=dialog] button")).find((b) => b.textContent?.includes("Create issue"));
            if (!(button instanceof HTMLElement)) throw new Error("no Create issue button");
            const t = Date.now();
            button.click();
            return t;
          });
          await watcher.waitForFunction(() => (window as unknown as { __seen?: number }).__seen !== undefined, null, { timeout: 30_000 });
          const seen = await watcher.evaluate(() => (window as unknown as { __seen: number }).__seen);
          await writer.locator("[role=dialog]").waitFor({ state: "detached" });
          if (r >= warmup) samples.push(seen - sent);
        }
        results.push({ issues, cpu, latency: stats(samples) });
        await watcher.context().close();
        await writer.context().close();
      }
    } finally {
      linear.child.kill("SIGTERM");
    }
  }
  return results;
};

const runLoads = async (browser: Browser): Promise<LoadResult[]> => {
  const site = await startServer("site/server/main.exe", [], "site/client");
  const linear = await startServer("linear/server/main.exe", ["--issues", "1000"], "linear/client");
  try {
    const docs = await largestDocsPage(site.base);
    const pages: [string, string, string][] = [
      ["site /", site.base + "/", site.base],
      [`site ${docs}`, site.base + docs, site.base],
      ["site /components/select (lazy route)", site.base + "/components/select", site.base],
      ["linear / (1000)", linear.base + "/", linear.base],
    ];
    const results: LoadResult[] = [];
    for (const cpu of rates) for (const [name, url, origin] of pages) results.push(await measureLoad(browser, name, url, origin, cpu));
    return results;
  } finally {
    site.child.kill("SIGTERM");
    linear.child.kill("SIGTERM");
  }
};

const mb = (bytes: number): string => `${(bytes / 1024 / 1024).toFixed(2)} MB`;
const kb = (bytes: number): string => `${(bytes / 1024).toFixed(1)} KB`;
const ms = (s: Stats): string => `${s.median.toFixed(1)} (${s.p25.toFixed(1)}–${s.p75.toFixed(1)})`;

type Results = {
  meta: Record<string, unknown>;
  bundles?: BundleResult[];
  keyed?: KeyedResult[];
  memory?: MemoryResult[];
  filter?: FilterResult[];
  loads?: LoadResult[];
  live?: LiveResult[];
};

const markdown = (r: Results): string => {
  const out: string[] = [`# Browser benchmark ${String(r.meta["date"])}`, ""];
  if (r.meta["label"]) out.push(String(r.meta["label"]), "");
  out.push(
    `Machine: ${String(r.meta["nproc"])} CPUs, ${String(r.meta["cpuModel"])}, Chromium ${String(r.meta["chromium"])}, Node ${String(r.meta["node"])}.`,
    "",
    `Method: ${String(r.meta["method"])}`,
    "",
    "Times in ms: median (25th–75th percentile).",
    "",
  );
  if (r.keyed && r.keyed.length > 0) {
    const shown = appNames.filter((a) => r.keyed!.some((k) => k.app === a));
    for (const cpu of [...new Set(r.keyed.map((k) => k.cpu))]) {
      out.push(`## Keyed rows, CPU ${cpu}x`, "", `| operation | ${shown.join(" | ")} |`, `|---|${shown.map(() => "---").join("|")}|`);
      for (const op of ops) {
        const cells = shown.map((a) => r.keyed!.find((k) => k.cpu === cpu && k.op === op.id && k.app === a));
        if (cells.every((c) => c === undefined)) continue;
        out.push(`| ${op.label} | ${cells.map((c) => (c === undefined ? "" : c.stats ? ms(c.stats) : "failed")).join(" | ")} |`);
      }
      out.push("");
    }
    const failed = r.keyed.filter((k) => k.error !== undefined);
    if (failed.length > 0) {
      out.push("Failures:", "");
      for (const f of failed) out.push(`- ${f.app} ${f.op} at ${f.cpu}x: ${f.error}`);
      out.push("");
    }
  }
  if (r.memory && r.memory.length > 0) {
    out.push("## Memory (JS heap used after garbage collection, median)", "", "| app | after load | after create 1,000 |", "|---|---|---|");
    for (const m of r.memory) out.push(`| ${m.app} | ${mb(m.readyBytes.median)} | ${mb(m.create1kBytes.median)} |`);
    out.push("");
  }
  if (r.filter && r.filter.length > 0) {
    out.push(
      "## Silica filter (1,000 rows), keystroke to next frame",
      "",
      "| CPU | narrowing keystroke | widening keystroke | clear filter (back to 1,000) |",
      "|---|---|---|---|",
    );
    for (const f of r.filter) out.push(`| ${f.cpu}x | ${ms(f.narrow)} | ${ms(f.widen)} | ${ms(f.reset)} |`);
    out.push("");
  }
  if (r.bundles && r.bundles.length > 0) {
    out.push("## Bundle size", "", "| bundle | raw | gzip -9 | brotli 11 |", "|---|---|---|---|");
    for (const b of r.bundles) out.push(`| ${b.name} | ${kb(b.raw)} | ${kb(b.gzip)} | ${kb(b.brotli)} |`);
    out.push("");
  }
  if (r.loads && r.loads.length > 0) {
    out.push(
      "## Page loads (server render plus hydration)",
      "",
      "| page | CPU | first contentful paint | data-silica-ready | CPU check loop |",
      "|---|---|---|---|---|",
    );
    for (const l of r.loads) out.push(`| ${l.page} | ${l.cpu}x | ${ms(l.fcp)} | ${ms(l.ready)} | ${l.cpuCheckMs.toFixed(0)} ms |`);
    out.push("");
  }
  if (r.live && r.live.length > 0) {
    out.push(
      "## Live: create in one page to the DOM change in another (mini Linear)",
      "",
      "| issues | CPU | create press to DOM change |",
      "|---|---|---|",
    );
    for (const l of r.live) out.push(`| ${l.issues} | ${l.cpu}x | ${ms(l.latency)} |`);
    out.push("");
  }
  return out.join("\n");
};

const main = async (): Promise<void> => {
  const needsApps = suites.some((s) => s === "keyed" || s === "memory");
  if (needsApps) for (const a of apps) if (!existsSync(join(appDirs[a], "index.html"))) throw new Error(`missing ${appDirs[a]}; build it first`);
  if (suites.includes("filter") && !existsSync(join(filterDir, "index.html"))) throw new Error(`missing ${filterDir}; run dune build @all`);

  const browser = await chromium.launch({
    executablePath: chromePath,
    headless: true,
    args: ["--disable-gpu-vsync", "--disable-frame-rate-limit"],
  });
  const { server, base } = await serveStatic({ ...appDirs, filter: filterDir });
  const results: Results = {
    meta: {
      date: new Date().toISOString(),
      label: args.label,
      nproc: cpus().length,
      cpuModel: cpus()[0]?.model ?? "unknown",
      chromium: browser.version(),
      node: process.version,
      options: { apps, ops: selectedOps.map((o) => o.id), suites, cpu: rates, runs, warmup, loadRuns },
      method:
        "keyed and filter times run in the page: performance.now() before element.click() (or the input event), " +
        "end when a MessageChannel message posted from the next requestAnimationFrame callback arrives, " +
        "that is after the frame's style, layout and paint. Chromium runs with --disable-gpu-vsync --disable-frame-rate-limit, " +
        "so the next frame starts at once and the time does not include waiting for vsync. Each measured run uses a fresh browser context and page, " +
        `with ${warmup} untimed setup+operation rounds first in the same page. CPU throttling uses CDP Emulation.setCPUThrottlingRate ` +
        "after the app is loaded. Memory: CDP HeapProfiler.collectGarbage then Runtime.getHeapUsage usedSize. " +
        "Page loads: fresh context (cold HTTP cache), throttle set on a same-origin page first, " +
        "first-contentful-paint entry and the time a MutationObserver sees data-silica-ready on the root; " +
        "the CPU check loop (5M iterations, first load) shows the throttle is active. " +
        "Live: two pages of mini Linear in separate contexts; Date.now() in the writer page just before the Create issue " +
        "button is clicked, and Date.now() in the watcher page when a MutationObserver sees the new title in the list " +
        "(one machine, one clock): the POST, the handler, Server.publish, the WebSocket message, the step and the paint.",
    },
  };
  try {
    if (suites.includes("bundle")) results.bundles = runBundles();
    if (suites.includes("keyed")) results.keyed = await runKeyed(browser, base);
    if (suites.includes("memory")) results.memory = await runMemory(browser, base);
    if (suites.includes("filter")) results.filter = await runFilter(browser, base);
    if (suites.includes("load")) results.loads = await runLoads(browser);
    if (suites.includes("live")) results.live = await runLive(browser);
  } finally {
    server.close();
    await browser.close();
  }
  mkdirSync(dirname(resolve(args.out)), { recursive: true });
  writeFileSync(resolve(args.out), JSON.stringify(results, null, 2) + "\n");
  const md = resolve(args.out).replace(/\.json$/, "") + ".md";
  writeFileSync(md, markdown(results));
  console.log(`wrote ${resolve(args.out)} and ${md}`);
};

await main();
