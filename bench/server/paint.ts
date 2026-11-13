import { spawn } from "node:child_process";
import { createRequire } from "node:module";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const require = createRequire(resolve(root, "test/e2e/package.json"));
const { chromium } = require("playwright-core") as typeof import("playwright-core");

const [app = "site", runsText = "10", exeArg] = process.argv.slice(2);
const runs = Number(runsText);
const build = resolve(root, "_build/default", app);
const exe = exeArg ?? resolve(build, "server/main.exe");
const port = 18090;
const path = app === "site" ? "/docs/glossary" : "/";

const server = spawn(
  exe,
  ["--port", String(port), "--static", resolve(build, "client/dist"), "--metafile", resolve(build, "client/meta.json")],
  { stdio: "ignore" },
);
for (;;) {
  const up = await fetch(`http://127.0.0.1:${port}/_silica/health`).then((r) => r.ok, () => false);
  if (up) break;
  await new Promise((ok) => setTimeout(ok, 100));
}

const browser = await chromium.launch({ executablePath: process.env["CHROME"] ?? "/usr/bin/chromium" });
const results: Array<{ fcp: number; ready: number }> = [];
for (let i = 0; i < runs; i++) {
  const context = await browser.newContext();
  const page = await context.newPage();
  const cdp = await context.newCDPSession(page);
  await cdp.send("Network.enable");
  await cdp.send("Network.emulateNetworkConditions", {
    offline: false,
    latency: 150,
    downloadThroughput: (1.6 * 1024 * 1024) / 8,
    uploadThroughput: (750 * 1024) / 8,
  });
  await cdp.send("Emulation.setCPUThrottlingRate", { rate: 4 });
  await page.addInitScript(() => {
    const w = window as unknown as { __ready?: number };
    new MutationObserver((_, observer) => {
      if (document.getElementById("silica-root")?.hasAttribute("data-silica-ready")) {
        w.__ready = performance.now();
        observer.disconnect();
      }
    }).observe(document, { attributes: true, subtree: true, attributeFilter: ["data-silica-ready"] });
  });
  await page.goto(`http://localhost:${port}${path}`);
  await page.waitForFunction(() => (window as unknown as { __ready?: number }).__ready !== undefined, null, { timeout: 60000 });
  results.push(
    await page.evaluate(() => ({
      fcp: performance.getEntriesByName("first-contentful-paint")[0]?.startTime ?? NaN,
      ready: (window as unknown as { __ready: number }).__ready,
    })),
  );
  await context.close();
}
await browser.close();
server.kill();

const median = (xs: number[]) => [...xs].sort((a, b) => a - b)[Math.floor(xs.length / 2)] ?? NaN;
console.log(
  `${app} ${path}: ${runs} runs, 1.6 Mbps, 150 ms latency, CPU 4x slower; ` +
    `median first contentful paint ${median(results.map((r) => r.fcp)).toFixed(0)} ms, ` +
    `median ready ${median(results.map((r) => r.ready)).toFixed(0)} ms`,
);
