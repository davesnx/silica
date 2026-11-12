import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, spawnSync, type ChildProcess } from "node:child_process";
import { createServer } from "node:net";
import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type Page } from "playwright-core";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const exe = resolve(root, "_build/default/linear/server/main.exe");
const dist = resolve(root, "_build/default/linear/client_dev/dist");
const metafile = resolve(root, "_build/default/linear/client_dev/meta.json");
const suite = resolve(root, "_build/default/test/linear_check/main.exe");
const specs = resolve(root, "test/linear_check/spec");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";

const scratch = mkdtempSync(join(tmpdir(), "silica-trace-"));
const traces = join(scratch, "traces");

let server: ChildProcess;
let browser: Browser;
let base: string;
let serverLog = "";

const freePort = (): Promise<number> =>
  new Promise((ok, fail) => {
    const s = createServer();
    s.once("error", fail);
    s.listen(0, "127.0.0.1", () => {
      const addr = s.address();
      const p = typeof addr === "object" && addr ? addr.port : 0;
      s.close(() => ok(p));
    });
  });

const waitUp = async (url: string): Promise<void> => {
  for (let i = 0; i < 100; i++) {
    try {
      await fetch(url);
      return;
    } catch {
      await new Promise((r) => setTimeout(r, 100));
    }
  }
  throw new Error(`server did not start\n${serverLog}`);
};

const hide = (page: Page): Promise<void> =>
  page.evaluate(() => {
    Object.defineProperty(document, "visibilityState", { configurable: true, get: () => "hidden" });
    document.dispatchEvent(new Event("visibilitychange"));
  });

const saved = async (): Promise<string[]> => {
  for (let i = 0; i < 50; i++) {
    const files = existsSync(traces) ? readdirSync(traces) : [];
    if (files.length > 0) return files;
    await new Promise((r) => setTimeout(r, 100));
  }
  return [];
};

before(async () => {
  assert.ok(existsSync(exe), `missing ${exe}; run dune build`);
  assert.ok(existsSync(suite), `missing ${suite}; run dune build`);
  const port = await freePort();
  base = `http://127.0.0.1:${port}`;
  server = spawn(exe, ["--port", String(port), "--static", dist, "--metafile", metafile, "--traces", traces], {
    cwd: root,
    env: {
      ...process.env,
      SILICA_DEV_STATE: join(scratch, "state"),
      SILICA_SECRET: "0123456789abcdef0123456789abcdef",
    },
  });
  server.stdout?.on("data", (d: Buffer) => (serverLog += d.toString()));
  server.stderr?.on("data", (d: Buffer) => (serverLog += d.toString()));
  await waitUp(base + "/");
  browser = await chromium.launch({ executablePath: chromePath, headless: true });
});

after(async () => {
  await browser?.close();
  await new Promise<void>((ok) => {
    if (server.exitCode !== null) return ok();
    server.once("exit", () => ok());
    server.kill("SIGTERM");
  });
  rmSync(scratch, { recursive: true, force: true });
});

describe("mini Linear records a trace in development", () => {
  test("a hidden page posts its trace, the server saves it, and it conforms", async () => {
    const page = await browser.newPage();
    page.setDefaultTimeout(5000);
    const errors: string[] = [];
    page.on("console", (message) => {
      if (message.type() === "error" && message.text().includes("trace")) errors.push(message.text());
    });
    await page.goto(base + "/issue/12");
    await page.locator("#comments-empty").waitFor();
    await page.locator("#comment-input").focus();
    await page.keyboard.type("Typed in the browser");
    await page.keyboard.press("Control+Enter");
    await page.locator("#comments li", { hasText: "Typed in the browser" }).waitFor();
    await hide(page);
    const files = await saved();
    assert.equal(files.length, 1, `one trace saved\n${serverLog}`);
    assert.match(files[0] ?? "", /^trace-\d{8}T\d{6}-\d+\.jsonl$/, "a name that the server made");
    const text = readFileSync(join(traces, files[0] ?? ""), "utf8");
    const header = JSON.parse(text.split("\n")[0] ?? "{}");
    assert.equal(header.root, "router");
    assert.equal(header.build, "dev");
    assert.equal(header.origin.platform, "browser");
    assert.ok(text.includes('"machine":"comment-composer"'), "the composer is in the trace");
    assert.deepEqual(errors, []);
    const conform = spawnSync(suite, ["conform", "--spec", specs, join(traces, files[0] ?? "")], {
      cwd: root,
      encoding: "utf8",
    });
    assert.equal(conform.status, 0, `the trace conforms\n${conform.stdout}${conform.stderr}`);
    await page.close();
  });
});
