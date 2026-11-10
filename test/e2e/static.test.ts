import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import { createServer as createHttpServer, request, type Server } from "node:http";
import { createServer } from "node:net";
import { existsSync, mkdtempSync, readFileSync, rmSync, statSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, extname, join, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type BrowserContext, type Page } from "playwright-core";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const exporter = resolve(root, "_build/default/site/export/export.exe");
const exe = resolve(root, "_build/default/site/server/main.exe");
const dist = resolve(root, "_build/default/site/client/dist");
const metafile = resolve(root, "_build/default/site/client/meta.json");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";

let out: string;
let server: Server;
let browser: Browser;
let base: string;

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

const types: Record<string, string> = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json",
};

const isFile = (path: string): boolean => existsSync(path) && statSync(path).isFile();

const staticHost = (dir: string): Server =>
  createHttpServer((req, res) => {
    if (req.method !== "GET" && req.method !== "HEAD") {
      res.writeHead(405).end();
      return;
    }
    const path = decodeURIComponent(new URL(req.url ?? "/", "http://host").pathname);
    const file = [path, `${path}.html`, `${path}/index.html`]
      .map((p) => resolve(dir, "." + p))
      .find((f) => (f === dir || f.startsWith(dir + sep)) && isFile(f));
    const status = file ? 200 : 404;
    const body = readFileSync(file ?? join(dir, "404.html"));
    res.writeHead(status, { "content-type": types[extname(file ?? "404.html")] ?? "application/octet-stream" });
    res.end(req.method === "HEAD" ? undefined : body);
  });

before(async () => {
  assert.ok(existsSync(exporter), `missing ${exporter}; run dune build`);
  out = mkdtempSync(join(tmpdir(), "silica-static-"));
  const run = spawnSync(exporter, ["--out", out, "--static", dist, "--metafile", metafile], { cwd: root });
  assert.equal(run.status, 0, `export failed\n${run.stdout}${run.stderr}`);
  const port = await freePort();
  base = `http://127.0.0.1:${port}`;
  server = staticHost(out);
  await new Promise<void>((ok) => server.listen(port, "127.0.0.1", ok));
  browser = await chromium.launch({ executablePath: chromePath, headless: true });
});

after(async () => {
  await browser?.close();
  await new Promise((ok) => server?.close(ok));
  if (out) rmSync(out, { recursive: true, force: true });
});

const visit = async (
  path: string,
  { javaScript = true }: { javaScript?: boolean } = {},
): Promise<{ context: BrowserContext; page: Page; errors: string[]; posts: string[] }> => {
  const context = await browser.newContext({ javaScriptEnabled: javaScript, reducedMotion: "reduce" });
  const page = await context.newPage();
  page.setDefaultTimeout(5000);
  const errors: string[] = [];
  const posts: string[] = [];
  page.on("console", (m) => {
    if (m.type() === "error" && !m.text().includes("favicon") && !m.text().includes("404")) errors.push(m.text());
  });
  page.on("pageerror", (e) => errors.push(String(e)));
  page.on("request", (r) => {
    if (r.method() !== "GET") posts.push(r.url());
  });
  await page.goto(base + path);
  if (javaScript) await page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
  return { context, page, errors, posts };
};

const sidebar = (page: Page) => page.locator("aside .hidden.lg\\:block");
const h1 = async (page: Page) => (await page.locator("h1").first().textContent())?.trim();

describe("static export", () => {
  test("docs links change the page without a reload, and back works", async () => {
    const { context, page, errors, posts } = await visit("/");
    await page.evaluate(() => ((window as any).__stay = "kept"));
    await page.locator("#nav-docs").click();
    await page.waitForURL(`${base}/docs`);
    await sidebar(page).locator('a[href="/docs/router"]').click();
    await page.waitForURL(`${base}/docs/router`);
    await page.locator("#doc h1", { hasText: "Router" }).waitFor();
    await page.locator("#doc h2").nth(3).waitFor();
    const next = page.locator("#doc a[rel=next]");
    const nextHref = await next.getAttribute("href");
    await next.click();
    await page.waitForURL(`${base}${nextHref}`);
    await page.locator("#doc h1").filter({ hasNotText: "Loading" }).waitFor();
    await page.goBack();
    await page.locator("#doc h1", { hasText: "Router" }).waitFor();
    assert.equal(await page.evaluate(() => (window as any).__stay), "kept", "no page load happened");
    assert.deepEqual(posts, [], "a static page sends no POST");
    assert.deepEqual(errors, []);
    await context.close();
  });

  test("a doc page after the first page reads its answer file once, on hover", async () => {
    const { context, page, posts } = await visit("/docs");
    const asked: string[] = [];
    page.on("request", (r) => {
      if (r.url().includes("/silica-answers/")) asked.push(new URL(r.url()).pathname);
    });
    const link = sidebar(page).locator('a[href="/docs/checker"]');
    await link.hover();
    await new Promise((r) => setTimeout(r, 300));
    assert.equal(asked.length, 1, "one preload on hover");
    assert.match(asked[0] ?? "", /^\/silica-answers\/api\/doc\/[0-9a-f]{8}\.json$/);
    await link.click();
    await page.waitForURL(`${base}/docs/checker`);
    await page.locator("#doc h1").filter({ hasNotText: "Loading" }).waitFor();
    assert.equal(await h1(page), "The checker and Quint");
    await new Promise((r) => setTimeout(r, 300));
    assert.equal(asked.length, 1, "the click asked nothing more");
    assert.deepEqual(posts, []);
    await context.close();
  });

  test("a component page runs its stories", async () => {
    const { context, page, errors } = await visit("/components/switch");
    await page.locator("#switch-basic-control-checked").click();
    await page.waitForFunction(
      () => document.querySelector("[data-story=basic] .story-preview [role=switch]")?.getAttribute("aria-checked") === "true",
    );
    assert.deepEqual(errors, []);
    await context.close();
  });

  test("a chosen theme is kept across navigation and reload", async () => {
    const { context, page } = await visit("/docs/tutorial");
    await page.locator("#theme-trigger").click();
    await page.getByRole("option", { name: "Ocean" }).click();
    await page.waitForFunction(() => document.documentElement.getAttribute("data-theme") === "ocean");
    await page.locator("#nav-components").click();
    await page.waitForURL(`${base}/components`);
    assert.equal(await page.evaluate(() => document.documentElement.getAttribute("data-theme")), "ocean");
    await page.reload();
    await page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
    await page.waitForFunction(() => document.documentElement.getAttribute("data-theme") === "ocean");
    await context.close();
  });

  test("the save sample shows its failed state: a static host has no server", async () => {
    const { context, page } = await visit("/");
    await page.locator("#hero-live button").click();
    await page.locator("#hero-live button", { hasText: "Not saved. Try again" }).waitFor();
    await context.close();
  });

  test("pages read and links work with no script; an unknown path gets the not-found page", async () => {
    const { context, page } = await visit("/docs/router", { javaScript: false });
    assert.equal(await h1(page), "Router");
    assert.ok((await page.locator("#doc h2").count()) > 3);
    await sidebar(page).locator('a[href="/docs/hydration"]').click();
    await page.waitForURL(`${base}/docs/hydration`);
    assert.ok((await page.locator("#doc h2").count()) > 0);
    await page.locator("#nav-components").click();
    await page.waitForURL(`${base}/components`);
    await page.locator('#component-list a[href="/components/dialog"]').click();
    await page.waitForURL(`${base}/components/dialog`);
    assert.ok((await page.locator("[data-story=basic] .story-preview button").count()) > 0);
    const missing = await page.goto(base + "/no/such/page");
    assert.equal(missing?.status(), 404);
    assert.equal(await page.title(), "Not found · Silica");
    await context.close();
  });
});

type Reply = { status: number; headers: Record<string, string | string[] | undefined>; body: string };

const send = (url: string, options: { method?: string; headers?: Record<string, string>; body?: string } = {}): Promise<Reply> =>
  new Promise((ok, fail) => {
    const req = request(url, { method: options.method ?? "GET", headers: options.headers, agent: false }, (res) => {
      let body = "";
      res.on("data", (d: Buffer) => (body += d.toString()));
      res.on("end", () => ok({ status: res.statusCode ?? 0, headers: res.headers, body }));
    });
    req.on("error", fail);
    req.end(options.body);
  });

describe("server: graceful stop", () => {
  test("SIGTERM lets a request in flight finish, then the process exits with 0", async () => {
    const port = await freePort();
    const origin = `http://127.0.0.1:${port}`;
    const child = spawn(exe, ["--port", String(port), "--static", dist, "--metafile", metafile], { cwd: root });
    let log = "";
    child.stderr?.on("data", (d: Buffer) => (log += d.toString()));
    const exited = new Promise<number | null>((ok) => child.on("exit", (code) => ok(code)));
    let page: Reply | undefined;
    for (let i = 0; i < 100 && !page; i++) {
      page = await send(origin + "/").catch(() => undefined);
      if (!page) await new Promise((r) => setTimeout(r, 100));
    }
    assert.ok(page, `server did not start\n${log}`);
    const cookie = String(page.headers["set-cookie"] ?? "").split(";")[0] ?? "";
    const csrf = /<meta name="silica-csrf" content="([^"]*)">/.exec(page.body)?.[1] ?? "";
    const health = await send(origin + "/_silica/health");
    assert.equal(health.status, 200);
    const saving = send(origin + "/api/save", {
      method: "POST",
      headers: { origin, cookie, "content-type": "application/json", "x-silica-csrf": csrf },
      body: '"draft"',
    });
    await new Promise((r) => setTimeout(r, 150));
    child.kill("SIGTERM");
    const saved = await saving;
    assert.equal(saved.status, 200, saved.body);
    assert.match(saved.body, /\d\d:\d\d:\d\d/);
    const code = await Promise.race([exited, new Promise<string>((r) => setTimeout(() => r("timeout"), 5000))]);
    assert.equal(code, 0, log);
    await assert.rejects(send(origin + "/_silica/health"), "the port is closed");
  });
});
