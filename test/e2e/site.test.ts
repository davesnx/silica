import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, type ChildProcess } from "node:child_process";
import { createServer } from "node:net";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type BrowserContext, type Page } from "playwright-core";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const exe = resolve(root, "_build/default/site/server/main.exe");
const dist = resolve(root, "_build/default/site/client/dist");
const metafile = resolve(root, "_build/default/site/client/meta.json");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";

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

before(async () => {
  assert.ok(existsSync(exe), `missing ${exe}; run dune build`);
  assert.ok(existsSync(metafile), `missing ${metafile}; run dune build`);
  const port = await freePort();
  base = `http://127.0.0.1:${port}`;
  server = spawn(exe, ["--port", String(port), "--static", dist, "--metafile", metafile], { cwd: root });
  server.stdout?.on("data", (d: Buffer) => (serverLog += d.toString()));
  server.stderr?.on("data", (d: Buffer) => (serverLog += d.toString()));
  await waitUp(base + "/");
  browser = await chromium.launch({ executablePath: chromePath, headless: true });
});

after(async () => {
  await browser?.close();
  server?.kill("SIGTERM");
});

type Options = { scheme?: "light" | "dark"; cookie?: string; javaScript?: boolean; width?: number };

const firstFrame = `
  window.__firstFrame = null;
  window.__themeChanges = [];
  new MutationObserver((records) => {
    for (const r of records)
      if (r.target === document.documentElement) window.__themeChanges.push(r.target.getAttribute("data-theme"));
  }).observe(document, { attributes: true, subtree: true, attributeFilter: ["data-theme"] });
  const tick = () => {
    if (document.body) window.__firstFrame = {
      background: getComputedStyle(document.documentElement).backgroundColor,
      theme: document.documentElement.getAttribute("data-theme"),
    };
    else requestAnimationFrame(tick);
  };
  requestAnimationFrame(tick);
`;

const visit = async (
  path: string,
  { scheme = "light", cookie, javaScript = true, width = 1280 }: Options = {},
): Promise<{ context: BrowserContext; page: Page; errors: string[] }> => {
  const context = await browser.newContext({
    colorScheme: scheme,
    javaScriptEnabled: javaScript,
    reducedMotion: "reduce",
    viewport: { width, height: 900 },
  });
  if (cookie) await context.addCookies([{ name: "silica-theme", value: cookie, url: base }]);
  const page = await context.newPage();
  page.setDefaultTimeout(5000);
  const errors: string[] = [];
  page.on("console", (m) => {
    if (m.type() === "error" && !m.text().includes("favicon") && !m.text().includes("404")) errors.push(m.text());
  });
  page.on("pageerror", (e) => errors.push(String(e)));
  await page.addInitScript(firstFrame);
  await page.goto(base + path);
  if (javaScript) await page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
  return { context, page, errors };
};

const ready = (page: Page) => page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
const h1 = async (page: Page) => (await page.locator("h1").first().textContent())?.trim();
const first = (page: Page) =>
  page.waitForFunction(() => (window as any).__firstFrame).then(() => page.evaluate(() => (window as any).__firstFrame));
const background = (page: Page) => page.evaluate(() => getComputedStyle(document.documentElement).backgroundColor);
const marker = (page: Page) => page.evaluate(() => ((window as any).__stay = "kept"));
const kept = (page: Page) => page.evaluate(() => (window as any).__stay);
const sidebar = (page: Page) => page.locator("aside .hidden.lg\\:block");

const white = "rgb(255, 255, 255)";
const darkSurface = "rgb(17, 24, 39)";
const oceanSurface = "rgb(11, 30, 51)";

describe("site: navigation", () => {
  test("header, sidebar and pager links change the page without a reload; the current link is marked", async () => {
    const { context, page, errors } = await visit("/");
    assert.equal(await page.title(), "Silica: views that are statecharts");
    await marker(page);
    await page.locator("#nav-docs").click();
    await page.waitForURL(`${base}/docs`);
    assert.equal(await page.title(), "Documentation · Silica");
    assert.equal(await page.locator("#nav-docs").getAttribute("aria-current"), "page");
    await sidebar(page).locator('a[href="/docs/router"]').click();
    await page.waitForURL(`${base}/docs/router`);
    await page.locator("#doc h1", { hasText: "Router" }).waitFor();
    assert.match(await page.title(), /^Router.* · Silica$/);
    assert.equal(await sidebar(page).locator('a[href="/docs/router"]').getAttribute("aria-current"), "page");
    assert.equal(await page.locator("#nav-docs").getAttribute("data-active"), "", "the parent link is active");
    const next = page.locator("#doc a[rel=next]");
    const nextHref = await next.getAttribute("href");
    await next.click();
    await page.waitForURL(`${base}${nextHref}`);
    await page.goBack();
    await page.waitForURL(`${base}/docs/router`);
    await page.locator("#doc h1", { hasText: "Router" }).waitFor();
    assert.equal(await kept(page), "kept", "no page load happened");
    assert.deepEqual(errors, []);
    await context.close();
  });

  test("a heading anchor puts the hash in the URL and scrolls to the heading", async () => {
    const { context, page } = await visit("/docs/router");
    const toc = page.locator('nav[aria-label="On this page"] a').nth(3);
    const href = (await toc.getAttribute("href")) ?? "";
    const id = href.split("#")[1] ?? "";
    assert.ok(id, "the table of contents links to an anchor");
    await toc.click();
    await page.waitForURL(`${base}/docs/router#${id}`);
    await page.waitForFunction((id) => {
      const top = document.getElementById(id)?.getBoundingClientRect().top ?? 9999;
      return top >= 0 && top < 200;
    }, id);
    await context.close();
  });

  test("a docs link fetches the page on hover; the click uses that answer and asks nothing more", async () => {
    const { context, page } = await visit("/docs");
    const asked: string[] = [];
    page.on("request", (r) => {
      if (r.url().endsWith("/api/doc")) asked.push(r.postData() ?? "");
    });
    const link = sidebar(page).locator('a[href="/docs/checker"]');
    assert.equal(await link.getAttribute("data-silica-preload"), "intent");
    await link.hover();
    await page.waitForFunction(() => true);
    await new Promise((r) => setTimeout(r, 300));
    assert.deepEqual(asked, ['"checker"'], "one preload on hover");
    await link.click();
    await page.waitForURL(`${base}/docs/checker`);
    await page.locator("#doc h1").filter({ hasNotText: "Loading" }).waitFor();
    await new Promise((r) => setTimeout(r, 300));
    assert.deepEqual(asked, ['"checker"'], "the click asked nothing more");
    await context.close();
  });

  test("the landing statechart follows the live sample", async () => {
    const { context, page, errors } = await visit("/");
    const active = () => page.locator("#hero-chart .is-active").getAttribute("data-state");
    assert.equal(await active(), "idle");
    await page.locator("#hero-live button").click();
    await page.waitForFunction(() => document.querySelector("#hero-chart .is-active")?.getAttribute("data-state") === "saving");
    await page.waitForFunction(() => document.querySelector("#hero-chart .is-active")?.getAttribute("data-state") === "saved");
    assert.match((await page.locator("#hero-live").textContent()) ?? "", /Saved at \d\d:\d\d:\d\d/);
    await page.waitForFunction(
      () => document.querySelector("#hero-chart .is-active")?.getAttribute("data-state") === "idle",
      undefined,
      { timeout: 4000 },
    );
    assert.deepEqual(errors, []);
    await context.close();
  });
});

describe("site: search", () => {
  test("Ctrl+K opens the search; a page, a section and a component can be found and opened", async () => {
    const { context, page } = await visit("/");
    await marker(page);
    await page.keyboard.press("Control+k");
    const input = page.getByRole("combobox", { name: "Search the site" });
    await input.waitFor();
    assert.equal(await page.evaluate(() => document.activeElement?.getAttribute("role")), "combobox");
    await input.fill("Router");
    await page.keyboard.press("Enter");
    await page.waitForURL(`${base}/docs/router`);
    await page.locator("#doc h1", { hasText: "Router" }).waitFor();

    await page.locator("#search-open").click();
    await input.fill("Select component");
    await page.keyboard.press("Enter");
    await page.waitForURL(`${base}/components/select`);
    await page.locator("h1", { hasText: "Select" }).waitFor();

    await page.keyboard.press("/");
    await input.fill("Theming: The first paint");
    await page.keyboard.press("Enter");
    await page.waitForURL(`${base}/docs/theming#first-paint`);
    assert.equal(await kept(page), "kept");
    await context.close();
  });

  test("Escape closes the search and nothing matches gives the empty text", async () => {
    const { context, page } = await visit("/docs");
    await page.locator("#search-open").click();
    const input = page.getByRole("combobox", { name: "Search the site" });
    await input.fill("zzzz no such thing");
    await page.getByText("Nothing matches.").waitFor();
    await page.keyboard.press("Escape");
    await page.keyboard.press("Escape");
    await input.waitFor({ state: "detached" });
    assert.equal(page.url(), `${base}/docs`);
    await context.close();
  });
});

describe("site: component browser", () => {
  test("a story control changes the props of the live component", async () => {
    const { context, page, errors } = await visit("/components/switch");
    const preview = page.locator("[data-story=basic] .story-preview [role=switch]").first();
    assert.equal(await preview.getAttribute("aria-checked"), "false");
    assert.equal(await preview.isDisabled(), false);
    await page.locator("#switch-basic-control-checked").click();
    await page.waitForFunction(
      () => document.querySelector("[data-story=basic] .story-preview [role=switch]")?.getAttribute("aria-checked") === "true",
    );
    await page.locator("#switch-basic-control-disabled").click();
    await page.waitForFunction(
      () => (document.querySelector("[data-story=basic] .story-preview [role=switch]") as HTMLButtonElement | null)?.disabled === true,
    );
    assert.equal(await page.locator("#switch-basic-control-checked").getAttribute("aria-checked"), "true");
    assert.deepEqual(errors, []);
    await context.close();
  });

  test("a page shows the statechart, the story source and the story in every theme", async () => {
    const { context, page } = await visit("/components/select");
    const facts = await page.locator("section", { has: page.locator("#statechart") }).textContent();
    assert.match(facts ?? "", /idle/);
    assert.match(facts ?? "", /closed/);
    const source = await page.locator("[data-story=basic] figure pre").textContent();
    assert.match(source ?? "", /Select\.make/);
    const themes = await page.locator("#theme-grid [data-theme]").evaluateAll((els) =>
      els.map((e) => e.getAttribute("data-theme")),
    );
    assert.deepEqual(themes, ["light", "dark", "high-contrast", "ocean"]);
    await context.close();
  });
});

describe("site: lazy routes", () => {
  const chunkOf = (url: string) => /\/static\/site_component_page-[A-Z0-9]+\.js$/.test(url);

  test("a component page loads its chunk before it adopts the server HTML", async () => {
    const context = await browser.newContext({ reducedMotion: "reduce" });
    const page = await context.newPage();
    const warnings: string[] = [];
    page.on("console", (m) => {
      if (m.text().includes("hydration")) warnings.push(m.text());
    });
    const chunks: string[] = [];
    page.on("request", (r) => {
      if (chunkOf(r.url())) chunks.push(r.url());
    });
    const response = await page.goto(base + "/components/switch");
    const html = (await response?.text()) ?? "";
    const preloads = [...html.matchAll(/<link rel="modulepreload" href="([^"]+)"/g)].map((m) => m[1]);
    assert.ok(preloads.length >= 2, "the entry and its shared chunk are preloaded");
    assert.ok(!preloads.some((p) => chunkOf(base + p)), "the lazy chunk is not preloaded by the HTML");
    await ready(page);
    assert.equal(chunks.length, 1, "the chunk of the page loaded once");
    assert.deepEqual(warnings, [], "no hydration warning or mismatch");
    assert.equal(await page.locator("p[aria-busy]").count(), 0, "no loading view");
    await page.locator("#switch-basic-control-checked").click();
    await page.waitForFunction(
      () => document.querySelector("[data-story=basic] .story-preview [role=switch]")?.getAttribute("aria-checked") === "true",
    );
    await context.close();
  });

  test("other pages do not load the chunk; a link to a component loads it on hover", async () => {
    const { context, page, errors } = await visit("/components");
    const chunks: string[] = [];
    page.on("request", (r) => {
      if (chunkOf(r.url())) chunks.push(r.url());
    });
    await new Promise((r) => setTimeout(r, 300));
    assert.deepEqual(chunks, [], "the index page needs no chunk");
    const link = page.locator('#component-list a[href="/components/tabs"]');
    assert.equal(await link.getAttribute("data-silica-preload"), "intent");
    await marker(page);
    await link.hover();
    await page.waitForFunction(() => performance.getEntriesByType("resource").some((e) => /site_component_page-/.test(e.name)));
    assert.equal(chunks.length, 1, "hover loads the chunk");
    await link.click();
    await page.waitForURL(`${base}/components/tabs`);
    await page.locator("h1", { hasText: "Tabs" }).waitFor();
    assert.equal(chunks.length, 1, "the click loads nothing more");
    assert.equal(await kept(page), "kept", "no page load happened");
    assert.deepEqual(errors, []);
    await context.close();
  });
});

describe("site: theme", () => {
  for (const [path, scheme, cookie, theme, bg] of [
    ["/", "dark", undefined, "system", darkSurface],
    ["/docs/router", "dark", undefined, "system", darkSurface],
    ["/docs/router", "dark", "light", "light", white],
    ["/components", "light", "ocean", "ocean", oceanSurface],
  ] as const) {
    test(`no flash on ${path}: system ${scheme}, cookie ${cookie ?? "none"}`, async () => {
      const { context, page } = await visit(path, { scheme, cookie });
      assert.deepEqual(await first(page), { background: bg, theme });
      const changes: string[] = await page.evaluate(() => (window as any).__themeChanges);
      assert.deepEqual(
        changes.filter((t) => t !== theme),
        [],
        "the client writes no other theme",
      );
      await context.close();
    });
  }

  test("a theme chosen on a docs page is kept across navigation and reload", async () => {
    const { context, page } = await visit("/docs/tutorial");
    await page.locator("#theme-trigger").click();
    await page.getByRole("option", { name: "Ocean" }).click();
    await page.waitForFunction(() => document.documentElement.getAttribute("data-theme") === "ocean");
    assert.equal(await background(page), oceanSurface);
    const stored = (await context.cookies(base)).find((c) => c.name === "silica-theme");
    assert.equal(stored?.value, "ocean");
    await page.locator("#nav-components").click();
    await page.waitForURL(`${base}/components`);
    assert.equal(await page.evaluate(() => document.documentElement.getAttribute("data-theme")), "ocean");
    await page.reload();
    await ready(page);
    assert.deepEqual(await first(page), { background: oceanSurface, theme: "ocean" });
    assert.equal((await page.locator("#theme-trigger").textContent())?.trim(), "Ocean");
    await context.close();
  });
});

describe("site: JavaScript off", () => {
  test("pages are readable and links work with no script", async () => {
    const { context, page } = await visit("/docs/router", { javaScript: false });
    assert.equal(await h1(page), "Router");
    assert.ok((await page.locator("#doc h2").count()) > 3, "the content is in the HTML");
    assert.ok((await page.locator("#doc figure pre").count()) > 0, "code samples are in the HTML");
    await sidebar(page).locator('a[href="/docs/hydration"]').click();
    await page.waitForURL(`${base}/docs/hydration`);
    assert.ok((await page.locator("#doc h2").count()) > 0);
    await page.locator("#nav-components").click();
    await page.waitForURL(`${base}/components`);
    await page.locator('#component-list a[href="/components/dialog"]').click();
    await page.waitForURL(`${base}/components/dialog`);
    assert.match((await page.locator("[data-story=basic] figure pre").textContent()) ?? "", /Dialog/);
    assert.ok((await page.locator("[data-story=basic] .story-preview button").count()) > 0, "the story is rendered");
    await page.goto(base + "/");
    assert.equal(await h1(page), "Views that are statecharts.");
    assert.ok((await page.locator("#hero-chart [data-state]").count()) === 4);
    await context.close();
  });

  test("on a phone width, the sidebar is a details element that opens with no script", async () => {
    const { context, page } = await visit("/docs/router", { javaScript: false, width: 390 });
    const details = page.locator("aside details");
    assert.equal(await details.isVisible(), true);
    assert.equal(await sidebar(page).isVisible(), false);
    await details.locator("summary").click();
    await details.locator('a[href="/docs/data"]').click();
    await page.waitForURL(`${base}/docs/data`);
    const overflow = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
    assert.ok(overflow <= 0, `no horizontal scroll (${overflow}px)`);
    await context.close();
  });
});
