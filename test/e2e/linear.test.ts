import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, type ChildProcess } from "node:child_process";
import { createServer } from "node:net";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type Page } from "playwright-core";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const exe = resolve(root, "_build/default/linear/server/main.exe");
const dist = resolve(root, "_build/default/linear/client/dist");
const metafile = resolve(root, "_build/default/linear/client/meta.json");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";

let server: ChildProcess;
let browser: Browser;
let port: number;
let base: string;
let serverLog = "";

const startServer = (onPort: number): ChildProcess => {
  const child = spawn(exe, ["--port", String(onPort), "--static", dist, "--metafile", metafile], { cwd: root });
  child.stdout?.on("data", (d: Buffer) => (serverLog += d.toString()));
  child.stderr?.on("data", (d: Buffer) => (serverLog += d.toString()));
  return child;
};

const stopServer = (child: ChildProcess): Promise<void> =>
  new Promise((ok) => {
    if (child.exitCode !== null) return ok();
    child.once("exit", () => ok());
    child.kill("SIGTERM");
  });

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
  port = await freePort();
  base = `http://127.0.0.1:${port}`;
  server = startServer(port);
  await waitUp(base + "/");
  browser = await chromium.launch({ executablePath: chromePath, headless: true });
});

after(async () => {
  await browser?.close();
  await stopServer(server);
});

const open = async (path: string): Promise<Page> => {
  const page = await browser.newPage();
  page.setDefaultTimeout(5000);
  await page.goto(base + path);
  await page.locator("#issue-list li, #issue-detail, #issue-not-found").first().waitFor();
  return page;
};

const rows = (page: Page) => page.locator("#issue-list li");
const marker = (page: Page) => page.evaluate(() => (window as unknown as { __marker?: number }).__marker);
const mark = (page: Page) =>
  page.evaluate(() => {
    (window as unknown as { __marker?: number }).__marker = 1;
  });

const createIssue = async (page: Page, title: string) => {
  await page.locator("#new-issue").click();
  await page.locator("[role=dialog]").waitFor();
  await page.getByLabel("Title").fill(title);
  await page.getByRole("button", { name: "Create issue" }).click();
  await page.locator("[role=dialog]").waitFor({ state: "detached" });
};

describe("mini Linear", () => {
  test("the server renders the list and the user; styles are applied", async () => {
    const res = await fetch(base + "/");
    assert.equal(res.status, 200);
    const html = await res.text();
    assert.match(html, /Login page flashes on load/);
    assert.match(html, /you are (Ada|Ben|Cleo)/);
    assert.match(html, /<link rel="stylesheet" href="\/static\/app-[A-Z0-9]{8,}\.css">/);
    assert.match(html, /\/static\/client_main-[A-Z0-9]{8,}\.js/);
    const css = html.match(/href="(\/static\/app-[A-Z0-9]{8,}\.css)"/)![1]!;
    const sheet = await fetch(base + css);
    assert.equal(sheet.status, 200);
    assert.match(sheet.headers.get("content-type") ?? "", /text\/css/);
    assert.match(sheet.headers.get("cache-control") ?? "", /immutable/);

    const page = await open("/");
    assert.equal(await rows(page).count(), 30);
    assert.match((await page.locator("#me").textContent()) ?? "", /^you are (Ada|Ben|Cleo)$/);
    const button = await page.locator("#new-issue").evaluate((el) => {
      const style = getComputedStyle(el);
      return { background: style.backgroundColor, color: style.color, radius: style.borderRadius };
    });
    assert.notEqual(button.background, "rgba(0, 0, 0, 0)", "tailwind background applied");
    assert.notEqual(button.radius, "0px", "tailwind radius applied");
    const pageBackground = () => page.evaluate(() => getComputedStyle(document.documentElement).backgroundColor);
    const light = await pageBackground();
    await page.emulateMedia({ colorScheme: "dark" });
    const dark = await pageBackground();
    assert.notEqual(light, dark, "prefers-color-scheme changes the page");
    const decoration = (title: string) =>
      page.locator("#issue-list li", { hasText: title }).locator("span.truncate").evaluate((el) => getComputedStyle(el).textDecorationLine);
    assert.equal(await decoration("Remove the old billing code"), "line-through", "a conditional class from Class.join has CSS");
    assert.equal(await decoration("Login page flashes on load"), "none");
    await page.close();
  });

  test("the first paint adopts the server HTML and runs no second Remote call", async () => {
    const page = await browser.newPage();
    page.setDefaultTimeout(5000);
    const asked: string[] = [];
    page.on("request", (request) => {
      if (request.url().includes("/api/")) asked.push(request.url());
    });
    let release: () => void = () => {};
    const gate = new Promise<void>((ok) => (release = ok));
    await page.route("**/static/client_main-*.js", async (route) => {
      await gate;
      await route.continue();
    });
    await page.goto(base + "/", { waitUntil: "commit" });
    await rows(page).first().waitFor({ state: "attached" });
    assert.equal(await rows(page).count(), 30, "the rows are there before any script runs");
    await page.evaluate(() => {
      const root = document.getElementById("silica-root")!;
      const w = window as unknown as { __before: Element[]; __removed: number };
      w.__before = [...root.querySelectorAll("*")];
      w.__removed = 0;
      new MutationObserver((records) => {
        for (const record of records) for (const removed of record.removedNodes) if (removed.nodeType === 1) w.__removed++;
      }).observe(root, { childList: true, subtree: true });
    });
    const socket = page.waitForEvent("websocket");
    release();
    await socket;
    await page.locator("#new-issue").click();
    await page.locator("[role=dialog]").waitFor();
    const result = await page.evaluate(() => {
      const w = window as unknown as { __before: Element[]; __removed: number };
      const inDialog = (n: Node) => !!(n as Element).closest?.("[role=dialog]");
      return {
        checked: w.__before.length,
        replaced: w.__before.filter((el) => !el.isConnected && !inDialog(el)).length,
        removed: w.__removed,
      };
    });
    assert.ok(result.checked > 60, `checked ${result.checked} elements`);
    assert.equal(result.replaced, 0, "no server element was replaced");
    assert.equal(result.removed, 0, "no element was removed");
    assert.deepEqual(asked, [], "no Remote call ran on load");
    await page.close();
  });

  test("a click opens the detail with no reload; back returns to the list", async () => {
    const page = await open("/");
    await mark(page);
    const asked: string[] = [];
    page.on("request", (request) => {
      if (request.url().includes("/api/")) asked.push(request.url());
    });
    await page.locator("#issue-list a", { hasText: "Login page flashes on load" }).click();
    await page.waitForURL(base + "/issue/3");
    await page.locator("#issue-detail").waitFor();
    assert.equal(await page.locator("#issue-detail h1").textContent(), "Login page flashes on load");
    const text = (await page.locator("#issue-detail").textContent()) ?? "";
    for (const part of ["ENG-3", "In Progress", "Urgent", "Ada", "bug", "2026-11-05"]) assert.ok(text.includes(part), `${part} in ${text}`);
    assert.equal(await page.title(), "ENG-3 Login page flashes on load");
    assert.equal(await marker(page), 1, "no page load");
    assert.deepEqual(
      asked.map((url) => new URL(url).pathname).sort(),
      ["/api/comments", "/api/issues"],
      `one call for the issue and one for its comments: ${asked}`,
    );
    await page.goBack();
    await rows(page).first().waitFor();
    assert.equal(await rows(page).count(), 30);
    assert.equal(await marker(page), 1);
    await page.close();
  });

  test("a direct visit to a detail has the issue in the server HTML; an unknown issue says so", async () => {
    const html = await (await fetch(base + "/issue/4")).text();
    assert.match(html, /<h1[^>]*>Add an export to CSV<\/h1>/);
    const page = await open("/issue/4");
    assert.equal(await page.locator("#issue-detail h1").textContent(), "Add an export to CSV");
    await page.close();
    const missing = await open("/issue/9999");
    assert.equal(await missing.locator("#issue-not-found").textContent(), "There is no issue ENG-9999.");
    await missing.close();
    const nowhere = await fetch(base + "/nowhere");
    assert.equal(nowhere.status, 404);
  });

  test("create through the modal: validation, focus, close, and the new issue shows", async () => {
    const page = await open("/");
    await mark(page);
    const link = page.locator("#new-issue");
    await link.click();
    await page.waitForURL(base + "/?new=true");
    const dialog = page.locator("[role=dialog]");
    await dialog.waitFor();
    assert.equal(await marker(page), 1, "no page load");
    assert.equal(await page.evaluate(() => document.activeElement?.getAttribute("aria-label")), "Title", "title has focus");
    await page.getByRole("button", { name: "Create issue" }).click();
    assert.equal(await page.locator("#new-issue-error").textContent(), "Title is required");
    assert.equal(await rows(page).count(), 30, "nothing was created");
    await page.getByLabel("Title").fill("Created from the test");
    await page.getByLabel("Description").fill("First line\nSecond line");
    await page.getByLabel("Priority").selectOption("Urgent");
    await page.getByLabel("Status").selectOption("In Progress");
    await page.getByRole("button", { name: "Create issue" }).click();
    await dialog.waitFor({ state: "detached" });
    await page.waitForURL(base + "/");
    assert.equal(await page.evaluate(() => document.activeElement?.id), "new-issue", "focus returns to the trigger");
    await page.waitForFunction(() => document.querySelectorAll("#issue-list li").length === 31);
    assert.match((await page.locator("#issue-list li", { hasText: "Created from the test" }).textContent()) ?? "", /ENG-31.*Created from the test.*In Progress/);
    await page.locator("#issue-list a", { hasText: "Created from the test" }).click();
    await page.locator("#issue-detail").waitFor();
    const text = (await page.locator("#issue-detail").textContent()) ?? "";
    for (const part of ["Urgent", "In Progress", "First line\nSecond line"]) assert.ok(text.includes(part), `${part} in ${text}`);
    assert.equal(await marker(page), 1);
    await page.close();
  });

  test("the modal closes with Escape, with Cancel and with the back button; the URL flag opens it", async () => {
    const page = await open("/?new=true");
    const dialog = page.locator("[role=dialog]");
    await dialog.waitFor();
    assert.equal(await page.evaluate(() => document.activeElement?.getAttribute("aria-label")), "Title", "focus on load");
    await page.keyboard.press("Escape");
    await dialog.waitFor({ state: "detached" });
    await page.waitForURL(base + "/");
    await page.locator("#new-issue").click();
    await dialog.waitFor();
    await page.getByRole("button", { name: "Cancel", exact: true }).click();
    await dialog.waitFor({ state: "detached" });
    await page.locator("#new-issue").click();
    await dialog.waitFor();
    await page.goBack();
    await dialog.waitFor({ state: "detached" });
    const trapped = await (async () => {
      await page.locator("#new-issue").click();
      await dialog.waitFor();
      for (let i = 0; i < 12; i++) await page.keyboard.press("Tab");
      return page.evaluate(() => !!document.activeElement?.closest("[role=dialog]"));
    })();
    assert.equal(trapped, true, "focus stays inside the dialog");
    await page.close();
  });

  test("a second page sees a new issue at once, on the list and on a detail", async () => {
    const a = await open("/");
    const b = await open("/");
    const c = await open("/issue/1");
    const before = await rows(b).count();
    assert.notEqual(
      await a.locator("#me").textContent(),
      await b.locator("#me").textContent(),
      "each session has its own user",
    );
    await createIssue(a, "Seen by the others");
    await b.waitForFunction((n) => document.querySelectorAll("#issue-list li").length === n, before + 1);
    assert.match((await rows(b).first().textContent()) ?? "", /Seen by the others/);
    await createIssue(b, "Second from B");
    await a.waitForFunction((n) => document.querySelectorAll("#issue-list li").length === n, before + 2);
    assert.match((await rows(a).first().textContent()) ?? "", /Second from B/);
    assert.equal(await c.locator("#issue-detail h1").textContent(), "Set up the project", "the detail page stays valid");
    for (const page of [a, b, c]) await page.close();
  });

  test("filters and display options live in the URL; keyboard only", async () => {
    const page = await open("/");
    await page.locator("body").press("f");
    await page.locator("#issue-filter-popup").waitFor();
    await page.keyboard.press("Tab");
    await page.keyboard.press("Tab");
    assert.equal(await page.evaluate(() => document.activeElement?.id), "filter-status-todo");
    await page.keyboard.press("Space");
    await page.waitForURL(base + "/?status=todo");
    assert.equal(await page.evaluate(() => document.activeElement?.id), "filter-status-todo", "focus stays on the box after the list changes");
    const statuses = await rows(page).locator("span:last-child").allTextContents();
    assert.ok(statuses.length > 0 && statuses.every((s) => s === "Todo"), statuses.join(","));
    assert.equal(await page.locator("#issue-filter-trigger").textContent(), "Filter (1)");
    await page.keyboard.press("Escape");
    await page.locator("#issue-filter-popup").waitFor({ state: "detached" });
    assert.equal(await page.evaluate(() => document.activeElement?.id), "issue-filter-trigger");
    await page.keyboard.press("Tab");
    assert.equal(await page.evaluate(() => document.activeElement?.id), "issue-grouping-trigger");
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("Enter");
    await page.waitForURL(base + "/?status=todo&group=assignee");
    await page.locator("#group-assignee-ada").waitFor();
    const html = await (await fetch(base + "/?status=todo&group=assignee")).text();
    assert.ok(html.includes(">Users cannot reset a password</span>"), "the server renders the same view");
    assert.ok(!html.includes(">Set up the project</span>"), "a done issue is filtered out on the server too");
    await page.keyboard.press("Tab");
    await page.keyboard.press("Tab");
    assert.equal(await page.evaluate(() => document.activeElement?.id), "group-assignee-ada");
    await page.keyboard.press("Enter");
    assert.equal(await page.locator("#group-assignee-ada").getAttribute("aria-expanded"), "false");
    const shown = await rows(page).count();
    const other = await open("/");
    await createIssue(other, "A backlog issue from another page");
    await other.close();
    await page.waitForTimeout(300);
    assert.equal(await rows(page).count(), shown, "a push of a non-matching issue changes nothing");
    assert.equal(await page.locator("#group-assignee-ada").getAttribute("aria-expanded"), "false", "the collapsed group stays collapsed");
    assert.equal(page.url(), base + "/?status=todo&group=assignee");
    await page.close();
  });

  test("a bad query value falls back to its default", async () => {
    const page = await open("/?status=nope&group=assignee&order=size");
    await page.locator("#group-assignee-ada").waitFor();
    assert.ok((await rows(page).count()) >= 30);
    await page.close();
  });

  test("inline edit, keyboard only: a picked status shows at once and is saved", async () => {
    const page = await open("/issue/4");
    await page.locator("body").press("s");
    await page.locator("#status-picker-popup").waitFor();
    await page.keyboard.press("d");
    await page.keyboard.press("Enter");
    assert.equal(await page.locator("#status-picker-trigger").textContent(), "Status: Done");
    await page.waitForFunction(() => document.querySelector("#issue-detail")?.getAttribute("data-pending") === "0");
    const other = await open("/issue/4");
    assert.equal(await other.locator("#status-picker-trigger").textContent(), "Status: Done", "the server has it");
    await other.close();
    await page.keyboard.press("Tab");
    await page.locator("#edit-title").focus();
    await page.keyboard.press("Enter");
    assert.equal(await page.evaluate(() => document.activeElement?.id), "title-input");
    await page.keyboard.press("Escape");
    await page.locator("#title-input").waitFor({ state: "detached" });
    await page.locator("#edit-title").focus();
    await page.keyboard.press("Enter");
    await page.keyboard.press("ControlOrMeta+a");
    await page.keyboard.type("Export to CSV, renamed");
    await page.keyboard.press("Enter");
    assert.equal(await page.locator("#issue-detail h1").textContent(), "Export to CSV, renamed");
    await page.locator("#edit-description").focus();
    await page.keyboard.press("Enter");
    await page.keyboard.type(" More.");
    await page.keyboard.press("Control+Enter");
    await page.locator("#issue-description", { hasText: "More." }).waitFor();
    await page.waitForFunction(() => document.querySelector("#issue-detail")?.getAttribute("data-pending") === "0");
    await page.close();
  });

  test("inline edit, keyboard only: labels toggle in a popover and focus stays", async () => {
    const page = await open("/issue/13");
    await page.locator("body").press("l");
    await page.locator("#labels-picker-popup").waitFor();
    await page.keyboard.press("Tab");
    assert.equal(await page.evaluate(() => document.activeElement?.id), "label-bug");
    const trigger = page.locator("#labels-picker-trigger");
    const before = await trigger.textContent();
    await page.keyboard.press("Space");
    await page.waitForFunction((text) => document.querySelector("#labels-picker-trigger")?.textContent !== text, before);
    assert.equal(await page.evaluate(() => document.activeElement?.id), "label-bug", "focus stays on the box");
    await page.keyboard.press("Tab");
    await page.keyboard.press("Space");
    await trigger.filter({ hasText: "feature" }).waitFor();
    await page.keyboard.press("Escape");
    await page.locator("#labels-picker-popup").waitFor({ state: "detached" });
    assert.equal(await page.evaluate(() => document.activeElement?.id), "labels-picker-trigger");
    await page.waitForFunction(() => document.querySelector("#issue-detail")?.getAttribute("data-pending") === "0");
    await page.close();
  });

  test("inline edit: a failed save goes back and a toast says so", async () => {
    const page = await open("/issue/7");
    await page.route("**/api/issues/update", (route) => route.abort());
    const before = await page.locator("#priority-picker-trigger").textContent();
    await page.locator("body").press("p");
    await page.locator("#priority-picker-popup").waitFor();
    await page.keyboard.press("u");
    await page.keyboard.press("Enter");
    await page.locator("#issue-toasts", { hasText: "Could not save the priority" }).waitFor();
    assert.equal(await page.locator("#priority-picker-trigger").textContent(), before, "the old value is back");
    await page.close();
  });

  test("inline edit: a topic state with the change settles it before the answer", async () => {
    const page = await open("/issue/8");
    let release: () => void = () => {};
    const held = new Promise<void>((ok) => (release = ok));
    let answered = false;
    await page.route("**/api/issues/update", async (route) => {
      const response = await route.fetch();
      await held;
      answered = true;
      await route.fulfill({ response });
    });
    await page.locator("#assignee-picker-trigger").click();
    await page.locator("#assignee-picker-popup").waitFor();
    await page.getByRole("menuitem", { name: "Ben" }).click();
    assert.equal(await page.locator("#assignee-picker-trigger").textContent(), "Assignee: Ben");
    await page.waitForFunction(() => document.querySelector("#issue-detail")?.getAttribute("data-pending") === "0");
    assert.equal(answered, false, "the Remote answer has not arrived yet");
    release();
    await page.waitForTimeout(200);
    assert.equal(await page.locator("#assignee-picker-trigger").textContent(), "Assignee: Ben");
    await page.close();
  });

  test("inline edit: the later writer on an old version keeps the draft and sees the new value", async () => {
    const a = await open("/issue/9");
    const b = await open("/issue/9");
    await a.locator("#edit-title").click();
    await a.locator("#title-input").fill("Title from A");
    await b.locator("#edit-title").click();
    await b.locator("#title-input").fill("Title from B");
    await b.locator("#title-input").press("Enter");
    await b.waitForFunction(() => document.querySelector("#issue-detail")?.getAttribute("data-pending") === "0");
    await a.locator("#title-input").press("Enter");
    await a.locator("#edit-note", { hasText: "Title from B" }).waitFor();
    assert.equal(await a.locator("#title-input").inputValue(), "Title from A", "the draft is kept");
    await a.locator("#title-input").press("Enter");
    await b.locator("#issue-detail h1", { hasText: "Title from A" }).waitFor();
    await a.locator("#issue-detail h1", { hasText: "Title from A" }).waitFor();
    for (const page of [a, b]) await page.close();
  });

  test("comments, keyboard only: add with Ctrl+Enter, edit own, others see it live and cannot edit", async () => {
    const a = await open("/issue/11");
    const b = await open("/issue/11");
    await a.locator("#comments-empty").waitFor();
    await a.locator("#comment-input").focus();
    await a.keyboard.type("A first comment");
    await a.keyboard.press("Control+Enter");
    await a.locator("#comments li", { hasText: "A first comment" }).waitFor();
    await a.waitForFunction(
      () => (document.querySelector("#comment-input") as HTMLTextAreaElement | null)?.value === "",
      undefined,
      { timeout: 5000 },
    );
    const item = b.locator("#comments li", { hasText: "A first comment" });
    await item.waitFor();
    assert.equal(await item.locator("button", { hasText: "Edit" }).count(), 0, "another user cannot edit it");
    await a.locator("#comments li button", { hasText: "Edit" }).focus();
    await a.keyboard.press("Enter");
    await a.keyboard.press("ControlOrMeta+a");
    await a.keyboard.type("A first comment, edited");
    await a.keyboard.press("Control+Enter");
    const edited = b.locator("#comments li", { hasText: "A first comment, edited" });
    await edited.waitFor();
    assert.match((await edited.textContent()) ?? "", /\(edited\)/, "others see the edit mark");
    await a.locator("#comment-input").focus();
    await a.keyboard.type("Not this one");
    await a.keyboard.press("Escape");
    assert.equal(await a.locator("#comment-input").inputValue(), "", "Escape clears the draft");
    for (const page of [a, b]) await page.close();
  });

  test("keyboard only: J, K and arrows move the row cursor, Enter opens, C creates with Ctrl+Enter", async () => {
    const page = await open("/?group=none");
    const active = () => page.evaluate(() => document.activeElement?.id ?? "");
    const ids = await rows(page).locator("a").evaluateAll((links) => links.map((a) => a.id));
    await page.locator("body").press("j");
    assert.equal(await active(), ids[0]);
    await page.keyboard.press("j");
    await page.keyboard.press("ArrowDown");
    assert.equal(await active(), ids[2]);
    await page.keyboard.press("k");
    assert.equal(await active(), ids[1]);
    assert.equal(await page.locator(`#${ids[1]}`).getAttribute("data-cursor"), "");
    await page.keyboard.press("Enter");
    await page.waitForURL(base + "/issue/" + ids[1]!.replace("issue-row-", ""));
    await page.goBack();
    await rows(page).first().waitFor();
    await page.locator("body").press("c");
    await page.waitForURL(base + "/?new=true&group=none");
    await page.locator("[role=dialog]").waitFor();
    await page.keyboard.type("jkcfs typed, not shortcuts");
    assert.equal(page.url(), base + "/?new=true&group=none", "letters in the title are text");
    assert.equal(await page.locator("#command-menu-popup").count(), 0);
    await page.keyboard.press("Control+Enter");
    await page.locator("[role=dialog]").waitFor({ state: "detached" });
    await page.waitForURL(base + "/?group=none");
    await page.locator("#issue-list li", { hasText: "jkcfs typed, not shortcuts" }).waitFor();
    await page.close();
  });

  test("keyboard only: the command menu searches issues and sets a property of the focused issue", async () => {
    const page = await open("/");
    await page.locator("body").press("Control+k");
    await page.locator("#command-menu-popup").waitFor();
    await page.keyboard.type("ENG-12");
    await page.keyboard.press("Enter");
    await page.waitForURL(base + "/issue/12");
    await page.locator("#issue-detail").waitFor();
    await page.locator("body").press("Meta+k");
    await page.locator("#command-menu-popup").waitFor();
    await page.keyboard.type("priority: urgent");
    await page.keyboard.press("Enter");
    await page.locator("#command-menu-popup").waitFor({ state: "detached" });
    assert.equal(await page.locator("#priority-picker-trigger").textContent(), "Priority: Urgent");
    await page.locator("body").press("Control+k");
    await page.locator("#command-menu-popup").waitFor();
    await page.keyboard.press("Escape");
    await page.locator("#command-menu-popup").waitFor({ state: "detached" });
    await page.locator("body").press("Control+k");
    await page.keyboard.type("Go to list");
    await page.keyboard.press("Enter");
    await page.waitForURL(base + "/");
    await rows(page).first().waitFor();
    await page.locator("body").press("j");
    const first = await page.evaluate(() => document.activeElement?.id ?? "");
    await page.keyboard.press("Control+k");
    await page.keyboard.type("status: canceled");
    await page.keyboard.press("Enter");
    await page.locator(`#${first}`, { hasText: "Canceled" }).waitFor();
    await page.close();
  });

  test("the server restart: sessions are lost, the page reloads once and recovers; a second loss soon after does not reload", async () => {
    const own = await freePort();
    const url = `http://127.0.0.1:${own}`;
    let child = startServer(own);
    const page = await browser.newPage();
    page.setDefaultTimeout(15000);
    try {
      await waitUp(url + "/_silica/health");
      await page.goto(url + "/");
      await rows(page).first().waitFor();
      await page.locator("#new-issue").click();
      await page.getByLabel("Title").fill("Lost in the restart");
      await page.getByRole("button", { name: "Create issue" }).click();
      await page.waitForFunction(() => document.querySelectorAll("#issue-list li").length === 31);
      await mark(page);
      const loads: number[] = [];
      page.on("load", () => loads.push(Date.now()));

      await stopServer(child);
      await page.locator("#issues-offline").waitFor();
      child = startServer(own);
      await waitUp(url + "/_silica/health");

      for (let waited = 0; loads.length === 0; waited += 100) {
        assert.ok(waited < 20000, `no reload after the restart\n${serverLog}`);
        await new Promise((r) => setTimeout(r, 100));
      }
      await rows(page).first().waitFor();
      assert.equal(await rows(page).count(), 30, "the new server starts from the seed issues");
      assert.equal(await marker(page), undefined, "the page was reloaded");
      assert.match((await page.locator("#me").textContent()) ?? "", /^you are Ada$/, "a new session got the first user");
      await page.locator("#issue-list").waitFor();
      await page.waitForTimeout(1500);
      assert.equal(loads.length, 1, "one reload only");
      assert.equal(await page.locator("#issues-offline").count(), 0, "the page is live again");

      await createIssue(page, "After the restart");
      await page.waitForFunction(() => document.querySelectorAll("#issue-list li").length === 31);

      const errors: string[] = [];
      page.on("console", (m) => {
        if (m.type() === "error") errors.push(m.text());
      });
      await stopServer(child);
      child = startServer(own);
      await waitUp(url + "/_silica/health");
      const given = "silica: session gone again after a reload; not reloading";
      for (let waited = 0; !errors.includes(given); waited += 100) {
        assert.ok(waited < 10000, `no second session-gone message: ${JSON.stringify(errors)}\n${serverLog}`);
        await new Promise((r) => setTimeout(r, 100));
      }
      await page.waitForTimeout(1500);
      assert.equal(loads.length, 1, "a second session loss within 10 s of the reload does not reload again");
      assert.equal(await rows(page).count(), 31, "the page keeps its data");
    } finally {
      await page.close();
      await stopServer(child);
    }
  });
});

const interactive = (page: Page) =>
  page.waitForFunction(() => document.getElementById("silica-root")?.hasAttribute("data-silica-ready"));
const historyAt = (page: Page, path: string, index: number) =>
  page.waitForFunction(
    ([path, index]) =>
      location.pathname === path && (history.state as { silicaIndex?: number } | null)?.silicaIndex === index,
    [path, index] as const,
  );
const apiCalls = (page: Page): string[] => {
  const asked: string[] = [];
  page.on("request", (request) => {
    if (request.url().includes("/api/")) asked.push(new URL(request.url()).pathname);
  });
  return asked;
};

describe("mini Linear router", () => {
  test("hover on a row preloads the issue and its comments; the click shows the detail at once", async () => {
    const page = await open("/");
    await interactive(page);
    const asked = apiCalls(page);
    const answered = Promise.all([
      page.waitForResponse((r) => new URL(r.url()).pathname === "/api/issues"),
      page.waitForResponse((r) => new URL(r.url()).pathname === "/api/comments"),
    ]);
    await page.locator("#issue-row-3").hover();
    await answered;
    assert.deepEqual([...asked].sort(), ["/api/comments", "/api/issues"], "the hover runs both calls");
    const shown = await page.evaluate(() => {
      (document.getElementById("issue-row-3") as HTMLElement).click();
      const text = document.getElementById("silica-root")?.textContent ?? "";
      return {
        path: location.pathname,
        detail: document.querySelector("#issue-detail h1")?.textContent ?? null,
        loading: text.includes("Loading issue") || text.includes("Loading comments"),
      };
    });
    assert.deepEqual(shown, { path: "/issue/3", detail: "Login page flashes on load", loading: false });
    await page.waitForTimeout(300);
    assert.deepEqual([...asked].sort(), ["/api/comments", "/api/issues"], "the click makes no second request");
    await page.close();
  });

  test("the header link is the current page on the list only", async () => {
    const page = await open("/");
    await interactive(page);
    const nav = page.locator("#nav-issues");
    assert.equal(await nav.getAttribute("aria-current"), "page");
    await page.locator("#issue-row-4").click();
    await page.locator("#issue-detail").waitFor();
    assert.equal(await nav.getAttribute("aria-current"), null);
    await nav.click();
    await rows(page).first().waitFor();
    assert.equal(await nav.getAttribute("aria-current"), "page");
    await page.close();
  });

  test("after a click on an issue, focus is on the detail heading and the title is announced", async () => {
    const page = await open("/");
    await interactive(page);
    const preloaded = page.waitForResponse((r) => new URL(r.url()).pathname === "/api/comments");
    await page.locator("#issue-row-3").hover();
    await preloaded;
    await page.locator("#issue-row-3").click();
    await page.waitForURL(base + "/issue/3");
    await page.waitForFunction(() => document.activeElement?.matches("#issue-detail h1") === true);
    await page.waitForFunction(
      () => document.querySelector("[data-silica-announcer]")?.textContent === "ENG-3 Login page flashes on load",
    );
    await page.close();
  });

  test("a title draft stops back and a link press; Keep editing stays, Discard goes", async () => {
    const page = await open("/");
    await interactive(page);
    await mark(page);
    await page.locator("#issue-row-5").click();
    await historyAt(page, "/issue/5", 1);
    const saved = (await page.locator("#issue-detail h1").textContent()) ?? "";
    await page.locator("#edit-title").click();
    await page.locator("#title-input").fill(saved);
    await page.goBack();
    await page.waitForTimeout(200);
    await historyAt(page, "/", 0);
    await page.goForward();
    await historyAt(page, "/issue/5", 1);

    await page.locator("#edit-title").click();
    await page.locator("#title-input").fill("A draft that is not saved");
    const confirm = page.getByRole("alertdialog");
    await page.goBack();
    await confirm.waitFor();
    await page.waitForTimeout(200);
    await historyAt(page, "/issue/5", 1);
    assert.equal(await page.evaluate(() => document.activeElement?.id), "unsaved-keep", "focus is on Keep editing");
    await page.keyboard.press("Enter");
    await confirm.waitFor({ state: "detached" });
    assert.equal(await page.evaluate(() => document.activeElement?.id), "title-input", "focus is back in the editor");
    assert.equal(await page.locator("#title-input").inputValue(), "A draft that is not saved");

    await page.locator("a", { hasText: "Back to issues" }).click();
    await confirm.waitFor();
    await page.waitForTimeout(200);
    await historyAt(page, "/issue/5", 1);
    await page.keyboard.press("Shift+Tab");
    assert.equal(await page.evaluate(() => document.activeElement?.id), "unsaved-discard");
    await page.keyboard.press("Enter");
    await historyAt(page, "/", 2);
    await rows(page).first().waitFor();

    await page.goBack();
    await historyAt(page, "/issue/5", 1);
    await page.locator("#edit-description").click();
    await page.locator("#description-input").fill("A description draft");
    await page.goBack();
    await confirm.waitFor();
    await page.getByRole("button", { name: "Discard" }).click();
    await historyAt(page, "/", 0);
    await rows(page).first().waitFor();
    assert.equal(await marker(page), 1, "no page load");
    await page.close();
  });

  test("the list position comes back on back, after the list loads", async () => {
    const page = await browser.newPage({ viewport: { width: 800, height: 400 } });
    page.setDefaultTimeout(5000);
    await page.goto(base + "/");
    await rows(page).first().waitFor();
    await interactive(page);
    await mark(page);
    await page.evaluate(() => window.scrollTo(0, 500));
    await page.waitForFunction(() => Math.round(window.scrollY) === 500);
    await page.evaluate(() => (document.getElementById("issue-row-12") as HTMLElement).click());
    await page.locator("#issue-detail").waitFor();
    let release: () => void = () => {};
    const held = new Promise<void>((ok) => (release = ok));
    await page.route("**/api/issues", async (route) => {
      await held;
      await route.continue();
    });
    await page.goBack();
    await page.waitForURL(base + "/");
    await page.getByText("Loading issues...").waitFor();
    assert.ok((await page.evaluate(() => window.scrollY)) < 500, "the loading list is too short for the position");
    release();
    await rows(page).first().waitFor();
    await page.waitForFunction(() => Math.round(window.scrollY) === 500);
    assert.equal(await marker(page), 1, "no page load");
    await page.close();
  });
});

describe("mini Linear theme", () => {
  test("the cookie theme is in the first HTML, with the app tokens; the switcher changes it", async () => {
    const context = await browser.newContext({ colorScheme: "light" });
    await context.addCookies([{ name: "silica-theme", value: "dark", url: base }]);
    const html = await (await context.request.get(base + "/")).text();
    assert.match(html, /^<!doctype html><html data-theme="dark">/, "the server writes the theme");
    const page = await context.newPage();
    page.setDefaultTimeout(5000);
    await page.goto(base + "/");
    await page.locator("#issue-list li").first().waitFor();
    await page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
    const tokens = () =>
      page.evaluate(() => {
        const style = getComputedStyle(document.documentElement);
        return { background: style.backgroundColor, warning: style.getPropertyValue("--warning").trim() };
      });
    assert.deepEqual(await tokens(), { background: "rgb(17, 24, 39)", warning: "#fbbf24" });
    await page.locator("#theme-trigger").click();
    await page.getByRole("option", { name: "Ocean" }).click();
    await page.waitForFunction(() => document.documentElement.getAttribute("data-theme") === "ocean");
    assert.deepEqual(await tokens(), { background: "rgb(11, 30, 51)", warning: "#fcd34d" });
    assert.equal((await context.cookies(base)).find((c) => c.name === "silica-theme")?.value, "ocean");
    await context.close();
  });
});
