import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, type ChildProcess } from "node:child_process";
import { createServer } from "node:net";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type Page } from "playwright-core";

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

const open = async (slug: string): Promise<Page> => {
  const page = await browser.newPage({ reducedMotion: "reduce" });
  page.setDefaultTimeout(5000);
  await page.goto(`${base}/components/${slug}`);
  await page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
  await page.locator("[data-story=events]").evaluate((el) => el.scrollIntoView());
  return page;
};

const focusedId = (page: Page) => page.evaluate(() => (document.activeElement?.id || document.activeElement?.tagName) ?? "");
const events = (page: Page) => page.locator("#event-log li").allTextContents();
const htmlOverflow = (page: Page) =>
  page.evaluate(() => getComputedStyle(document.documentElement).overflow);

describe("components: dialog", () => {
  test("keyboard: Enter opens, focus moves in, Tab wraps, Escape closes and focus returns", async () => {
    const page = await open("dialog");
    const trigger = page.locator("#rename-trigger");
    assert.equal(await trigger.getAttribute("aria-haspopup"), "dialog");
    assert.equal(await trigger.getAttribute("aria-expanded"), "false");
    await trigger.focus();
    await page.keyboard.press("Enter");
    const dialog = page.getByRole("dialog", { name: "Rename" });
    await dialog.waitFor();
    assert.equal(await trigger.getAttribute("aria-expanded"), "true");
    assert.equal(await dialog.getAttribute("aria-modal"), "true");
    assert.equal(await focusedId(page), "rename-name", "focus moved to the first field");
    assert.equal(await htmlOverflow(page), "hidden", "the page does not scroll");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "case-trigger");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "rename-save");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "help-trigger");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "rename-close");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "rename-name", "Tab wraps to the first element");
    await page.keyboard.press("Shift+Tab");
    assert.equal(await focusedId(page), "rename-close", "Shift+Tab wraps to the last element");
    await page.keyboard.press("Escape");
    await dialog.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "rename-trigger", "focus returned to the trigger");
    assert.notEqual(await htmlOverflow(page), "hidden", "the page scrolls again");
    assert.deepEqual(await events(page), ["open trigger-press", "close escape-key"]);
    await page.close();
  });

  test("the page behind is inert; a select in the dialog opens in its own portal and works by keyboard", async () => {
    const page = await open("dialog");
    await tabTo(page, "rename-trigger");
    await page.keyboard.press("Enter");
    const dialog = page.getByRole("dialog", { name: "Rename" });
    await dialog.waitFor();
    assert.equal(await page.evaluate(() => document.getElementById("open-outside")?.closest("[inert]") != null), true, "the page is inert");
    const focusOutside = () =>
      page.evaluate(() => {
        const button = document.getElementById("open-outside");
        let focused = false;
        button?.addEventListener("focus", () => (focused = true), { once: true });
        button?.focus();
        return focused;
      });
    assert.equal(await focusOutside(), false, "the browser does not focus an inert element");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "case-trigger");
    await page.keyboard.press("ArrowDown");
    const list = page.locator("#case-popup");
    await list.waitFor();
    assert.equal(await list.evaluate((el) => el.closest("[inert]") === null), true, "the list is not inert");
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("Enter");
    await list.waitFor({ state: "detached" });
    assert.equal(await page.locator("#case-trigger").textContent(), "Upper case");
    assert.equal(await dialog.count(), 1, "the dialog stays open");
    await page.keyboard.press("Escape");
    await dialog.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "rename-trigger", "focus returned to the trigger");
    assert.equal(await page.evaluate(() => document.querySelectorAll("[inert]").length), 0, "nothing is inert after close");
    assert.equal(await focusOutside(), true, "after close it can take focus");
    await page.close();
  });

  test("pointer: a press inside keeps it open, a press on the backdrop closes it", async () => {
    const page = await open("dialog");
    await page.locator("#rename-trigger").click();
    const dialog = page.getByRole("dialog", { name: "Rename" });
    await dialog.waitFor();
    await page.locator("#rename-title").click();
    assert.equal(await dialog.count(), 1, "a press inside does not close it");
    await page.mouse.click(5, 5);
    await dialog.waitFor({ state: "detached" });
    assert.deepEqual(await events(page), ["open trigger-press", "close outside-press"]);
    await page.close();
  });

  test("content: typing and Enter save, the dialog closes and focus returns", async () => {
    const page = await open("dialog");
    await page.locator("#rename-trigger").click();
    await page.locator("#rename-name").fill("Grace");
    await page.keyboard.press("Enter");
    await page.getByRole("dialog").waitFor({ state: "detached" });
    assert.equal(await page.locator("#saved").textContent(), "Name: Grace");
    assert.equal(await focusedId(page), "rename-trigger");
    assert.deepEqual(await events(page), ["open trigger-press", "save", "close close-press"]);
    await page.close();
  });

  test("nested: Escape and presses reach the newest dialog only; focus moves between them", async () => {
    const page = await open("dialog");
    await page.locator("#rename-trigger").click();
    const rename = page.getByRole("dialog", { name: "Rename" });
    await rename.waitFor();
    await page.locator("#help-trigger").focus();
    await page.keyboard.press("Enter");
    const help = page.getByRole("dialog", { name: "Help" });
    await help.waitFor();
    assert.equal(await focusedId(page), "help-close", "the outer trap lets focus into the inner dialog");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "help-close", "Tab stays in the inner dialog");
    await page.locator("#help-title").click();
    assert.equal(await help.count(), 1, "a press in the inner dialog closes nothing");
    assert.equal(await rename.count(), 1);
    await page.keyboard.press("Escape");
    await help.waitFor({ state: "detached" });
    assert.equal(await rename.count(), 1, "Escape closed only the inner dialog");
    assert.equal(await focusedId(page), "help-trigger", "focus returned inside the outer dialog");
    await page.keyboard.press("Escape");
    await rename.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "rename-trigger");
    assert.deepEqual(await events(page), ["open trigger-press", "close escape-key"]);
    await page.close();
  });

  test("the parent opens it, and styles from silica-ui.css apply", async () => {
    const page = await open("dialog");
    await page.locator("#open-outside").click();
    const dialog = page.getByRole("dialog", { name: "Rename" });
    await dialog.waitFor();
    assert.deepEqual(await events(page), ["open from outside"], "no open change for the parent's own request");
    const style = await dialog.evaluate((el) => {
      const s = getComputedStyle(el);
      return { position: s.position, background: s.backgroundColor };
    });
    assert.equal(style.position, "fixed");
    assert.equal(style.background, "rgb(255, 255, 255)");
    const backdrop = await page
      .locator("[data-ui=dialog-backdrop]")
      .evaluate((el) => getComputedStyle(el).backgroundColor);
    assert.match(backdrop, /rgba\(0, 0, 0, 0\.4\)|oklab|color/);
    await page.close();
  });
});

const tabTo = async (page: Page, id: string): Promise<void> => {
  for (let i = 0; i < 100; i++) {
    await page.keyboard.press("Tab");
    if ((await focusedId(page)) === id) return;
  }
  throw new Error(`Tab never reached #${id}`);
};

const box = async (page: Page, selector: string) => {
  await page.evaluate(
    () => new Promise((done) => requestAnimationFrame(() => requestAnimationFrame(done))),
  );
  const b = await page.locator(selector).boundingBox();
  assert.ok(b, `${selector} has a box`);
  return b;
};

describe("components: popover", () => {
  test("keyboard: Tab to the trigger, Enter opens, focus moves in, Escape closes and focus returns", async () => {
    const page = await open("popover");
    const trigger = page.locator("#label-trigger");
    assert.equal(await trigger.getAttribute("aria-haspopup"), "dialog");
    assert.equal(await trigger.getAttribute("aria-expanded"), "false");
    await tabTo(page, "label-trigger");
    await page.keyboard.press("Enter");
    const popover = page.getByRole("dialog", { name: "Edit label" });
    await popover.waitFor();
    assert.equal(await trigger.getAttribute("aria-expanded"), "true");
    assert.equal(await trigger.getAttribute("aria-controls"), "label-popup");
    assert.equal(await popover.getAttribute("aria-modal"), null, "not modal");
    assert.equal(await focusedId(page), "label-popup", "focus moved into the popup");
    assert.notEqual(await htmlOverflow(page), "hidden", "the page still scrolls");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "label-name");
    await page.keyboard.press("Escape");
    await popover.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "label-trigger", "focus returned to the trigger");
    await page.keyboard.press("Space");
    await popover.waitFor();
    await page.keyboard.press("Escape");
    await popover.waitFor({ state: "detached" });
    assert.deepEqual(await events(page), [
      "open trigger-press",
      "close escape-key",
      "open trigger-press",
      "close escape-key",
    ]);
    await page.close();
  });

  test("pointer: a press inside keeps it, a press outside closes it, the trigger toggles it", async () => {
    const page = await open("popover");
    await page.locator("#label-trigger").click();
    const popover = page.getByRole("dialog", { name: "Edit label" });
    await popover.waitFor();
    await page.locator("#label-title").click();
    assert.equal(await popover.count(), 1, "a press inside does not close it");
    await page.locator("#saved").click();
    await popover.waitFor({ state: "detached" });
    assert.notEqual(await focusedId(page), "label-trigger", "an outside press does not move focus to the trigger");
    await page.locator("#label-trigger").click();
    await popover.waitFor();
    await page.locator("#label-trigger").click();
    await popover.waitFor({ state: "detached" });
    await page.locator("#label-trigger").click();
    await page.locator("#label-name").fill("Crash");
    await page.keyboard.press("Enter");
    await popover.waitFor({ state: "detached" });
    assert.equal(await page.locator("#saved").textContent(), "Label: Crash");
    assert.deepEqual(await events(page), [
      "open trigger-press",
      "close outside-press",
      "open trigger-press",
      "close trigger-press",
      "open trigger-press",
      "save",
      "close close-press",
    ]);
    await page.close();
  });

  test("position: the popup is next to the trigger, on the side from the props, and flips when there is no room", async () => {
    const page = await open("popover");
    await page.locator("#label-trigger").click();
    await page.locator("#label-popup[data-side]").waitFor();
    let t = await box(page, "#label-trigger");
    let p = await box(page, "#label-popup");
    assert.equal(await page.locator("#label-popup").getAttribute("data-side"), "bottom");
    assert.ok(Math.abs(p.y - (t.y + t.height + 4)) < 1, `below the trigger: ${p.y} vs ${t.y + t.height}`);
    assert.ok(Math.abs(p.x - t.x) < 1, "aligned at the start of the trigger");
    await page.keyboard.press("Escape");
    await page.locator("#info-trigger").click();
    await page.locator("#info-popup[data-side]").waitFor();
    t = await box(page, "#info-trigger");
    p = await box(page, "#info-popup");
    assert.equal(await page.locator("#info-popup").getAttribute("data-side"), "top");
    assert.ok(Math.abs(p.y + p.height - (t.y - 4)) < 1, `above the trigger: ${p.y + p.height} vs ${t.y}`);
    await page.keyboard.press("Escape");
    t = await box(page, "#label-trigger");
    await page.setViewportSize({ width: 1280, height: Math.ceil(t.y + t.height + 40) });
    await page.locator("#label-trigger").click();
    await page.locator("#label-popup[data-side]").waitFor();
    p = await box(page, "#label-popup");
    assert.equal(await page.locator("#label-popup").getAttribute("data-side"), "top", "no room below: it flips");
    t = await box(page, "#label-trigger");
    assert.ok(Math.abs(p.y + p.height - (t.y - 4)) < 1, `above the trigger: ${p.y + p.height} vs ${t.y}`);
    await page.close();
  });
  test("position: a top-side popup with no set width is above the trigger from its first placement", async () => {
    const page = await open("popover");
    await page.addStyleTag({ content: "#info-popup { width: auto !important; max-width: 12rem !important; }" });
    await page.evaluate(() => {
      const w = window as unknown as { firstBottom?: number };
      new MutationObserver((_, observer) => {
        const popup = document.getElementById("info-popup");
        if (popup?.hasAttribute("data-side")) {
          w.firstBottom = popup.getBoundingClientRect().bottom;
          observer.disconnect();
        }
      }).observe(document.body, { subtree: true, childList: true, attributes: true });
    });
    await page.locator("#info-trigger").click();
    await page.locator("#info-popup[data-side]").waitFor();
    const t = await box(page, "#info-trigger");
    const first = await page.evaluate(() => (window as unknown as { firstBottom?: number }).firstBottom);
    assert.ok(first !== undefined && Math.abs(first - (t.y - 4)) < 1, `first placement above the trigger: ${first} vs ${t.y - 4}`);
    await page.close();
  });
});

describe("components: tooltip", () => {
  test("keyboard: focus shows it at once, Escape hides it and focus stays on the trigger", async () => {
    const page = await open("tooltip");
    const trigger = page.locator("#save-trigger");
    assert.equal(await trigger.getAttribute("aria-describedby"), null);
    await tabTo(page, "save-trigger");
    const tip = page.getByRole("tooltip");
    await tip.waitFor();
    assert.equal(await tip.textContent(), "Save the draft");
    assert.equal(await trigger.getAttribute("aria-describedby"), "save-popup");
    assert.equal(await focusedId(page), "save-trigger", "focus does not move into the tooltip");
    await page.keyboard.press("Escape");
    await tip.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "save-trigger", "focus stays on the trigger");
    await page.keyboard.press("Tab");
    await page.getByRole("tooltip").waitFor();
    assert.equal(await focusedId(page), "help-trigger");
    assert.equal(await page.locator("#help-trigger").getAttribute("aria-describedby"), "help-popup");
    await page.keyboard.press("Shift+Tab");
    await page.locator("#help-popup").waitFor({ state: "detached" });
    await page.locator("#save-popup").waitFor();
    assert.deepEqual((await events(page)).slice(0, 4), [
      "save open trigger-focus",
      "save close escape-key",
      "help open trigger-focus",
      "help close trigger-focus",
    ]);
    await page.close();
  });

  test("hover: it shows after the delay and hides when the pointer leaves", async () => {
    const page = await open("tooltip");
    const t = await box(page, "#save-trigger");
    await page.mouse.move(t.x + t.width / 2, t.y + t.height / 2);
    const start = Date.now();
    const tip = page.locator("#save-popup");
    await page.waitForTimeout(300);
    assert.equal(await tip.count(), 0, "not shown before the delay");
    await tip.waitFor();
    assert.ok(Date.now() - start >= 550, `shown after the delay: ${Date.now() - start} ms`);
    await page.mouse.move(5, 5);
    await tip.waitFor({ state: "detached" });
    assert.deepEqual(await events(page), ["save open trigger-hover", "save close trigger-hover"]);
    await page.close();
  });

  test("hover: with a close delay, the pointer can move onto the popup and keep it open", async () => {
    const page = await open("tooltip");
    const t = await box(page, "#help-trigger");
    await page.mouse.move(t.x + t.width / 2, t.y + t.height / 2);
    const tip = page.locator("#help-popup");
    await tip.waitFor();
    const p = await box(page, "#help-popup");
    await page.mouse.move(p.x + p.width / 2, p.y + p.height / 2, { steps: 4 });
    await page.waitForTimeout(600);
    assert.equal(await tip.count(), 1, "the pointer on the popup keeps it open");
    await page.mouse.move(5, 5);
    await tip.waitFor({ state: "detached" });
    assert.deepEqual(await events(page), ["help open trigger-hover", "help close trigger-hover"]);
    await page.close();
  });

  test("position: the popup is next to the trigger on the side from the props", async () => {
    const page = await open("tooltip");
    await tabTo(page, "save-trigger");
    await page.locator("#save-popup[data-side]").waitFor();
    let t = await box(page, "#save-trigger");
    let p = await box(page, "#save-popup");
    assert.equal(await page.locator("#save-popup").getAttribute("data-side"), "top");
    assert.ok(Math.abs(p.y + p.height - (t.y - 4)) < 1, `above the trigger: ${p.y + p.height} vs ${t.y}`);
    assert.ok(Math.abs(p.x + p.width / 2 - (t.x + t.width / 2)) < 1, "centered on the trigger");
    await page.keyboard.press("Tab");
    await page.locator("#help-popup[data-side]").waitFor();
    t = await box(page, "#help-trigger");
    p = await box(page, "#help-popup");
    assert.equal(await page.locator("#help-popup").getAttribute("data-side"), "bottom");
    assert.ok(Math.abs(p.y - (t.y + t.height + 4)) < 1, `below the trigger: ${p.y} vs ${t.y + t.height}`);
    await page.close();
  });
});

const assertBelow = async (page: Page, trigger: string, popup: string) => {
  const t = await box(page, trigger);
  const p = await box(page, popup);
  assert.equal(await page.locator(popup).getAttribute("data-side"), "bottom");
  assert.ok(p.y >= t.y + t.height && p.y <= t.y + t.height + 12, `popup top ${p.y} is just below the trigger ${t.y + t.height}`);
  assert.ok(Math.abs(p.x - t.x) < 1, `popup left ${p.x} is aligned with the trigger ${t.x}`);
};

describe("components: menu", () => {
  test("keyboard: Enter opens once, arrows wrap and skip disabled, Home, End, a letter, Space chooses, focus returns", async () => {
    const page = await open("menu");
    const trigger = page.locator("#actions-trigger");
    assert.equal(await trigger.getAttribute("aria-haspopup"), "menu");
    assert.equal(await trigger.getAttribute("aria-expanded"), "false");
    await trigger.focus();
    await page.keyboard.press("Enter");
    const menu = page.getByRole("menu", { name: "Actions" });
    await menu.waitFor();
    assert.equal(await trigger.getAttribute("aria-expanded"), "true");
    assert.equal(await trigger.getAttribute("aria-controls"), "actions-popup");
    assert.equal(await focusedId(page), "actions-item-0");
    assert.deepEqual(await events(page), ["open trigger-press"], "Enter opened the menu only once and chose nothing");
    const steps: [string, string][] = [
      ["ArrowDown", "actions-item-2"],
      ["ArrowDown", "actions-item-3"],
      ["ArrowDown", "actions-item-0"],
      ["ArrowUp", "actions-item-3"],
      ["Home", "actions-item-0"],
      ["End", "actions-item-3"],
      ["a", "actions-item-2"],
    ];
    for (const [k, id] of steps) {
      await page.keyboard.press(k);
      assert.equal(await focusedId(page), id, `after ${k}`);
      assert.equal(await page.locator(`#${id}`).getAttribute("tabindex"), "0");
    }
    assert.equal(await page.locator("#actions-item-1").getAttribute("aria-disabled"), "true");
    await page.keyboard.press(" ");
    await menu.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "actions-trigger");
    await page.keyboard.press(" ");
    await menu.waitFor();
    assert.equal(await focusedId(page), "actions-item-0", "Space opened it once");
    await page.keyboard.press("Escape");
    await menu.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "actions-trigger");
    await page.keyboard.press("ArrowUp");
    await menu.waitFor();
    assert.equal(await focusedId(page), "actions-item-3", "ArrowUp opens on the last item");
    await page.keyboard.press("Enter");
    await menu.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "actions-trigger");
    assert.deepEqual(await events(page), [
      "open trigger-press",
      "select archive",
      "close item-press",
      "open trigger-press",
      "close escape-key",
      "open list-navigation",
      "select delete",
      "close item-press",
    ]);
    await page.close();
  });

  test("Tab and Shift+Tab close the menu and move focus on from the trigger", async () => {
    const page = await open("menu");
    const menu = page.getByRole("menu");
    await page.locator("#actions-trigger").focus();
    await page.keyboard.press("ArrowDown");
    await menu.waitFor();
    await page.keyboard.press("Tab");
    await menu.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "after");
    await page.locator("#actions-trigger").focus();
    await page.keyboard.press("ArrowDown");
    await menu.waitFor();
    await page.keyboard.press("Shift+Tab");
    await menu.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "before");
    assert.deepEqual(await events(page), [
      "open list-navigation",
      "close focus-out",
      "open list-navigation",
      "close focus-out",
    ]);
    await page.close();
  });

  test("pointer: hover highlights, a press chooses, a press outside closes", async () => {
    const page = await open("menu");
    const menu = page.getByRole("menu");
    await page.locator("#actions-trigger").click();
    await menu.waitFor();
    await page.locator("#actions-item-2").hover();
    assert.equal(await focusedId(page), "actions-item-2");
    assert.equal(await page.locator("#actions-item-2").getAttribute("data-highlighted"), "");
    await page.locator("#actions-item-1").click({ force: true });
    assert.equal(await menu.count(), 1, "a press on a disabled item does nothing");
    await page.locator("#actions-item-2").click();
    await menu.waitFor({ state: "detached" });
    await page.locator("#actions-trigger").click();
    await menu.waitFor();
    await page.mouse.click(5, 300);
    await menu.waitFor({ state: "detached" });
    assert.deepEqual(await events(page), [
      "open trigger-press",
      "select archive",
      "close item-press",
      "open trigger-press",
      "close outside-press",
    ]);
    await page.close();
  });

  test("the menu is in a portal, below the trigger", async () => {
    const page = await open("menu");
    await page.locator("#actions-trigger").click();
    await page.getByRole("menu").waitFor();
    assert.ok(
      await page.locator("#actions-popup").evaluate((el) => el.closest("[data-silica-portal]")?.parentElement === document.body),
    );
    await assertBelow(page, "#actions-trigger", "#actions-popup");
    await page.close();
  });
});

describe("components: select", () => {
  test("keyboard: Enter opens once, focus stays on the trigger, arrows do not wrap, a letter, Enter selects into the form", async () => {
    const page = await open("select");
    const trigger = page.locator("#status-trigger");
    assert.equal(await trigger.getAttribute("role"), "combobox");
    assert.equal(await trigger.getAttribute("aria-haspopup"), "listbox");
    assert.equal(await trigger.getAttribute("data-placeholder"), "");
    await trigger.focus();
    await page.keyboard.press("Enter");
    const list = page.getByRole("listbox", { name: "Status" });
    await list.waitFor();
    assert.deepEqual(await events(page), ["open trigger-press"], "Enter opened the list only once");
    assert.equal(await focusedId(page), "status-trigger");
    const active = () => trigger.getAttribute("aria-activedescendant");
    assert.equal(await active(), "status-option-0");
    const steps: [string, string][] = [
      ["ArrowDown", "status-option-1"],
      ["ArrowDown", "status-option-3"],
      ["End", "status-option-5"],
      ["ArrowDown", "status-option-5"],
      ["Home", "status-option-0"],
      ["ArrowUp", "status-option-0"],
      ["d", "status-option-4"],
    ];
    for (const [k, id] of steps) {
      await page.keyboard.press(k);
      assert.equal(await active(), id, `after ${k}`);
      assert.equal(await focusedId(page), "status-trigger");
    }
    await page.keyboard.press("Enter");
    await list.waitFor({ state: "detached" });
    assert.equal(await trigger.textContent(), "Done");
    assert.equal(await trigger.getAttribute("data-placeholder"), null);
    assert.equal(await page.locator("#issue-form input[type=hidden][name=status]").inputValue(), "done");
    assert.equal(await page.locator("#status-value").textContent(), "Status: done");
    assert.equal(await focusedId(page), "status-trigger");
    await page.keyboard.press(" ");
    await list.waitFor();
    assert.equal(await active(), "status-option-4", "Space opened it once, on the selected option");
    assert.equal(await page.locator("#status-option-4").getAttribute("aria-selected"), "true");
    await page.keyboard.press("ArrowUp");
    await page.keyboard.press("Escape");
    await list.waitFor({ state: "detached" });
    assert.equal(await trigger.textContent(), "Done", "Escape keeps the value");
    assert.equal(await focusedId(page), "status-trigger");
    assert.deepEqual(await events(page), [
      "open trigger-press",
      "value done",
      "close item-press",
      "open trigger-press",
      "close escape-key",
    ]);
    await page.close();
  });

  test("Tab selects the highlighted option, closes and moves focus on", async () => {
    const page = await open("select");
    const list = page.getByRole("listbox");
    await page.locator("#status-trigger").focus();
    await page.keyboard.press("ArrowDown");
    await list.waitFor();
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("Tab");
    await list.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "priority-trigger");
    assert.equal(await page.locator("#status-trigger").textContent(), "Todo");
    assert.equal(await page.locator("#issue-form input[name=status]").inputValue(), "todo");
    assert.deepEqual(await events(page), ["open list-navigation", "value todo", "close focus-out"]);
    await page.close();
  });

  test("pointer: a press on an option selects it, a press outside closes with no change, the parent sets the value", async () => {
    const page = await open("select");
    const list = page.getByRole("listbox");
    const trigger = page.locator("#status-trigger");
    await trigger.click();
    await list.waitFor();
    await page.locator("#status-option-3").hover();
    assert.equal(await trigger.getAttribute("aria-activedescendant"), "status-option-3");
    await page.locator("#status-option-2").click({ force: true });
    assert.equal(await list.count(), 1, "a press on a disabled option does nothing");
    await page.locator("#status-option-3").click();
    await list.waitFor({ state: "detached" });
    assert.equal(await trigger.textContent(), "In progress");
    assert.equal(await focusedId(page), "status-trigger");
    await trigger.click();
    await list.waitFor();
    await page.mouse.click(5, 400);
    await list.waitFor({ state: "detached" });
    assert.equal(await trigger.textContent(), "In progress");
    await page.locator("#set-done").click();
    assert.equal(await trigger.textContent(), "Done");
    assert.equal(await page.locator("#issue-form input[name=status]").inputValue(), "done");
    assert.deepEqual(await events(page), [
      "open trigger-press",
      "value progress",
      "close item-press",
      "open trigger-press",
      "close outside-press",
    ]);
    await page.close();
  });

  test("the list is in a portal, below the trigger, at least as wide", async () => {
    const page = await open("select");
    await page.locator("#status-trigger").click();
    await page.getByRole("listbox").waitFor();
    assert.ok(
      await page.locator("#status-popup").evaluate((el) => el.closest("[data-silica-portal]")?.parentElement === document.body),
    );
    await assertBelow(page, "#status-trigger", "#status-popup");
    const t = await box(page, "#status-trigger");
    const p = await box(page, "#status-popup");
    assert.ok(p.width >= t.width - 0.5, `list width ${p.width} >= trigger width ${t.width}`);
    await page.close();
  });
});

const options = (page: Page, listbox: string) => page.locator(`${listbox} [role=option]`).allTextContents();

describe("components: combobox", () => {
  test("keyboard: typing filters, arrows move aria-activedescendant, Enter chooses, focus stays in the input", async () => {
    const page = await open("combobox");
    const input = page.locator("#fruit-input");
    assert.equal(await input.getAttribute("role"), "combobox");
    assert.equal(await input.getAttribute("aria-expanded"), "false");
    await input.focus();
    await page.keyboard.type("ap");
    const listbox = page.getByRole("listbox", { name: "Fruit", exact: true });
    await listbox.waitFor();
    assert.deepEqual(await options(page, "#fruit-listbox"), ["Apple", "Apricot", "Grape"]);
    assert.equal(await input.getAttribute("aria-expanded"), "true");
    assert.equal(await input.getAttribute("aria-activedescendant"), null);
    await page.keyboard.press("ArrowDown");
    assert.equal(await input.getAttribute("aria-activedescendant"), "fruit-option-0");
    await page.keyboard.press("ArrowDown");
    assert.equal(await input.getAttribute("aria-activedescendant"), "fruit-option-1");
    assert.equal(await page.locator("#fruit-option-1").getAttribute("data-highlighted"), "");
    assert.equal(await focusedId(page), "fruit-input", "focus stays in the input");
    await page.keyboard.press("Enter");
    await listbox.waitFor({ state: "detached" });
    assert.equal(await input.inputValue(), "Apricot");
    assert.equal(await focusedId(page), "fruit-input");
    assert.deepEqual(await events(page), ["fruit apricot"]);
    await page.close();
  });

  test("Escape closes the list, then clears the text", async () => {
    const page = await open("combobox");
    const input = page.locator("#fruit-input");
    await input.focus();
    await page.keyboard.type("an");
    const listbox = page.locator("#fruit-listbox");
    await listbox.waitFor();
    await page.keyboard.press("Escape");
    await listbox.waitFor({ state: "detached" });
    assert.equal(await input.inputValue(), "an");
    await page.keyboard.press("Escape");
    assert.equal(await input.inputValue(), "");
    assert.equal(await focusedId(page), "fruit-input");
    await page.close();
  });

  test("the list sits under the input and scrolls the highlighted option into view", async () => {
    const page = await open("combobox");
    await page.locator("#fruit-input").focus();
    const scrollY = await page.evaluate(() => window.scrollY);
    await page.keyboard.press("ArrowUp");
    const popup = page.locator("#fruit-popup");
    await popup.waitFor();
    await page.waitForFunction(() => document.getElementById("fruit-popup")?.hasAttribute("data-side"));
    assert.equal(await popup.getAttribute("data-side"), "bottom");
    const field = await box(page, "#fruit-input");
    const list = await box(page, "#fruit-popup");
    assert.ok(Math.abs(list.x - field.x) < 1, `aligned at the start: ${list.x} vs ${field.x}`);
    assert.ok(list.y >= field.y + field.height && list.y <= field.y + field.height + 8, "just under the input");
    assert.ok(list.width >= field.width - 1, "at least as wide as the input");
    assert.equal(await page.locator("#fruit-input").getAttribute("aria-activedescendant"), "fruit-option-12");
    const last = await box(page, "#fruit-option-12");
    assert.ok(last.y >= list.y && last.y + last.height <= list.y + list.height + 1, "the last option is in view");
    assert.ok(await popup.evaluate((el) => el.scrollTop > 0), "the list scrolled");
    assert.equal(await page.evaluate(() => window.scrollY), scrollY, "the page did not scroll");
    await page.close();
  });

  test("pointer: a press on an option chooses it, a press outside closes the list", async () => {
    const page = await open("combobox");
    const input = page.locator("#fruit-input");
    await input.click();
    await page.keyboard.type("an");
    await page.locator("#fruit-option-9").click();
    assert.equal(await input.inputValue(), "Mango");
    assert.equal(await focusedId(page), "fruit-input");
    await page.keyboard.press("Alt+ArrowDown");
    const listbox = page.locator("#fruit-listbox");
    await listbox.waitFor();
    assert.equal(await page.locator("#fruit-option-9").getAttribute("aria-selected"), "true");
    await page.mouse.click(5, 400);
    await listbox.waitFor({ state: "detached" });
    assert.deepEqual(await events(page), ["fruit mango"]);
    await page.close();
  });

  test("keyboard: Home and End move to the ends, Tab and Shift+Tab close the list and keep the text", async () => {
    const page = await open("combobox");
    const input = page.locator("#fruit-input");
    const listbox = page.locator("#fruit-listbox");
    await tabTo(page, "fruit-input");
    await page.keyboard.type("p");
    await listbox.waitFor();
    await page.keyboard.press("ArrowDown");
    assert.equal(await input.getAttribute("aria-activedescendant"), "fruit-option-0");
    await page.keyboard.press("End");
    assert.equal(await input.getAttribute("aria-activedescendant"), "fruit-option-12");
    await page.keyboard.press("Home");
    assert.equal(await input.getAttribute("aria-activedescendant"), "fruit-option-0");
    await page.keyboard.press("Tab");
    await listbox.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "pick-input", "focus moved on");
    assert.equal(await input.inputValue(), "p", "Tab does not choose");
    await page.keyboard.press("Shift+Tab");
    assert.equal(await focusedId(page), "fruit-input");
    await page.keyboard.press("Alt+ArrowDown");
    await listbox.waitFor();
    await page.keyboard.press("Shift+Tab");
    await listbox.waitFor({ state: "detached" });
    assert.notEqual(await focusedId(page), "fruit-input");
    assert.deepEqual(await events(page), []);
    await page.close();
  });

  test("focus moved by a script out of the input and the list closes it; a press on an option still chooses", async () => {
    const page = await open("combobox");
    const input = page.locator("#fruit-input");
    const listbox = page.locator("#fruit-listbox");
    await input.click();
    await page.keyboard.type("an");
    await listbox.waitFor();
    await page.locator("#pick-input").focus();
    await listbox.waitFor({ state: "detached" });
    assert.equal(await input.inputValue(), "an");
    await input.click();
    await page.keyboard.press("Alt+ArrowDown");
    await listbox.waitFor();
    await page.locator("#fruit-option-2").click();
    assert.equal(await input.inputValue(), "Banana");
    assert.equal(await focusedId(page), "fruit-input");
    assert.deepEqual(await events(page), ["fruit banana"]);
    await page.close();
  });

  test("inline: the list is under the input and the first match is highlighted", async () => {
    const page = await open("combobox");
    const input = page.locator("#pick-input");
    assert.equal(await input.getAttribute("aria-activedescendant"), "pick-option-0");
    const field = await box(page, "#pick-input");
    const list = await box(page, "#pick-listbox");
    assert.ok(list.y >= field.y + field.height, "under the input");
    await input.focus();
    await page.keyboard.type("citrus");
    assert.deepEqual(await options(page, "#pick-listbox"), ["Lemon", "Orange"]);
    assert.equal(await input.getAttribute("aria-activedescendant"), "pick-option-8");
    await page.keyboard.press("ArrowDown");
    assert.equal(await input.getAttribute("aria-activedescendant"), "pick-option-10");
    await page.keyboard.press("Enter");
    assert.deepEqual(await events(page), ["pick orange"]);
    await page.close();
  });
});

describe("components: command menu", () => {
  test("Ctrl+J opens it with focus in the input, typing filters, Enter runs the first match and closes, focus returns", async () => {
    const page = await open("command-menu");
    await page.locator("#menu-open").focus();
    await page.keyboard.press("Control+j");
    const dialog = page.getByRole("dialog", { name: "Command menu" });
    await dialog.waitFor();
    assert.equal(await focusedId(page), "menu-search-input", "focus in the search input");
    assert.equal(await page.locator("#menu-search-input").inputValue(), "");
    await page.keyboard.type("new");
    assert.deepEqual(await options(page, "#menu-search-listbox"), ["New issue", "New project"]);
    assert.equal(
      await page.locator("#menu-search-input").getAttribute("aria-activedescendant"),
      "menu-search-option-1",
    );
    await page.keyboard.press("Enter");
    await dialog.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "menu-open", "focus returned");
    assert.deepEqual(await events(page), ["run new-issue", "close close-press"]);
    await page.close();
  });

  test("keyboard: Home and End move the highlight, Tab stays in the dialog", async () => {
    const page = await open("command-menu");
    await tabTo(page, "menu-open");
    await page.keyboard.press("Control+j");
    const dialog = page.getByRole("dialog", { name: "Command menu" });
    await dialog.waitFor();
    const input = page.locator("#menu-search-input");
    const ids = await page
      .locator("#menu-search-listbox [role=option]:not([aria-disabled])")
      .evaluateAll((els) => els.map((el) => el.id));
    assert.ok(ids.length > 2);
    await page.keyboard.press("End");
    assert.equal(await input.getAttribute("aria-activedescendant"), ids.at(-1));
    await page.keyboard.press("Home");
    assert.equal(await input.getAttribute("aria-activedescendant"), ids[0]);
    for (let i = 0; i < 4; i++) {
      await page.keyboard.press("Tab");
      assert.ok(await dialog.evaluate((el) => el.contains(document.activeElement)), "focus stays in the dialog");
    }
    assert.equal(await dialog.count(), 1, "Tab does not close it");
    await page.close();
  });

  test("Escape clears the text, then closes; each open starts empty; a press outside closes", async () => {
    const page = await open("command-menu");
    await page.locator("#menu-open").focus();
    await page.keyboard.press("Meta+j");
    const dialog = page.getByRole("dialog", { name: "Command menu" });
    await dialog.waitFor();
    await page.keyboard.type("zz");
    assert.equal(await page.locator("#menu-search-status").textContent(), "No command found");
    await page.keyboard.press("Escape");
    assert.equal(await page.locator("#menu-search-input").inputValue(), "");
    assert.equal(await dialog.count(), 1, "the first Escape only clears");
    await page.keyboard.press("Escape");
    await dialog.waitFor({ state: "detached" });
    assert.equal(await focusedId(page), "menu-open");
    await page.keyboard.press("Control+j");
    await dialog.waitFor();
    assert.equal(await page.locator("#menu-search-input").inputValue(), "", "a new open starts empty");
    await page.mouse.click(5, 5);
    await dialog.waitFor({ state: "detached" });
    assert.deepEqual(await events(page), ["close escape-key", "close outside-press"]);
    await page.close();
  });
});

const formEntries = (page: Page, id: string) =>
  page.evaluate((formId) => [...new FormData(document.getElementById(formId) as HTMLFormElement).entries()].map(([k, v]) => `${k}=${v}`), id);

describe("components: checkbox", () => {
  test("Space and a click toggle it, Enter does not; aria-checked and the form value follow", async () => {
    const page = await open("checkbox");
    const box = page.getByRole("checkbox", { name: "Accept the terms" });
    assert.equal(await box.getAttribute("aria-checked"), "false");
    assert.deepEqual(await formEntries(page, "terms-form"), []);
    await box.focus();
    await page.keyboard.press("Space");
    assert.equal(await box.getAttribute("aria-checked"), "true");
    assert.deepEqual(await formEntries(page, "terms-form"), ["terms=on"]);
    await page.keyboard.press("Enter");
    assert.equal(await box.getAttribute("aria-checked"), "true", "Enter does not toggle");
    await box.click();
    assert.equal(await box.getAttribute("aria-checked"), "false");
    assert.deepEqual(await formEntries(page, "terms-form"), []);
    assert.deepEqual(await events(page), ["checked true", "checked false"]);
    await page.close();
  });

  test("mixed shows aria-checked mixed and a click checks it; a disabled box ignores clicks", async () => {
    const page = await open("checkbox");
    await page.locator("#set-mixed").click();
    const box = page.locator("#terms");
    assert.equal(await box.getAttribute("aria-checked"), "mixed");
    assert.equal(await box.getAttribute("data-indeterminate"), "");
    await box.click();
    assert.equal(await box.getAttribute("aria-checked"), "true");
    const locked = page.getByRole("checkbox", { name: "Locked" });
    assert.equal(await locked.isDisabled(), true);
    await locked.click({ force: true });
    assert.equal(await locked.getAttribute("aria-checked"), "false");
    assert.deepEqual(await events(page), ["checked true"]);
    await page.close();
  });
});

describe("components: switch", () => {
  test("Space, Enter and a click toggle it; aria-checked and the form value follow", async () => {
    const page = await open("switch");
    const sw = page.getByRole("switch", { name: "Wi-Fi" });
    assert.equal(await sw.getAttribute("aria-checked"), "false");
    await sw.focus();
    await page.keyboard.press("Space");
    assert.equal(await sw.getAttribute("aria-checked"), "true");
    assert.deepEqual(await formEntries(page, "wifi-form"), ["wifi=on"]);
    await page.keyboard.press("Enter");
    assert.equal(await sw.getAttribute("aria-checked"), "false");
    assert.deepEqual(await formEntries(page, "wifi-form"), []);
    await sw.click();
    assert.equal(await sw.getAttribute("aria-checked"), "true");
    assert.equal(await page.locator("#wifi-thumb").getAttribute("data-checked"), "");
    assert.deepEqual(await events(page), ["checked true", "checked false", "checked true"]);
    await page.close();
  });
});

describe("components: tabs", () => {
  test("arrows move focus with wrap and skip the disabled tab, Home and End, Enter and Space select", async () => {
    const page = await open("tabs");
    const panel = (name: string) => page.getByRole("tabpanel", { name });
    assert.equal(await page.locator("#settings[role=tablist]").getAttribute("aria-orientation"), "horizontal");
    await page.keyboard.press("Tab");
    await page.keyboard.press("Tab");
    while ((await focusedId(page)) !== "settings-tab-account") await page.keyboard.press("Tab");
    await page.keyboard.press("ArrowRight");
    assert.equal(await focusedId(page), "settings-tab-billing");
    assert.equal(await page.locator("#settings-tab-account").getAttribute("aria-selected"), "true", "focus does not select");
    assert.equal(await panel("Account").isVisible(), true);
    await page.keyboard.press("ArrowRight");
    assert.equal(await focusedId(page), "settings-tab-danger", "the disabled tab is skipped");
    await page.keyboard.press("ArrowRight");
    assert.equal(await focusedId(page), "settings-tab-account", "wrap");
    await page.keyboard.press("ArrowLeft");
    assert.equal(await focusedId(page), "settings-tab-danger", "wrap back");
    await page.keyboard.press("Home");
    assert.equal(await focusedId(page), "settings-tab-account");
    await page.keyboard.press("End");
    assert.equal(await focusedId(page), "settings-tab-danger");
    await page.keyboard.press("Enter");
    assert.equal(await page.locator("#settings-tab-danger").getAttribute("aria-selected"), "true");
    assert.equal(await panel("Danger").isVisible(), true);
    assert.equal(await panel("Account").count(), 0, "a hidden panel is not in the accessibility tree");
    await page.keyboard.press("ArrowLeft");
    assert.equal(await focusedId(page), "settings-tab-billing");
    await page.keyboard.press("Space");
    assert.equal(await panel("Billing").isVisible(), true);
    assert.equal(await page.locator("#settings-tab-billing").getAttribute("tabindex"), "0");
    await page.keyboard.press("Tab");
    assert.equal(await focusedId(page), "settings-panel-billing", "Tab leaves the list for the panel");
    await page.keyboard.press("Shift+Tab");
    assert.equal(await focusedId(page), "settings-tab-billing", "the tab stop is the focused tab");
    assert.deepEqual(await events(page), ["value danger", "value billing"]);
    await page.close();
  });

  test("a click selects a tab and shows its panel", async () => {
    const page = await open("tabs");
    await page.getByRole("tab", { name: "Billing" }).click();
    assert.equal(await page.getByRole("tabpanel", { name: "Billing" }).isVisible(), true);
    assert.equal(await page.locator("#settings-panel-account").isHidden(), true);
    assert.equal(await page.locator("#settings-tab-billing").getAttribute("data-active"), "");
    assert.deepEqual(await events(page), ["value billing"]);
    await page.close();
  });
});

describe("components: toast", () => {
  test("a toast appears; the close button closes it; Escape in a toast closes it", async () => {
    const page = await open("toast");
    const region = page.getByRole("region", { name: "Notifications" });
    assert.equal(await region.getAttribute("aria-live"), "polite");
    await page.locator("#add-toast").click();
    await page.locator("#add-urgent").click();
    const saved = page.getByRole("dialog", { name: "Saved t1" });
    await saved.waitFor();
    assert.equal(await saved.getAttribute("aria-modal"), "false");
    const failed = page.getByRole("alertdialog", { name: "Failed t2" });
    await failed.waitFor();
    await saved.getByRole("button", { name: "Close" }).click();
    await saved.waitFor({ state: "detached" });
    await failed.focus();
    await page.keyboard.press("Escape");
    await failed.waitFor({ state: "detached" });
    assert.deepEqual(await events(page), ["add t1", "add t2", "close t1", "close t2"]);
    await page.close();
  });

  test("a toast closes by itself after the timeout; hovering pauses it", async () => {
    const page = await open("toast");
    await page.locator("#add-toast").click();
    const first = page.getByRole("dialog", { name: "Saved t1" });
    await first.waitFor();
    await first.waitFor({ state: "detached", timeout: 3000 });
    assert.deepEqual(await events(page), ["add t1", "close t1"]);
    await page.locator("#add-toast").click();
    const second = page.getByRole("dialog", { name: "Saved t2" });
    await second.hover();
    assert.equal(await page.locator("#toasts").getAttribute("data-paused"), "");
    await page.waitForTimeout(2500);
    assert.equal(await second.count(), 1, "hovering keeps it");
    await page.mouse.move(5, 5);
    assert.equal(await page.locator("#toasts").getAttribute("data-paused"), null);
    await second.waitFor({ state: "detached", timeout: 3000 });
    assert.deepEqual(await events(page), ["add t1", "close t1", "add t2", "close t2"]);
    await page.close();
  });
});

describe("components: first frame of a popup", () => {
  type Frame = { side: string | null; x: number; y: number; width: number; height: number };

  const firstFrame = async (page: Page, trigger: string, popup: string): Promise<{ first: Frame | null; final: Frame }> => {
    await page.evaluate((id) => {
      const w = window as unknown as { firstFrame?: Frame | null };
      w.firstFrame = undefined;
      document.addEventListener(
        "click",
        () =>
          requestAnimationFrame(() => {
            const p = document.getElementById(id);
            const r = p?.getBoundingClientRect();
            w.firstFrame = p && r ? { side: p.getAttribute("data-side"), x: r.x, y: r.y, width: r.width, height: r.height } : null;
          }),
        { capture: true, once: true },
      );
    }, popup);
    await page.locator(trigger).click();
    await page.locator(`#${popup}[data-side]`).waitFor();
    const b = await box(page, `#${popup}`);
    const side = await page.locator(`#${popup}`).getAttribute("data-side");
    const first = await page.evaluate(() => (window as unknown as { firstFrame?: Frame | null }).firstFrame ?? null);
    return { first, final: { side, ...b } };
  };

  const same = (label: string, first: Frame | null, final: Frame) => {
    assert.ok(first, `${label}: the popup is in the first frame after the press`);
    assert.equal(first.side, final.side, `${label}: the side is set in the first frame`);
    for (const k of ["x", "y", "width", "height"] as const)
      assert.ok(Math.abs(first[k] - final[k]) < 1, `${label}: ${k} in the first frame ${first[k]} vs later ${final[k]}`);
  };

  test("a popover is in its place in the first frame, on the bottom and on the top side", async () => {
    const page = await open("popover");
    let r = await firstFrame(page, "#label-trigger", "label-popup");
    same("bottom", r.first, r.final);
    await page.keyboard.press("Escape");
    await page.locator("#label-popup").waitFor({ state: "detached" });
    r = await firstFrame(page, "#info-trigger", "info-popup");
    assert.equal(r.final.side, "top");
    same("top", r.first, r.final);
    await page.keyboard.press("Escape");
    await page.locator("#info-popup").waitFor({ state: "detached" });
    await page.addStyleTag({ content: "#info-popup { width: auto !important; max-width: 12rem !important; }" });
    r = await firstFrame(page, "#info-trigger", "info-popup");
    same("top, no set width", r.first, r.final);
    await page.close();
  });

  test("a menu is in its place in the first frame", async () => {
    const page = await open("menu");
    const r = await firstFrame(page, "#actions-trigger", "actions-popup");
    same("menu", r.first, r.final);
    await page.close();
  });
});

describe("components: theme", () => {
  const white = "rgb(255, 255, 255)";
  const darkSurface = "rgb(17, 24, 39)";
  const black = "rgb(0, 0, 0)";
  const oceanSurface = "rgb(11, 30, 51)";

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

  type Options = { scheme: "light" | "dark"; cookie?: string; javaScript?: boolean; slug?: string };

  const load = async ({ scheme, cookie, javaScript = true, slug = "switch" }: Options) => {
    const context = await browser.newContext({ colorScheme: scheme, javaScriptEnabled: javaScript });
    if (cookie) await context.addCookies([{ name: "silica-theme", value: cookie, url: base }]);
    const page = await context.newPage();
    page.setDefaultTimeout(5000);
    await page.addInitScript(firstFrame);
    await page.goto(`${base}/components/${slug}`);
    if (javaScript) await page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
    return { context, page };
  };

  const bodyBackground = (page: Page) => page.evaluate(() => getComputedStyle(document.documentElement).backgroundColor);
  const first = (page: Page) =>
    page.waitForFunction(() => (window as any).__firstFrame).then(() => page.evaluate(() => (window as any).__firstFrame));

  for (const [scheme, cookie, theme, background] of [
    ["light", undefined, "system", white],
    ["dark", undefined, "system", darkSurface],
    ["light", "ocean", "ocean", oceanSurface],
    ["dark", "light", "light", white],
  ] as const) {
    test(`no flash: system ${scheme}, cookie ${cookie ?? "none"}: the first frame has the theme`, async () => {
      const { context, page } = await load({ scheme, cookie });
      assert.deepEqual(await first(page), { background, theme }, "the first frame");
      assert.equal(await bodyBackground(page), background, "after the start");
      const writes: (string | null)[] = await page.evaluate(() => (window as any).__themeChanges);
      assert.deepEqual(writes.filter((value) => value !== theme), [], "the client writes only the same data-theme");
      await context.close();
      const noScript = await load({ scheme, cookie, javaScript: false });
      assert.equal(await bodyBackground(noScript.page), background, "the same with no script at all");
      await noScript.context.close();
    });
  }

  test("switch: the attribute, the colours and the cookie change; a reload keeps the choice", async () => {
    const { context, page } = await load({ scheme: "light" });
    await page.locator("#theme-trigger").click();
    await page.getByRole("option", { name: "High contrast" }).click();
    await page.waitForFunction(() => document.documentElement.getAttribute("data-theme") === "high-contrast");
    assert.equal(await bodyBackground(page), black);
    assert.equal(await page.evaluate(() => getComputedStyle(document.documentElement).colorScheme), "dark");
    const cookies = await context.cookies(base);
    const stored = cookies.find((c) => c.name === "silica-theme");
    assert.equal(stored?.value, "high-contrast");
    assert.ok(stored && stored.expires > Date.now() / 1000 + 360 * 24 * 3600, "kept for a year");
    await page.reload();
    await page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
    assert.deepEqual(await first(page), { background: black, theme: "high-contrast" }, "the first frame after reload");
    assert.equal((await page.locator("#theme-trigger").textContent())?.trim(), "High contrast");
    await page.locator("#theme-trigger").click();
    await page.getByRole("option", { name: "System" }).click();
    await page.waitForFunction(() => document.documentElement.getAttribute("data-theme") === "system");
    assert.equal(await bodyBackground(page), white);
    await context.close();
  });

  test("system: the page follows a change of the system setting, live", async () => {
    const { context, page } = await load({ scheme: "light" });
    assert.equal(await bodyBackground(page), white);
    await page.emulateMedia({ colorScheme: "dark" });
    assert.equal(await bodyBackground(page), darkSurface);
    assert.equal(await page.evaluate(() => getComputedStyle(document.documentElement).colorScheme), "dark");
    await page.emulateMedia({ colorScheme: "light" });
    assert.equal(await bodyBackground(page), white);
    await context.close();
  });

  const luminance = (rgb: string): number => {
    const parts = (rgb.match(/\d+(\.\d+)?/g) ?? []).map(Number);
    assert.ok(parts.length === 3 || (parts.length === 4 && parts[3] === 1), `an opaque rgb colour: ${rgb}`);
    const [r = 0, g = 0, b = 0] = parts.map((v) => {
      const c = v / 255;
      return c <= 0.03928 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
    });
    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  };
  const ratio = (a: string, b: string): number => {
    const la = luminance(a);
    const lb = luminance(b);
    return (Math.max(la, lb) + 0.05) / (Math.min(la, lb) + 0.05);
  };

  test("nested scope: each sample uses its theme on a light page; text on surface has contrast 4.5 or more", async () => {
    const { context, page } = await load({ scheme: "light", slug: "theme-scope" });
    const expected: Record<string, string> = {
      light: white,
      dark: darkSurface,
      "high-contrast": black,
      ocean: oceanSurface,
    };
    for (const [name, background] of Object.entries(expected)) {
      const sample = page.locator(`#theme-${name}`);
      const colours = await sample.evaluate((el) => {
        const scope = el.parentElement as HTMLElement;
        const style = (e: Element) => getComputedStyle(e);
        return {
          scope: scope.getAttribute("data-theme"),
          background: style(scope).backgroundColor,
          text: style(el.querySelector("h2")!).color,
          muted: style(el.querySelector("p")!).color,
          danger: style(el.querySelector(".text-ui-danger")!).color,
          accent: style(el.querySelector("button")!).backgroundColor,
          accentText: style(el.querySelector("button")!).color,
        };
      });
      assert.equal(colours.scope, name);
      assert.equal(colours.background, background, `${name}: the scope background`);
      const pairs: [string, string, string][] = [
        ["text", colours.text, colours.background],
        ["muted", colours.muted, colours.background],
        ["danger", colours.danger, colours.background],
        ["accent text", colours.accentText, colours.accent],
      ];
      for (const [what, fg, bg] of pairs)
        assert.ok(ratio(fg, bg) >= 4.5, `${name}: ${what} ${fg} on ${bg} is ${ratio(fg, bg).toFixed(2)}`);
    }
    assert.equal(await bodyBackground(page), white, "the page keeps its own theme");
    await context.close();
  });

  test("nested scope: a popup opened in a scope uses the scope theme on a light page", async () => {
    const { context, page } = await load({ scheme: "light", slug: "theme-scope" });
    const expected: Record<string, [string, string]> = {
      light: [darkSurface, "light"],
      dark: ["rgb(243, 244, 246)", "dark"],
      "high-contrast": [white, "dark"],
      ocean: ["rgb(230, 241, 255)", "dark"],
    };
    for (const [name, [background, scheme]] of Object.entries(expected)) {
      await page.locator(`#theme-${name}-tip-trigger`).focus();
      const popup = page.locator(`#theme-${name}-tip-popup`);
      await popup.waitFor();
      const seen = await popup.evaluate((el) => ({
        theme: el.closest("[data-theme]")?.getAttribute("data-theme"),
        background: getComputedStyle(el).backgroundColor,
        scheme: getComputedStyle(el).colorScheme,
      }));
      assert.deepEqual(seen, { theme: name, background, scheme }, `${name}: the popup`);
      await page.keyboard.press("Escape");
      await popup.waitFor({ state: "detached" });
    }
    await context.close();
  });
});
