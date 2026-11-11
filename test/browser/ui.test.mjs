import { test } from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import * as Ui from "./out/test/browser/ui_app.mjs";

function setup() {
  const dom = new JSDOM(`<body><div id="root"></div></body>`, {
    url: "http://localhost/",
    pretendToBeVisual: true,
  });
  const w = dom.window;
  Object.assign(globalThis, {
    window: w,
    document: w.document,
    location: w.location,
    history: w.history,
  });
  delete globalThis.ResizeObserver;
  return w;
}

const $ = (id) => document.getElementById(id);
const active = () => document.activeElement?.id ?? "";

function click(w, el) {
  el.dispatchEvent(new w.MouseEvent("click", { bubbles: true, cancelable: true, button: 0 }));
}

function key(w, target, init) {
  const event = new w.KeyboardEvent("keydown", { bubbles: true, cancelable: true, ...init });
  target.dispatchEvent(event);
  return event;
}

function pointerdown(w, target) {
  target.dispatchEvent(new w.Event("pointerdown", { bubbles: true, cancelable: true }));
}

const events = () => [...document.querySelectorAll("#event-log li")].map((li) => li.textContent);

test("dialog: the trigger opens a popup in a portal, focus moves in, Escape closes it and focus returns", () => {
  const w = setup();
  Ui.mount_dialog("root");
  const trigger = $("rename-trigger");
  trigger.focus();
  click(w, trigger);
  const popup = $("rename-popup");
  assert.ok(popup, "the popup is painted");
  assert.ok(popup.closest("[data-silica-portal]").parentNode === document.body, "in a portal at the body");
  assert.equal(popup.getAttribute("role"), "dialog");
  assert.equal(trigger.getAttribute("aria-expanded"), "true");
  assert.equal(active(), "rename-name", "focus moved to the first field");
  assert.equal(document.documentElement.style.overflow, "hidden", "scroll lock");
  const escape = key(w, document.activeElement, { key: "Escape" });
  assert.ok(escape.defaultPrevented, "the layer took the Escape key");
  assert.equal($("rename-popup"), null, "closed");
  assert.equal(active(), "rename-trigger", "focus returned to the trigger");
  assert.equal(document.documentElement.style.overflow, "", "scroll lock released");
  assert.deepEqual(events(), ["open trigger-press", "close escape-key"]);
});

test("dialog: Tab on the last element wraps to the first, Shift+Tab on the first wraps to the last", () => {
  const w = setup();
  Ui.mount_dialog("root");
  click(w, $("rename-trigger"));
  $("rename-close").focus();
  const tab = key(w, $("rename-close"), { key: "Tab" });
  assert.ok(tab.defaultPrevented);
  assert.equal(active(), "rename-name");
  const back = key(w, $("rename-name"), { key: "Tab", shiftKey: true });
  assert.ok(back.defaultPrevented);
  assert.equal(active(), "rename-close");
});

test("dialog: a press inside keeps it open, a press outside closes it", () => {
  const w = setup();
  Ui.mount_dialog("root");
  click(w, $("rename-trigger"));
  pointerdown(w, $("rename-title"));
  assert.ok($("rename-popup"), "still open");
  pointerdown(w, document.body);
  assert.equal($("rename-popup"), null);
  assert.deepEqual(events(), ["open trigger-press", "close outside-press"]);
});

test("dialog: the page behind a modal dialog is inert; a popup opened from it in its own portal is not; nested dialogs mark the outer one", () => {
  const w = setup();
  const own = document.createElement("aside");
  own.setAttribute("inert", "");
  document.body.append(own);
  Ui.mount_dialog("root");
  const root = $("root");
  const portalOf = (id) => $(id).closest("[data-silica-portal]");
  click(w, $("rename-trigger"));
  assert.ok(root.hasAttribute("inert"), "the page is inert");
  assert.ok(!portalOf("rename-popup").hasAttribute("inert"), "the dialog is not");
  assert.ok(!$("rename-popup").closest("[inert]"));
  click(w, $("case-trigger"));
  const list = $("case-popup");
  assert.ok(list && portalOf("case-popup") !== portalOf("rename-popup"), "the select list is in its own portal");
  assert.equal(list.closest("[inert]"), null, "the select list is not inert");
  click(w, $("case-option-1"));
  assert.equal($("case-popup"), null);
  assert.equal($("case-trigger").textContent, "Upper case", "the select works");
  click(w, $("help-trigger"));
  assert.ok(portalOf("rename-popup").hasAttribute("inert"), "the outer dialog is inert behind the inner one");
  assert.equal($("help-popup").closest("[inert]"), null, "the inner dialog is not");
  key(w, document.activeElement, { key: "Escape" });
  assert.equal($("help-popup"), null);
  assert.ok(!portalOf("rename-popup").hasAttribute("inert"), "the outer dialog is not inert again");
  assert.ok(root.hasAttribute("inert"), "the page still is");
  key(w, document.activeElement, { key: "Escape" });
  assert.equal($("rename-popup"), null);
  assert.ok(!root.hasAttribute("inert"), "the page is not inert after close");
  assert.equal(active(), "rename-trigger", "focus returned to the trigger");
  assert.ok(own.hasAttribute("inert"), "an element that the page made inert stays inert");
  assert.deepEqual([...document.querySelectorAll("[inert]")], [own]);
  own.remove();
});

test("nested dialogs: the inner portal comes after the outer one, which keeps its nodes; Escape and presses reach the inner one only", () => {
  const w = setup();
  Ui.mount_dialog("root");
  click(w, $("rename-trigger"));
  const outer = $("rename-popup");
  const field = $("rename-name");
  click(w, $("help-trigger"));
  const inner = $("help-popup");
  assert.ok(inner, "the inner dialog is open");
  assert.ok($("rename-popup") === outer && $("rename-name") === field, "the outer popup kept its nodes");
  const portals = [...document.body.children].filter((n) => n.hasAttribute("data-silica-portal"));
  assert.equal(portals.length, 2);
  assert.ok(portals[0].contains(outer) && portals[1].contains(inner), "outer portal first, inner portal on top");
  assert.equal(active(), "help-close", "the outer trap let focus into the inner dialog");
  pointerdown(w, $("help-title"));
  assert.ok($("help-popup") && $("rename-popup"), "a press in the inner dialog closes nothing");
  key(w, document.activeElement, { key: "Escape" });
  assert.equal($("help-popup"), null, "Escape closed the inner dialog");
  assert.ok($("rename-popup"), "and not the outer one");
  assert.equal(active(), "help-trigger", "focus went to the inner trigger");
  click(w, $("help-trigger"));
  pointerdown(w, document.body);
  assert.equal($("help-popup"), null, "a press outside both closes both");
  assert.equal($("rename-popup"), null);
});

function pointer(w, type, target) {
  target.dispatchEvent(
    new w.MouseEvent(type, { bubbles: true, cancelable: true, relatedTarget: document.body }),
  );
}

const wait = (ms) => new Promise((done) => setTimeout(done, ms));

test("popover: the trigger opens a non-modal popup in a portal, focus moves in, Escape closes it and focus returns", () => {
  const w = setup();
  Ui.mount_popover("root");
  const trigger = $("label-trigger");
  assert.equal(trigger.getAttribute("aria-haspopup"), "dialog");
  trigger.focus();
  click(w, trigger);
  const popup = $("label-popup");
  assert.ok(popup, "the popup is painted");
  assert.ok(popup.closest("[data-silica-portal]").parentNode === document.body, "in a portal at the body");
  assert.equal(popup.getAttribute("role"), "dialog");
  assert.equal(popup.getAttribute("aria-modal"), null, "not modal");
  assert.equal(popup.getAttribute("aria-labelledby"), "label-title");
  assert.equal(trigger.getAttribute("aria-expanded"), "true");
  assert.equal(trigger.getAttribute("aria-controls"), "label-popup");
  assert.equal(active(), "label-popup", "focus moved to the popup");
  assert.equal(document.documentElement.style.overflow, "", "no scroll lock");
  const tab = key(w, $("label-name"), { key: "Tab" });
  assert.equal(tab.defaultPrevented, false, "no focus trap");
  const escape = key(w, document.activeElement, { key: "Escape" });
  assert.ok(escape.defaultPrevented, "the layer took the Escape key");
  assert.equal($("label-popup"), null, "closed");
  assert.equal(active(), "label-trigger", "focus returned to the trigger");
  assert.deepEqual(events(), ["open trigger-press", "close escape-key"]);
});

test("popover: a press outside closes it and keeps focus; the trigger toggles it", () => {
  const w = setup();
  Ui.mount_popover("root");
  click(w, $("label-trigger"));
  pointerdown(w, $("label-title"));
  assert.ok($("label-popup"), "a press inside keeps it open");
  pointerdown(w, document.body);
  assert.equal($("label-popup"), null, "a press outside closes it");
  assert.notEqual(active(), "label-trigger", "focus does not move to the trigger");
  click(w, $("label-trigger"));
  click(w, $("label-trigger"));
  assert.equal($("label-popup"), null, "a second press closes it");
  assert.deepEqual(events(), [
    "open trigger-press",
    "close outside-press",
    "open trigger-press",
    "close trigger-press",
  ]);
});

test("popover: the content saves and closes it; the close part closes it", () => {
  const w = setup();
  Ui.mount_popover("root");
  click(w, $("label-trigger"));
  const field = $("label-name");
  field.value = "Crash";
  field.dispatchEvent(new w.Event("input", { bubbles: true }));
  $("label-name").form.dispatchEvent(new w.Event("submit", { bubbles: true, cancelable: true }));
  assert.equal($("label-popup"), null);
  assert.equal($("saved").textContent, "Label: Crash");
  assert.equal(active(), "label-trigger");
  click(w, $("label-trigger"));
  click(w, $("label-close"));
  assert.equal($("label-popup"), null);
  assert.deepEqual(events(), [
    "open trigger-press",
    "save",
    "close close-press",
    "open trigger-press",
    "close close-press",
  ]);
});

test("tooltip: focus shows it at once and describes the trigger, Escape and blur hide it", () => {
  const w = setup();
  Ui.mount_tooltip("root");
  const trigger = $("save-trigger");
  assert.equal(trigger.getAttribute("aria-describedby"), null);
  trigger.focus();
  const popup = $("save-popup");
  assert.ok(popup, "shown");
  assert.ok(popup.closest("[data-silica-portal]").parentNode === document.body, "in a portal at the body");
  assert.equal(popup.getAttribute("role"), "tooltip");
  assert.equal(popup.textContent, "Save the draft");
  assert.equal(popup.getAttribute("tabindex"), null, "not focusable");
  assert.equal(trigger.getAttribute("aria-describedby"), "save-popup");
  assert.equal(active(), "save-trigger", "focus stays on the trigger");
  const escape = key(w, trigger, { key: "Escape" });
  assert.ok(escape.defaultPrevented);
  assert.equal($("save-popup"), null, "Escape hides it");
  assert.equal(active(), "save-trigger");
  trigger.blur();
  trigger.focus();
  assert.ok($("save-popup"), "focus shows it again");
  trigger.blur();
  assert.equal($("save-popup"), null, "blur hides it");
  assert.deepEqual(events(), [
    "save open trigger-focus",
    "save close escape-key",
    "save open trigger-focus",
    "save close trigger-focus",
  ]);
});

test("tooltip: hover shows it after the delay; the popup keeps it open while it closes", async () => {
  const w = setup();
  Ui.mount_tooltip("root");
  const trigger = $("help-trigger");
  pointer(w, "pointerover", trigger);
  assert.equal($("help-popup"), null, "not before the delay");
  await wait(150);
  assert.equal($("help-popup"), null, "not after half the delay");
  await wait(250);
  assert.ok($("help-popup"), "shown after the delay");
  pointer(w, "pointerout", trigger);
  assert.ok($("help-popup"), "still shown during the close delay");
  pointer(w, "pointerover", $("help-popup"));
  await wait(400);
  assert.ok($("help-popup"), "the pointer on the popup keeps it");
  pointer(w, "pointerout", $("help-popup"));
  await wait(400);
  assert.equal($("help-popup"), null, "hidden after the close delay");
  assert.deepEqual(events(), ["help open trigger-hover", "help close trigger-hover"]);
});

test("anchor: the renderer observes the size of the trigger and the popup", () => {
  const w = setup();
  const observed = [];
  globalThis.ResizeObserver = class {
    observe(node) {
      observed.push(node.id);
    }
    disconnect() {}
  };
  try {
    Ui.mount_popover("root");
    click(w, $("label-trigger"));
    assert.ok($("label-popup"), "the popup is painted");
    assert.deepEqual(observed, ["label-trigger", "label-popup"]);
    const popup = $("label-popup");
    assert.equal(popup.getAttribute("data-side"), "bottom");
    assert.equal(popup.style.position, "fixed");
    assert.equal(popup.style.top, "4px", "jsdom has no layout: 4 pixels under an empty trigger");
    assert.equal(popup.style.left, "8px", "moved inside the 8 pixel padding");
    assert.equal(popup.style.getPropertyValue("--anchor-height"), "0px");
  } finally {
    delete globalThis.ResizeObserver;
  }
});

function pointerover(w, target) {
  target.dispatchEvent(new w.MouseEvent("pointerover", { bubbles: true, cancelable: true }));
}

const inPortal = (el) => el.closest("[data-silica-portal]")?.parentNode === document.body;

test("menu: Enter opens on the first item, arrows and a letter move focus, Enter chooses and focus returns", () => {
  const w = setup();
  Ui.mount_menu("root");
  const trigger = $("actions-trigger");
  trigger.focus();
  const enter = key(w, trigger, { key: "Enter" });
  assert.ok(enter.defaultPrevented, "no click from the native button");
  const popup = $("actions-popup");
  assert.ok(popup && inPortal(popup), "the menu is in a portal at the body");
  assert.equal(popup.getAttribute("role"), "menu");
  assert.equal(trigger.getAttribute("aria-controls"), "actions-popup");
  assert.equal(active(), "actions-item-0");
  assert.equal($("actions-item-0").getAttribute("tabindex"), "0");
  assert.equal($("actions-item-2").getAttribute("tabindex"), "-1");
  key(w, document.activeElement, { key: "ArrowDown" });
  assert.equal(active(), "actions-item-2", "the disabled item is skipped");
  key(w, document.activeElement, { key: "d" });
  assert.equal(active(), "actions-item-3");
  assert.ok($("actions-item-3").hasAttribute("data-highlighted"));
  const choose = key(w, document.activeElement, { key: "Enter" });
  assert.ok(choose.defaultPrevented);
  assert.equal($("actions-popup"), null, "closed");
  assert.equal(active(), "actions-trigger");
  assert.deepEqual(events(), ["open trigger-press", "select delete", "close item-press"]);
});

test("menu: a click opens it, the pointer highlights an item, a click chooses it", () => {
  const w = setup();
  Ui.mount_menu("root");
  click(w, $("actions-trigger"));
  assert.equal(active(), "actions-popup", "focus on the menu, no item highlighted");
  pointerover(w, $("actions-item-2"));
  assert.equal(active(), "actions-item-2");
  pointerover(w, $("actions-item-1"));
  assert.equal(active(), "actions-item-2", "a disabled item is not highlighted");
  click(w, $("actions-item-1"));
  assert.ok($("actions-popup"), "a click on a disabled item does nothing");
  click(w, $("actions-item-0"));
  assert.equal($("actions-popup"), null);
  assert.deepEqual(events(), ["open trigger-press", "select edit", "close item-press"]);
});

test("menu: Tab closes without prevent, Escape closes and focuses the trigger, an outside press closes", () => {
  const w = setup();
  Ui.mount_menu("root");
  key(w, $("actions-trigger"), { key: "ArrowUp" });
  assert.equal(active(), "actions-item-3", "ArrowUp opens on the last item");
  const tab = key(w, document.activeElement, { key: "Tab" });
  assert.ok(!tab.defaultPrevented, "the browser moves focus on");
  assert.equal($("actions-popup"), null);
  assert.equal(active(), "actions-trigger");
  key(w, $("actions-trigger"), { key: "ArrowDown" });
  key(w, document.activeElement, { key: "Escape" });
  assert.equal($("actions-popup"), null);
  assert.equal(active(), "actions-trigger");
  click(w, $("actions-trigger"));
  pointerdown(w, $("actions-item-0"));
  assert.ok($("actions-popup"), "a press inside keeps it open");
  pointerdown(w, document.body);
  assert.equal($("actions-popup"), null);
  assert.deepEqual(events(), [
    "open list-navigation",
    "close focus-out",
    "open list-navigation",
    "close escape-key",
    "open trigger-press",
    "close outside-press",
  ]);
});

test("select: Enter opens with focus on the trigger, arrows move the active descendant, Enter selects into the form", () => {
  const w = setup();
  Ui.mount_select("root");
  const trigger = $("status-trigger");
  assert.equal(trigger.getAttribute("role"), "combobox");
  assert.ok(trigger.hasAttribute("data-placeholder"));
  assert.equal(trigger.textContent, "Pick a status");
  trigger.focus();
  const enter = key(w, trigger, { key: "Enter" });
  assert.ok(enter.defaultPrevented, "no click from the native button");
  const list = $("status-popup");
  assert.ok(list && inPortal(list), "the list is in a portal at the body");
  assert.equal(list.getAttribute("role"), "listbox");
  assert.equal(active(), "status-trigger", "focus stays on the trigger");
  assert.equal(trigger.getAttribute("aria-activedescendant"), "status-option-0");
  key(w, trigger, { key: "ArrowDown" });
  key(w, trigger, { key: "ArrowDown" });
  assert.equal(trigger.getAttribute("aria-activedescendant"), "status-option-3", "the disabled option is skipped");
  assert.ok($("status-option-3").hasAttribute("data-highlighted"));
  key(w, trigger, { key: "Enter" });
  assert.equal($("status-popup"), null);
  assert.equal(trigger.textContent, "In progress");
  assert.ok(!trigger.hasAttribute("data-placeholder"));
  assert.equal(new w.FormData($("issue-form")).get("status"), "progress");
  assert.equal(active(), "status-trigger");
  key(w, trigger, { key: "ArrowDown" });
  assert.equal(trigger.getAttribute("aria-activedescendant"), "status-option-3", "opens on the selected option");
  assert.equal($("status-option-3").getAttribute("aria-selected"), "true");
  assert.deepEqual(events(), ["open trigger-press", "value progress", "close item-press", "open list-navigation"]);
});

test("select: Tab selects without prevent, Escape and an outside press keep the value, a click selects", () => {
  const w = setup();
  Ui.mount_select("root");
  const trigger = $("status-trigger");
  key(w, trigger, { key: "ArrowDown" });
  key(w, trigger, { key: "ArrowDown" });
  const tab = key(w, trigger, { key: "Tab" });
  assert.ok(!tab.defaultPrevented, "the browser moves focus on");
  assert.equal($("status-popup"), null);
  assert.equal(trigger.textContent, "Todo");
  key(w, trigger, { key: "ArrowDown" });
  key(w, trigger, { key: "End" });
  key(w, trigger, { key: "Escape" });
  assert.equal(trigger.textContent, "Todo", "Escape keeps the value");
  click(w, trigger);
  pointerdown(w, document.body);
  assert.equal($("status-popup"), null);
  click(w, trigger);
  pointerover(w, $("status-option-4"));
  assert.equal(trigger.getAttribute("aria-activedescendant"), "status-option-4");
  click(w, $("status-option-4"));
  assert.equal(trigger.textContent, "Done");
  assert.equal(new w.FormData($("issue-form")).get("status"), "done");
  assert.deepEqual(events(), [
    "open list-navigation",
    "value todo",
    "close focus-out",
    "open list-navigation",
    "close escape-key",
    "open trigger-press",
    "close outside-press",
    "open trigger-press",
    "value done",
    "close item-press",
  ]);
});

test("anchor: with a ResizeObserver, the trigger and the popup are observed; the popup gets a valid position and data-side", () => {
  const w = setup();
  const observed = [];
  let disconnected = 0;
  globalThis.ResizeObserver = class {
    observe(node) {
      observed.push(node.id);
    }
    disconnect() {
      disconnected += 1;
    }
  };
  try {
    Ui.mount_menu("root");
    click(w, $("actions-trigger"));
    assert.ok($("actions-popup"), "the menu opened with no error");
    assert.deepEqual(observed, ["actions-trigger", "actions-popup"]);
    const popup = $("actions-popup");
    assert.equal(popup.getAttribute("data-side"), "bottom");
    assert.equal(popup.style.position, "fixed");
    const length = /^\d+(\.\d+)?px$/;
    assert.match(popup.style.left, length, "a valid CSS length");
    assert.match(popup.style.top, length, "a valid CSS length");
    assert.match(popup.style.getPropertyValue("--anchor-height"), length);
    assert.equal(parseFloat(popup.style.left), 8, "inside the viewport padding");
    assert.equal(parseFloat(popup.style.top), 4, "below the trigger, 4 pixels away");
    key(w, document.activeElement, { key: "Escape" });
    assert.equal(disconnected, 1, "closing stops the observer");
  } finally {
    delete globalThis.ResizeObserver;
  }
});

function type(w, input, text) {
  input.value = text;
  input.dispatchEvent(new w.Event("input", { bubbles: true }));
}

const optionTexts = (listbox) => [...listbox.querySelectorAll("[role=option]")].map((o) => o.textContent);

test("combobox: typing filters into a list in a portal, arrows move aria-activedescendant, Enter chooses, focus stays", () => {
  const w = setup();
  Ui.mount_combobox("root");
  const input = $("fruit-input");
  input.focus();
  assert.equal(input.getAttribute("role"), "combobox");
  assert.equal(input.getAttribute("aria-expanded"), "false");
  assert.equal($("fruit-listbox"), null);
  type(w, input, "ap");
  const listbox = $("fruit-listbox");
  assert.ok(listbox.closest("[data-silica-portal]").parentNode === document.body, "in a portal at the body");
  assert.deepEqual(optionTexts(listbox), ["Apple", "Apricot", "Grape"]);
  assert.equal(input.getAttribute("aria-expanded"), "true");
  assert.equal(input.getAttribute("aria-controls"), "fruit-listbox");
  assert.equal(input.hasAttribute("aria-activedescendant"), false);
  const down = key(w, input, { key: "ArrowDown" });
  assert.ok(down.defaultPrevented);
  assert.equal(input.getAttribute("aria-activedescendant"), "fruit-option-0");
  key(w, input, { key: "ArrowDown" });
  assert.equal(input.getAttribute("aria-activedescendant"), "fruit-option-1");
  assert.ok($("fruit-option-1").hasAttribute("data-highlighted"));
  assert.equal(active(), "fruit-input", "focus stays in the input");
  const enter = key(w, input, { key: "Enter" });
  assert.ok(enter.defaultPrevented);
  assert.equal(input.value, "Apricot");
  assert.equal($("fruit-listbox"), null, "closed");
  assert.equal(input.hasAttribute("aria-activedescendant"), false);
  assert.equal(active(), "fruit-input");
  assert.deepEqual(events(), ["fruit apricot"]);
});

test("combobox: Escape closes, then clears, then is not used; a press on an option chooses it; a press outside closes", () => {
  const w = setup();
  Ui.mount_combobox("root");
  const input = $("fruit-input");
  input.focus();
  type(w, input, "an");
  assert.ok(key(w, input, { key: "Escape" }).defaultPrevented);
  assert.equal($("fruit-listbox"), null);
  assert.equal(input.value, "an");
  assert.ok(key(w, input, { key: "Escape" }).defaultPrevented);
  assert.equal(input.value, "");
  assert.equal(key(w, input, { key: "Escape" }).defaultPrevented, false, "an Escape it does not use is not prevented");
  type(w, input, "an");
  assert.deepEqual(optionTexts($("fruit-listbox")), ["Banana", "Mango", "Orange"]);
  pointerdown(w, $("fruit-option-9"));
  click(w, $("fruit-option-9"));
  assert.equal(input.value, "Mango");
  assert.equal(active(), "fruit-input");
  key(w, input, { key: "ArrowDown", altKey: true });
  assert.ok($("fruit-listbox"), "Alt+ArrowDown opens");
  assert.equal(input.hasAttribute("aria-activedescendant"), false, "and does not move");
  pointerdown(w, document.body);
  assert.equal($("fruit-listbox"), null);
  assert.deepEqual(events(), ["fruit mango"]);
});

test("combobox: Tab closes without prevent and keeps the text; focus moving out closes; Home and End move to the ends", () => {
  const w = setup();
  Ui.mount_combobox("root");
  const input = $("fruit-input");
  input.focus();
  type(w, input, "a");
  key(w, input, { key: "ArrowDown" });
  const tab = key(w, input, { key: "Tab" });
  assert.equal(tab.defaultPrevented, false, "the browser moves focus on");
  assert.equal($("fruit-listbox"), null, "Tab closes");
  assert.equal(input.value, "a", "Tab does not choose");
  key(w, input, { key: "ArrowDown" });
  const enabled = [...$("fruit-listbox").querySelectorAll("[role=option]:not([aria-disabled])")].map((o) => o.id);
  assert.ok(enabled.length > 2);
  const end = key(w, input, { key: "End" });
  assert.ok(end.defaultPrevented);
  assert.equal(input.getAttribute("aria-activedescendant"), enabled.at(-1));
  key(w, input, { key: "Home" });
  assert.equal(input.getAttribute("aria-activedescendant"), enabled[0]);
  input.focus();
  assert.ok($("fruit-listbox"), "focus inside keeps it open");
  $("pick-input").focus();
  assert.equal($("fruit-listbox"), null, "focus outside closes");
  assert.equal(input.value, "a");
  assert.deepEqual(events(), []);
});

test("combobox inline: the list is in place and the first match is highlighted", () => {
  const w = setup();
  Ui.mount_combobox("root");
  const input = $("pick-input");
  const listbox = $("pick-listbox");
  assert.ok(listbox && !listbox.closest("[data-silica-portal]"), "in place");
  assert.equal(input.getAttribute("aria-activedescendant"), "pick-option-0");
  type(w, input, "citrus");
  assert.deepEqual(optionTexts($("pick-listbox")), ["Lemon", "Orange"]);
  assert.equal(input.getAttribute("aria-activedescendant"), "pick-option-8");
  key(w, input, { key: "Enter" });
  assert.deepEqual(events(), ["pick lemon"]);
});

test("command menu: Ctrl+J opens it with focus in the input, typing filters, Enter runs and closes, focus returns", () => {
  const w = setup();
  Ui.mount_command_menu("root");
  const opener = $("menu-open");
  opener.focus();
  const shortcut = key(w, opener, { key: "j", ctrlKey: true });
  assert.ok(shortcut.defaultPrevented, "the shortcut is taken");
  const popup = $("menu-popup");
  assert.equal(popup.getAttribute("role"), "dialog");
  assert.equal(active(), "menu-search-input", "focus in the search input");
  const input = $("menu-search-input");
  assert.equal(input.value, "");
  assert.equal(input.getAttribute("aria-activedescendant"), "menu-search-option-0");
  type(w, input, "new");
  assert.deepEqual(optionTexts($("menu-search-listbox")), ["New issue", "New project"]);
  key(w, input, { key: "Enter" });
  assert.equal($("menu-popup"), null, "closed");
  assert.equal(active(), "menu-open", "focus returned");
  assert.deepEqual(events(), ["run new-issue", "close close-press"]);
});

test("command menu: an Escape with text clears it, the next Escape reaches the dialog and closes it", () => {
  const w = setup();
  Ui.mount_command_menu("root");
  key(w, document.body, { key: "j", metaKey: true });
  const input = $("menu-search-input");
  type(w, input, "zz");
  assert.equal($("menu-search-status").textContent, "No command found");
  key(w, input, { key: "Escape" });
  assert.ok($("menu-popup"), "still open");
  assert.equal(input.value, "");
  key(w, input, { key: "Escape" });
  assert.equal($("menu-popup"), null, "closed");
  assert.deepEqual(events(), ["close escape-key"]);
  key(w, document.body, { key: "j", ctrlKey: true });
  assert.equal($("menu-search-input").value, "", "a new open starts empty");
});

test("anchor: the renderer writes a valid position on the popup and observes the size of both elements", () => {
  const w = setup();
  const observed = [];
  globalThis.ResizeObserver = class {
    observe(node) {
      observed.push(node.id);
    }
    disconnect() {}
  };
  w.ResizeObserver = globalThis.ResizeObserver;
  try {
    Ui.mount_combobox("root");
    const input = $("fruit-input");
    input.focus();
    key(w, input, { key: "ArrowDown" });
    const popup = $("fruit-popup");
    assert.equal(popup.style.position, "fixed");
    assert.equal(popup.style.left, "8px", "jsdom has no layout: the popup keeps the viewport padding");
    assert.equal(popup.style.top, "4px");
    assert.equal(popup.getAttribute("data-side"), "bottom");
    assert.deepEqual(observed, ["fruit-input", "fruit-popup"]);
  } finally {
    delete globalThis.ResizeObserver;
  }
});

const formData = (w, id) => [...new w.FormData($(id)).entries()];

test("checkbox: a click toggles it and the form value; Enter does not; mixed then a click checks it", () => {
  const w = setup();
  Ui.mount_checkbox("root");
  const box = $("terms");
  assert.equal(box.getAttribute("role"), "checkbox");
  assert.equal(box.getAttribute("aria-checked"), "false");
  assert.deepEqual(formData(w, "terms-form"), []);
  click(w, box);
  assert.equal($("terms").getAttribute("aria-checked"), "true");
  assert.ok($("terms").hasAttribute("data-checked"));
  assert.deepEqual(formData(w, "terms-form"), [["terms", "on"]]);
  const enter = key(w, $("terms"), { key: "Enter" });
  assert.ok(enter.defaultPrevented, "Enter is taken so the native click does not happen");
  assert.equal($("terms").getAttribute("aria-checked"), "true");
  click(w, $("set-mixed"));
  assert.equal($("terms").getAttribute("aria-checked"), "mixed");
  assert.ok($("terms").hasAttribute("data-indeterminate"));
  click(w, $("terms"));
  assert.equal($("terms").getAttribute("aria-checked"), "true");
  assert.ok($("locked").disabled, "a disabled checkbox is a disabled button");
  assert.deepEqual(events(), ["checked true", "checked true"]);
});

test("switch: a click toggles it and the form value", () => {
  const w = setup();
  Ui.mount_switch("root");
  const sw = $("wifi");
  assert.equal(sw.getAttribute("role"), "switch");
  assert.equal(sw.getAttribute("aria-checked"), "false");
  assert.ok(sw.querySelector("[data-ui=switch-thumb]").hasAttribute("data-unchecked"));
  click(w, sw);
  assert.equal($("wifi").getAttribute("aria-checked"), "true");
  assert.ok($("wifi-thumb").hasAttribute("data-checked"));
  assert.deepEqual(formData(w, "wifi-form"), [["wifi", "on"]]);
  click(w, $("wifi"));
  assert.equal($("wifi").getAttribute("aria-checked"), "false");
  assert.deepEqual(formData(w, "wifi-form"), []);
  assert.deepEqual(events(), ["checked true", "checked false"]);
});

test("tabs: arrows move focus and the tab stop, skip disabled tabs and wrap; a click selects and shows the panel", () => {
  const w = setup();
  Ui.mount_tabs("root");
  const tab = (v) => $(`settings-tab-${v}`);
  const panel = (v) => $(`settings-panel-${v}`);
  assert.equal($("settings").getAttribute("role"), "tablist");
  assert.equal(tab("account").getAttribute("tabindex"), "0");
  assert.equal(tab("billing").getAttribute("tabindex"), "-1");
  assert.equal(panel("account").hidden, false);
  assert.equal(panel("billing").hidden, true);
  tab("account").focus();
  const right = key(w, tab("account"), { key: "ArrowRight" });
  assert.ok(right.defaultPrevented);
  assert.equal(active(), "settings-tab-billing");
  assert.equal(tab("billing").getAttribute("tabindex"), "0");
  assert.equal(tab("account").getAttribute("tabindex"), "-1");
  assert.equal(tab("account").getAttribute("aria-selected"), "true", "focus does not select");
  key(w, tab("billing"), { key: "ArrowRight" });
  assert.equal(active(), "settings-tab-danger", "the disabled tab is skipped");
  key(w, tab("danger"), { key: "ArrowRight" });
  assert.equal(active(), "settings-tab-account", "wrap");
  key(w, tab("account"), { key: "End" });
  assert.equal(active(), "settings-tab-danger");
  key(w, tab("danger"), { key: "Home" });
  assert.equal(active(), "settings-tab-account");
  click(w, tab("billing"));
  assert.equal(tab("billing").getAttribute("aria-selected"), "true");
  assert.ok(tab("billing").hasAttribute("data-active"));
  assert.equal(panel("billing").hidden, false);
  assert.equal(panel("account").hidden, true);
  assert.equal(panel("billing").getAttribute("aria-labelledby"), "settings-tab-billing");
  assert.deepEqual(events(), ["value billing"]);
});

test("toast: the button adds a toast; the close button and Escape close it; pointer in the viewport pauses", () => {
  const w = setup();
  Ui.mount_toast("root");
  const region = $("toasts");
  assert.equal(region.getAttribute("role"), "region");
  assert.equal(region.getAttribute("aria-label"), "Notifications");
  click(w, $("add-toast"));
  click(w, $("add-urgent"));
  const first = $("toasts-toast-t1");
  assert.equal(first.getAttribute("role"), "dialog");
  assert.equal(first.getAttribute("aria-modal"), "false");
  assert.equal($("toasts-toast-t2").getAttribute("role"), "alertdialog");
  assert.equal(document.getElementById(first.getAttribute("aria-labelledby")).textContent, "Saved t1");
  click(w, $("toasts-toast-t1-close"));
  assert.equal($("toasts-toast-t1"), null);
  const escape = key(w, $("toasts-toast-t2"), { key: "Escape" });
  assert.ok(escape.defaultPrevented);
  assert.equal($("toasts-toast-t2"), null);
  click(w, $("add-toast"));
  $("toasts-toast-t3").dispatchEvent(new w.Event("pointerover", { bubbles: true }));
  assert.ok($("toasts").hasAttribute("data-paused"));
  $("toasts-toast-t3").dispatchEvent(new w.Event("pointerout", { bubbles: true }));
  assert.ok(!$("toasts").hasAttribute("data-paused"));
  assert.deepEqual(events(), ["add t1", "add t2", "close t1", "close t2", "add t3"]);
});

test("toast: the oldest toast closes by itself after the timeout", async () => {
  const w = setup();
  Ui.mount_toast("root");
  click(w, $("add-toast"));
  await new Promise((r) => setTimeout(r, 1000));
  assert.ok($("toasts-toast-t1"), "not yet");
  await new Promise((r) => setTimeout(r, 900));
  assert.equal($("toasts-toast-t1"), null);
  assert.deepEqual(events(), ["add t1", "close t1"]);
});
