import { test } from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import * as App from "./out/test/browser/host_app.mjs";

const wait = (ms) => new Promise((ok) => setTimeout(ok, ms));

function setup(url = "http://localhost/") {
  const dom = new JSDOM(`<body><div id="a"></div><div id="b"></div></body>`, {
    url,
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
  delete globalThis.IntersectionObserver;
  return w;
}

function trackListeners(w) {
  const active = new Set();
  const proto = w.EventTarget.prototype;
  const add = proto.addEventListener;
  const remove = proto.removeEventListener;
  const capture = (options) => (typeof options === "boolean" ? options : !!(options && options.capture));
  const find = (target, type, listener, options) =>
    [...active].find((entry) => entry.target === target && entry.type === type && entry.listener === listener && entry.capture === capture(options));
  proto.addEventListener = function (type, listener, options) {
    if (/\/out\//.test(new Error().stack ?? "") && !find(this, type, listener, options)) active.add({ target: this, type, listener, capture: capture(options) });
    return add.call(this, type, listener, options);
  };
  proto.removeEventListener = function (type, listener, options) {
    const found = find(this, type, listener, options);
    if (found) active.delete(found);
    return remove.call(this, type, listener, options);
  };
  return {
    describe: () => [...active].map(({ target, type }) => `${target.nodeName ?? "window"} ${type}`).sort(),
    restore: () => Object.assign(proto, { addEventListener: add, removeEventListener: remove }),
  };
}

const inside = (root, name) => root.querySelector(`[data-test="${name}"]`);
const $ = (id) => document.getElementById(id);

function click(w, el) {
  el.dispatchEvent(new w.MouseEvent("click", { bubbles: true, cancelable: true, button: 0 }));
}

function key(w, target, key) {
  target.dispatchEvent(new w.KeyboardEvent("keydown", { key, bubbles: true, cancelable: true, composed: true }));
}

test("two apps in one document: a ref resolves in its own app", () => {
  const w = setup();
  App.mount_guest($("a"), "a");
  App.mount_guest($("b"), "b");
  click(w, inside($("b"), "focus"));
  assert.equal(document.activeElement, inside($("b"), "field"), "the focus goes to the field of app b");
  click(w, inside($("a"), "focus"));
  assert.equal(document.activeElement, inside($("a"), "field"), "the focus goes to the field of app a");
});

test("a stop removes every listener, timer, held resource, portal and node of the app", async () => {
  const w = setup();
  const listeners = trackListeners(w);
  try {
    const app = App.mount_guest($("a"), "a");
    assert.equal(App.lifecycle(app), "running");
    assert.ok($("a").hasAttribute("data-silica-ready"));
    click(w, inside($("a"), "toggle"));
    assert.equal(document.documentElement.style.overflow, "hidden", "the scroll lock runs");
    assert.ok(document.querySelector('[data-test="popup"]'), "the portal is painted");
    assert.ok(listeners.describe().includes("#document keydown"), "the key resource listens");
    App.stop(app);
    assert.equal(App.lifecycle(app), "stopped");
    assert.deepEqual(listeners.describe(), [], "no listener of the app is left");
    assert.equal($("a").childNodes.length, 0, "the nodes of the app are removed");
    assert.equal($("a").hasAttribute("data-silica-ready"), false);
    assert.equal(document.querySelector("[data-silica-portal]"), null, "the portal is removed");
    assert.equal(document.documentElement.style.overflow, "", "the scroll lock is released");
    const steps = App.guest_steps();
    key(w, document.body, "j");
    key(w, document.body, "Escape");
    await wait(70);
    assert.equal(App.guest_steps(), steps, "no key and no timer reaches the app");
    App.stop(app);
    App.set_label(app, "again");
    assert.equal(App.guest_steps(), steps, "a stopped app takes no event");
    assert.equal(App.lifecycle(app), "stopped");
  } finally {
    listeners.restore();
  }
});

test("a stop of one app leaves the portal, scroll lock and keys of the other app", () => {
  const w = setup();
  const a = App.mount_guest($("a"), "a");
  App.mount_guest($("b"), "b");
  click(w, inside($("a"), "toggle"));
  click(w, inside($("b"), "toggle"));
  assert.equal(document.querySelectorAll('[data-test="popup"]').length, 2);
  App.stop(a);
  assert.equal(document.querySelectorAll('[data-test="popup"]').length, 1, "the portal of b stays");
  assert.equal(document.documentElement.style.overflow, "hidden", "b still holds the scroll lock");
  key(w, document.body, "j");
  assert.equal(inside($("b"), "count").textContent, "1", "the key resource of b still runs");
});

test("the host gives the root new props", () => {
  setup();
  const app = App.mount_guest($("a"), "ada");
  assert.equal(inside($("a"), "label").textContent, "ada");
  const steps = App.guest_steps();
  App.set_label(app, "grace");
  assert.equal(inside($("a"), "label").textContent, "grace");
  assert.equal(App.guest_steps(), steps + 1, "one props step");
  App.set_label(app, "grace");
  assert.equal(App.guest_steps(), steps + 1, "equal props do nothing");
});

test("an app in a shadow root keeps its portal, keys, dismiss and focus in the shadow root", () => {
  const w = setup();
  const host = document.createElement("div");
  document.body.appendChild(host);
  const shadow = host.attachShadow({ mode: "open" });
  const root = document.createElement("div");
  shadow.appendChild(root);
  const app = App.mount_guest(root, "shadow");
  click(w, inside(root, "toggle"));
  assert.ok(shadow.querySelector('[data-test="popup"]'), "the portal is in the shadow root");
  assert.equal(document.querySelector("[data-silica-portal]"), null, "no portal in the document");
  key(w, inside(root, "toggle"), "j");
  assert.equal(inside(root, "count").textContent, "1", "a key in the shadow root reaches the key resource");
  click(w, inside(root, "focus"));
  assert.equal(shadow.activeElement, inside(root, "field"));
  key(w, inside(root, "field"), "Escape");
  assert.equal(shadow.querySelector('[data-test="popup"]'), null, "Escape closes the dismiss layer");
  App.stop(app);
  assert.equal(root.childNodes.length, 0);
  assert.equal(shadow.querySelector("[data-silica-portal]"), null);
});

test("an app in a detached element paints there and puts its portal in the top node", () => {
  const w = setup();
  const wrapper = document.createElement("div");
  const root = document.createElement("div");
  wrapper.appendChild(root);
  const app = App.mount_guest(root, "detached");
  assert.ok(inside(root, "toggle"), "the app painted into the detached element");
  click(w, inside(root, "toggle"));
  assert.ok(wrapper.querySelector(':scope > [data-silica-portal] [data-test="popup"]'), "the portal is in the top node");
  assert.equal(document.querySelector("[data-silica-portal]"), null);
  App.stop(app);
  assert.equal(wrapper.querySelector("[data-silica-portal]"), null);
});

test("title, cookie, document attribute and scroll lock go through the document, which counts the locks", () => {
  const w = setup();
  document.title = "host";
  App.clear_document_log();
  const a = App.mount_guest_on_logging_document($("a"), "a");
  const b = App.mount_guest_on_logging_document($("b"), "b");
  click(w, inside($("a"), "title"));
  assert.equal(document.title, "host", "the document of the host took the title");
  assert.equal(document.cookie, "");
  assert.equal(document.documentElement.hasAttribute("data-guest"), false);
  click(w, inside($("a"), "toggle"));
  click(w, inside($("b"), "toggle"));
  assert.equal(document.documentElement.style.overflow, "", "the document of the host did not lock the page");
  App.stop(a);
  App.stop(b);
  assert.deepEqual(App.document_lines(), ["title guest a", "cookie guest=a", "attribute data-guest=a", "lock", "unlock"]);
});

test("a stopped router app removes its announcer and its window listeners", async () => {
  const w = setup("http://localhost/home");
  const listeners = trackListeners(w);
  try {
    const app = App.mount_router_into($("a"));
    click(w, document.getElementById("to-about"));
    await wait(0);
    assert.ok(document.querySelector("[data-silica-announcer]"), "the announcer reads the title");
    App.stop(app);
    assert.equal(document.querySelector("[data-silica-announcer]"), null);
    assert.deepEqual(listeners.describe(), []);
  } finally {
    listeners.restore();
  }
});
