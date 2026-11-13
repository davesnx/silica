import { test } from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";

const bundles = { a: new URL("./hot_a.mjs", import.meta.url).href, b: new URL("./hot_b.mjs", import.meta.url).href };
let copies = 0;
const load = (name) => import(`${bundles[name]}?copy=${++copies}`);
const wait = (ms) => new Promise((ok) => setTimeout(ok, ms));
const $ = (id) => document.getElementById(id);

function setup() {
  const dom = new JSDOM(`<body><div id="root"></div></body>`, { url: "http://localhost/", pretendToBeVisual: true });
  const w = dom.window;
  Object.assign(globalThis, {
    window: w,
    document: w.document,
    location: w.location,
    history: w.history,
    CustomEvent: w.CustomEvent,
    addEventListener: w.addEventListener.bind(w),
    removeEventListener: w.removeEventListener.bind(w),
    dispatchEvent: w.dispatchEvent.bind(w),
  });
  delete globalThis.ResizeObserver;
  delete globalThis.IntersectionObserver;
  return w;
}

function trackListeners(w) {
  const active = new Map();
  const proto = w.EventTarget.prototype;
  const add = proto.addEventListener;
  const remove = proto.removeEventListener;
  const key = (target, type, listener, options) => {
    const capture = typeof options === "boolean" ? options : !!(options && options.capture);
    return [target, type, listener, capture];
  };
  const find = (entry) =>
    [...active.keys()].find((other) => other.every((part, index) => part === entry[index]));
  proto.addEventListener = function (type, listener, options) {
    const entry = key(this, type, listener, options);
    if (/hot_[ab]\.mjs/.test(new Error().stack ?? "") && !find(entry)) active.set(entry, true);
    return add.call(this, type, listener, options);
  };
  proto.removeEventListener = function (type, listener, options) {
    const found = find(key(this, type, listener, options));
    if (found) active.delete(found);
    return remove.call(this, type, listener, options);
  };
  return {
    describe: () => [...active.keys()].map(([target, type]) => `${target.nodeName ?? "window"} ${type}`).sort(),
  };
}

function trackTimers() {
  const pending = new Set();
  const set = globalThis.setTimeout;
  const clear = globalThis.clearTimeout;
  globalThis.setTimeout = (run, ms, ...rest) => {
    const id = set(() => {
      pending.delete(id);
      run(...rest);
    }, ms);
    if (/hot_[ab]\.mjs/.test(new Error().stack ?? "")) pending.add(id);
    return id;
  };
  globalThis.clearTimeout = (id) => {
    pending.delete(id);
    clear(id);
  };
  return { count: () => pending.size, restore: () => Object.assign(globalThis, { setTimeout: set, clearTimeout: clear }) };
}

function click(w, element) {
  element.dispatchEvent(new w.MouseEvent("click", { bubbles: true, cancelable: true, button: 0 }));
}

function type(w, element, text) {
  element.value = text;
  element.dispatchEvent(new w.Event("input", { bubbles: true }));
}

function resumed(w) {
  return new Promise((ok) => {
    const on = (event) => {
      w.removeEventListener("silica-resumed", on);
      ok(JSON.parse(event.detail));
    };
    w.addEventListener("silica-resumed", on);
  });
}

async function swap(w, name) {
  const report = resumed(w);
  globalThis.__silica_handoff = globalThis.__silica_dev.suspend();
  const next = await load(name);
  next.mount("root");
  return report;
}

test("a hot reload keeps state through codecs, DOM nodes and focus, restarts a snapshot that does not decode or has no codec, and leaves no listener or timer", async () => {
  const w = setup();
  const listeners = trackListeners(w);
  const timers = trackTimers();
  try {
    const first = await load("a");
    const before = listeners.describe();
    first.mount("root");
    click(w, $("bump"));
    click(w, $("bump"));
    type(w, $("draft"), "abc");
    click(w, $("note"));
    assert.equal($("note").textContent, "shown");
    assert.equal($("bump").textContent, "2");
    assert.equal($("echo").textContent, "abc");
    const nodes = { bump: $("bump"), label: $("label"), draft: $("draft"), labelText: $("label").firstChild };
    $("draft").focus();
    click(w, $("start"));
    assert.equal(timers.count(), 1, "the counter owns a timer");
    const mounted = listeners.describe();
    assert.ok(mounted.length > before.length, "the app added listeners");

    const report = resumed(w);
    const handoff = globalThis.__silica_dev.suspend();
    assert.deepEqual(listeners.describe(), before, "suspend removes every listener the app added");
    assert.equal(timers.count(), 0, "suspend cancels every timer");
    click(w, $("bump"));
    assert.equal($("bump").textContent, "2", "the old code no longer handles input");
    globalThis.__silica_handoff = handoff;
    (await load("b")).mount("root");
    const outcome = await report;

    assert.deepEqual([...outcome.kept].sort(), ["router", "router/0:counter"]);
    assert.deepEqual(
      outcome.restarted.map(({ path, machine }) => ({ path, machine })),
      [
        { path: "router/1:draft", machine: "draft" },
        { path: "router/2:note", machine: "note" },
      ],
    );
    assert.match(outcome.restarted[0].reason, /^its snapshot does not decode: /, "the renamed constructor does not decode");
    assert.match(outcome.restarted[1].reason, /^the machine "note" has no registered codecs/, "the note has no codec");
    assert.equal($("label").textContent, "Counter, edited", "the new view shows");
    assert.equal($("bump").textContent, "2", "the counter kept its state");
    assert.equal($("bump"), nodes.bump, "the button is the same DOM node");
    assert.equal($("label"), nodes.label, "the label is the same DOM node");
    assert.equal($("label").firstChild, nodes.labelText, "the text node is the same");
    assert.equal(document.activeElement, nodes.draft, "the focused input keeps the focus");
    assert.equal($("echo").textContent, "", "the draft machine started again");
    assert.equal($("note").textContent, "hidden", "the note machine started again");
    assert.deepEqual(listeners.describe(), mounted, "the new code added the same listeners");
    assert.equal(timers.count(), 1, "the timer of the kept state started again");
    await wait(80);
    assert.equal($("bump").textContent, "102", "the timer ran to its end once");
    click(w, $("bump"));
    assert.equal($("bump").textContent, "103", "the new code handles input once");
    keydown(w, "k");
    assert.equal($("echo").textContent, "!", "a document key resource runs once");

    const second = await swap(w, "a");
    assert.deepEqual(second.restarted.map((entry) => entry.path), ["router/1:draft"]);
    assert.ok(second.kept.includes("router/2:note"), "the note at its start snapshot is kept with no codec");
    assert.equal($("bump").textContent, "103");
    assert.equal($("label").textContent, "Counter");
    assert.equal($("echo").textContent, "", "the draft type changed back, so it started again");
    keydown(w, "k");
    assert.equal($("echo").textContent, "!");
    assert.deepEqual(listeners.describe(), mounted, "a second swap leaves the same listeners");
    const third = await swap(w, "a");
    assert.deepEqual(third.restarted, [], "the same code keeps every machine");
    assert.deepEqual([...third.kept].sort(), ["router", "router/0:counter", "router/1:draft", "router/2:note"]);
    assert.deepEqual(listeners.describe(), mounted, "a third swap leaves the same listeners");
    assert.equal(timers.count(), 0, "no timer is left");
    click(w, $("bump"));
    assert.equal($("bump").textContent, "104", "input runs once after three swaps");
    keydown(w, "k");
    assert.equal($("echo").textContent, "!!", "the draft kept its state, and the key resource runs once after three swaps");
  } finally {
    timers.restore();
  }
});

function keydown(w, key) {
  document.body.dispatchEvent(new w.KeyboardEvent("keydown", { key, bubbles: true, cancelable: true }));
}
