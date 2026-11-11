import { test } from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import * as App from "./out/test/browser/host_app.mjs";

const SVG = "http://www.w3.org/2000/svg";
const XLINK = "http://www.w3.org/1999/xlink";
const XHTML = "http://www.w3.org/1999/xhtml";

function setup(html = "", { url = "http://localhost/" } = {}) {
  const dom = new JSDOM(
    `<body><div id="root">${html}</div><div id="modal-root"></div></body>`,
    { url, pretendToBeVisual: true },
  );
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

const $ = (id) => document.getElementById(id);
const text = (id) => $(id).textContent;
const wait = (ms) => new Promise((r) => setTimeout(r, ms));

async function until(fn, label) {
  for (let i = 0; i < 200; i++) {
    if (fn()) return;
    await wait(5);
  }
  assert.fail("timeout: " + label);
}

function click(w, el) {
  el.dispatchEvent(
    new w.MouseEvent("click", { bubbles: true, cancelable: true, button: 0 }),
  );
}

function keydown(w, target, init) {
  const event = new w.KeyboardEvent("keydown", {
    bubbles: true,
    cancelable: true,
    ...init,
  });
  target.dispatchEvent(event);
  return event;
}

function pointerdown(w, target) {
  const event = new w.Event("pointerdown", { bubbles: true, cancelable: true });
  target.dispatchEvent(event);
  return event;
}

function captureWarnings() {
  const seen = [];
  const original = console.warn;
  console.warn = (m) => seen.push(String(m));
  return { seen, restore: () => (console.warn = original) };
}

function silenceErrors() {
  const original = console.error;
  const seen = [];
  console.error = (m) => seen.push(String(m));
  return { seen, restore: () => (console.error = original) };
}

const rootElements = () => [...document.querySelectorAll("#root *")];

test("hydration keeps the server DOM nodes", () => {
  const html = App.hydrate_html();
  assert.match(html, /a<!-- -->b/, "the server writes the marker");
  const w = setup(html);
  const before = rootElements();
  const observer = new w.MutationObserver(() => {});
  observer.observe(document.getElementById("root"), {
    childList: true,
    subtree: true,
  });
  App.mount_hydrate(false, "root");
  const records = observer.takeRecords();
  const after = rootElements();
  assert.equal(after.length, before.length);
  after.forEach((node, i) => assert.ok(node === before[i], `node ${i} (${node.tagName}) is the same`));
  const touched = records.flatMap((r) => [...r.removedNodes, ...r.addedNodes]).filter((n) => n.nodeType === 1);
  assert.equal(touched.length, 0, "no element was added or removed");
  assert.equal($("pair").childNodes.length, 2, "two texts, no marker");
  assert.ok([...$("pair").childNodes].every((n) => n.nodeType === 3));
  assert.equal($("pair").textContent, "ab");
  assert.ok($("icon") instanceof w.SVGElement, "server SVG is still an SVG element");
  const inc = $("inc");
  click(w, inc);
  assert.equal(text("inc"), "count 1");
  assert.ok($("inc") === inc, "a later update keeps the node too");
  const field = $("field");
  field.value = "typed";
  field.dispatchEvent(new w.Event("input", { bubbles: true }));
  assert.equal(text("typed"), "typed");
});

test("an input focused before hydration keeps focus", () => {
  const w = setup(App.hydrate_html());
  const field = $("field");
  field.focus();
  assert.ok(document.activeElement === field);
  App.mount_hydrate(false, "root");
  assert.ok($("field") === field);
  assert.ok(document.activeElement === field, "focus survived the mount");
  assert.equal(w.document.activeElement.id, "field");
});

test("text typed before hydration stays in the field and reaches the machine", () => {
  const w = setup(App.hydrate_html());
  const field = $("field");
  field.focus();
  field.value = "early";
  App.mount_hydrate(false, "root");
  assert.ok($("field") === field);
  assert.equal(field.value, "early", "the typed text survived the first paint");
  assert.equal(text("typed"), "early", "the machine got the typed text");
  click(w, $("inc"));
  assert.equal(field.value, "early", "a later paint keeps it");
});

test("a field the user did not change takes the client value at hydration", () => {
  const w = setup(App.hydrate_html().replace('value=""', 'value="server"'));
  App.mount_hydrate(false, "root");
  assert.equal($("field").value, "", "the client value wins over the server value");
  assert.equal(text("typed"), "");
  click(w, $("inc"));
  assert.equal(text("inc"), "count 1");
});

test("a structure mismatch clears the root and renders again", () => {
  const html = App.hydrate_html().replace('<h1 id="title">Hello</h1>', "");
  const w = setup(html);
  const before = rootElements();
  const warnings = captureWarnings();
  try {
    App.mount_hydrate(false, "root");
  } finally {
    warnings.restore();
  }
  assert.ok(warnings.seen.some((m) => /hydration mismatch/.test(m)), warnings.seen.join("|"));
  assert.equal(text("title"), "Hello", "the view is complete");
  assert.ok(!rootElements().some((n) => before.includes(n)), "no old node was kept");
  click(w, $("inc"));
  assert.equal(text("inc"), "count 1");
});

test("a tag mismatch clears the root and renders again", () => {
  const html = App.hydrate_html().replace('<h1 id="title">Hello</h1>', '<h2 id="title">Hello</h2>');
  setup(html);
  const warnings = captureWarnings();
  try {
    App.mount_hydrate(false, "root");
  } finally {
    warnings.restore();
  }
  assert.equal($("title").tagName, "H1");
});

test("in dev mode a mismatch throws, after the error page shows", () => {
  const html = App.hydrate_html().replace('<h1 id="title">Hello</h1>', "");
  setup(html);
  const errors = silenceErrors();
  try {
    assert.throws(() => App.mount_mismatch(true, "root"));
  } finally {
    errors.restore();
  }
  const page = document.querySelector("[data-silica-error]");
  assert.ok(page, "error page");
  assert.match(page.textContent, /hydration mismatch/, "dev: the page says what differed");
  assert.ok(App.reported().some((m) => /hydration mismatch/.test(m)), "on_error got it");
});

test("text and attribute differences take the client value", () => {
  const html = App.hydrate_html().replace("Hello", "Hola").replace('class="a"', 'class="b"');
  setup(html);
  const before = rootElements();
  const warnings = captureWarnings();
  try {
    App.mount_hydrate(true, "root");
  } finally {
    warnings.restore();
  }
  assert.equal(text("title"), "Hello");
  assert.equal($("app").getAttribute("class"), "a");
  assert.ok(rootElements().every((n, i) => n === before[i]), "all nodes kept");
  assert.ok(warnings.seen.some((m) => /differs/.test(m)), "dev logs the difference: " + warnings.seen.join("|"));
});

test("a server attribute that the view does not have is removed", () => {
  const html = App.hydrate_html().replace('id="title"', 'id="title" data-extra="1"');
  setup(html);
  App.mount_hydrate(false, "root");
  assert.equal($("title").hasAttribute("data-extra"), false);
});

function stubFetch(handler) {
  const calls = [];
  globalThis.fetch = (url, init) => {
    calls.push({ url, body: init.body });
    return Promise.resolve({
      status: 200,
      text: () => Promise.resolve(handler(url, init.body)),
    });
  };
  return calls;
}

test("settle answers are replayed: no second call, same nodes", async () => {
  const html = App.lookup_html();
  assert.match(html, /ready 8/);
  setup(html);
  document.body.insertAdjacentHTML("beforeend", App.lookup_answers());
  assert.ok($("silica-answers"), "the table is in the page");
  const calls = stubFetch((_, body) => String(2 * Number(body)));
  const before = rootElements();
  App.mount_lookup("root");
  assert.equal(calls.length, 0, "the call did not run");
  assert.equal(text("answer"), "ready 8");
  assert.ok(rootElements().every((n, i) => n === before[i]), "the first paint adopted the HTML");
  click(window, $("again"));
  await until(() => calls.length === 1, "a call that is not in the table runs");
  assert.equal(calls[0].body, "8");
  await until(() => text("answer") === "ready 16", "answer");
  assert.equal(calls.length, 1);
});

test("without a table the call runs", async () => {
  setup("");
  const calls = stubFetch((_, body) => String(2 * Number(body)));
  App.mount_lookup("root");
  assert.equal(text("answer"), "loading");
  await until(() => text("answer") === "ready 8", "answer");
  assert.equal(calls.length, 1);
  assert.equal(calls[0].body, "4");
});

test("an answer is used once", async () => {
  setup(App.lookup_html());
  document.body.insertAdjacentHTML("beforeend", App.lookup_answers());
  const calls = stubFetch((_, body) => String(2 * Number(body)));
  App.mount_lookup("root");
  assert.equal(calls.length, 0);
  click(window, $("again"));
  await until(() => text("answer") === "ready 16", "second");
  click(window, $("again"));
  await until(() => text("answer") === "ready 32", "third");
  assert.deepEqual(calls.map((c) => c.body), ["8", "16"]);
});

test("SVG elements get the SVG namespace; xlink goes through setAttributeNS", () => {
  const w = setup("");
  App.mount_svg("root");
  const svg = $("svg");
  assert.equal(svg.namespaceURI, SVG);
  assert.equal(svg.constructor.name, "SVGSVGElement");
  assert.equal(svg.getAttribute("viewBox"), "0 0 20 20", "viewBox keeps its case");
  assert.equal($("path").namespaceURI, SVG);
  const use = $("use");
  assert.equal(use.namespaceURI, SVG);
  assert.equal(use.getAttributeNS(XLINK, "href"), "#a");
  assert.equal(use.getAttributeNode("xlink:href").namespaceURI, XLINK, "the attribute has the xlink namespace");
  assert.equal(use.getAttributeNS("http://www.w3.org/XML/1998/namespace", "space"), "preserve");
  assert.equal($("link").namespaceURI, SVG, "an anchor inside svg is an SVG element");
  assert.equal($("link").firstChild.namespaceURI, SVG);
  assert.equal($("in-fo").namespaceURI, XHTML, "foreignObject children are HTML");
  assert.equal($("html-a").namespaceURI, XHTML);
  click(w, $("toggle"));
  assert.equal($("use").getAttributeNS(XLINK, "href"), "#b", "an update keeps the namespace");
  assert.ok($("use") === use);
  click(w, $("grow"));
  const circle = svg.querySelector("circle");
  assert.equal(circle.namespaceURI, SVG, "a new SVG child made by an update");
});

test("SVG from the server keeps its nodes and namespace", () => {
  setup('<div><svg id="svg" viewBox="0 0 20 20"><path id="path" d="M0 0"></path><use id="use" xlink:href="#a" xml:space="preserve"></use><a id="link"><text>t</text></a><foreignObject id="fo"><div id="in-fo">html</div></foreignObject></svg><a id="html-a" href="#x">link</a><button id="toggle">toggle</button><button id="grow">grow</button></div>');
  const svg = $("svg");
  const use = $("use");
  App.mount_svg("root");
  assert.ok($("svg") === svg && $("use") === use);
  assert.equal(use.getAttributeNS(XLINK, "href"), "#a");
});

const keyOf = (w, key, init = {}) => ({ key, ...init });

test("focus trap: focus enters, Tab wraps, outside focus comes back, close returns focus", () => {
  const w = setup("");
  App.mount_dialog("trap", "root");
  const opener = $("opener");
  opener.focus();
  click(w, opener);
  assert.equal(document.activeElement.id, "first", "focus moves into the dialog");
  $("close").focus();
  let event = keydown(w, $("close"), { key: "Tab" });
  assert.ok(event.defaultPrevented, "Tab on the last element is taken");
  assert.equal(document.activeElement.id, "first", "and wraps to the first");
  event = keydown(w, $("first"), { key: "Tab", shiftKey: true });
  assert.ok(event.defaultPrevented);
  assert.equal(document.activeElement.id, "close", "Shift+Tab on the first wraps to the last");
  $("second").focus();
  event = keydown(w, $("second"), { key: "Tab" });
  assert.ok(!event.defaultPrevented, "Tab in the middle is left to the browser");
  $("outside").focus();
  assert.equal(document.activeElement.id, "first", "focus that leaves comes back");
  click(w, $("close"));
  assert.ok(document.activeElement === opener || document.activeElement.id === "opener", "focus returns to the opener: " + document.activeElement.id);
  $("outside").focus();
  assert.equal(document.activeElement.id, "outside", "the trap is gone");
  const stray = keydown(w, $("outside"), { key: "Tab" });
  assert.ok(!stray.defaultPrevented, "no listener is left");
});

test("focus trap without return_focus leaves focus where it is", () => {
  const w = setup("");
  App.mount_dialog("trap-no-return", "root");
  $("opener").focus();
  click(w, $("opener"));
  click(w, $("close"));
  assert.notEqual(document.activeElement.id, "opener");
});

test("scroll lock locks while the state is active and restores the old style", () => {
  const w = setup("");
  document.documentElement.style.overflow = "scroll";
  App.mount_dialog("lock", "root");
  assert.equal(document.documentElement.style.overflow, "scroll");
  click(w, $("opener"));
  assert.equal(document.documentElement.style.overflow, "hidden");
  click(w, $("close"));
  assert.equal(document.documentElement.style.overflow, "scroll");
});

test("two scroll locks release together, to the original style", () => {
  const w = setup("");
  App.mount_dialog("double-lock", "root");
  click(w, $("opener"));
  assert.equal(document.documentElement.style.overflow, "hidden");
  click(w, $("close"));
  assert.equal(document.documentElement.style.overflow, "", "not stuck at hidden");
});

test("outside press closes, inside press does not, nothing is left behind", () => {
  const w = setup("");
  App.mount_dialog("outside", "root");
  click(w, $("opener"));
  pointerdown(w, $("first"));
  assert.match(text("info"), /outsides=0/, "inside press: no event");
  assert.ok($("dialog"));
  pointerdown(w, $("outside"));
  assert.equal($("dialog"), null, "closed by the outside press");
  assert.match(text("info"), /outsides=1 leaks=0/);
  pointerdown(w, $("outside"));
  assert.match(text("info"), /leaks=0/, "the listener was removed");
});

test("global keys: bound keys reach the machine and are prevented, others are not", () => {
  const w = setup("");
  App.mount_dialog("keys", "root");
  const idle = keydown(w, document.body, { key: "Escape" });
  assert.ok(!idle.defaultPrevented, "no source while closed");
  click(w, $("opener"));
  const other = keydown(w, document.body, { key: "x" });
  assert.ok(!other.defaultPrevented, "an unbound key is not prevented");
  assert.ok($("dialog"));
  const escape = keydown(w, document.body, { key: "Escape" });
  assert.ok(escape.defaultPrevented, "a bound key is prevented, synchronously");
  assert.equal($("dialog"), null);
  assert.match(text("info"), /escapes=1/);
  click(w, $("opener"));
  const combo = keydown(w, document.body, { key: "k", ctrlKey: true });
  assert.ok(combo.defaultPrevented);
  assert.equal($("dialog"), null, "Ctrl+k toggles");
  const late = keydown(w, document.body, { key: "Escape" });
  assert.ok(!late.defaultPrevented, "the listener stopped with the state");
  assert.match(text("info"), /leaks=0/);
});

test("global keys: not while typing in a text field unless Ctrl or Meta or Escape; not when an element handler prevented the key", () => {
  const w = setup("");
  App.mount_dialog("keys", "root");
  click(w, $("opener"));
  const fields = ["input", "textarea", "select"].map((tag) => {
    const el = document.createElement(tag);
    document.body.appendChild(el);
    return el;
  });
  const editable = document.createElement("div");
  editable.setAttribute("contenteditable", "true");
  document.body.appendChild(editable);
  for (const field of [...fields, editable]) {
    const typed = keydown(w, field, { key: "j" });
    assert.ok(!typed.defaultPrevented, `a letter in ${field.tagName} is not prevented`);
    assert.ok($("dialog"), `a letter in ${field.tagName} is text, not a shortcut`);
  }
  const combo = keydown(w, fields[0], { key: "k", ctrlKey: true });
  assert.ok(combo.defaultPrevented, "Ctrl+k works in a text field");
  assert.equal($("dialog"), null);
  click(w, $("opener"));
  keydown(w, fields[1], { key: "Escape" });
  assert.equal($("dialog"), null, "Escape works in a text field");
  assert.match(text("info"), /escapes=1/);
  click(w, $("opener"));
  const owner = document.createElement("button");
  owner.addEventListener("keydown", (ev) => ev.preventDefault());
  document.body.appendChild(owner);
  keydown(w, owner, { key: "j" });
  assert.ok($("dialog"), "a key that an element handler prevented is not a shortcut");
  keydown(w, document.body, { key: "j" });
  assert.equal($("dialog"), null, "the same key elsewhere is");
  for (const el of [...fields, editable, owner]) el.remove();
});

test("element key handlers: payload, ~prevent only when handled", () => {
  const w = setup("");
  App.mount_events("root");
  const down = keydown(w, $("prevent"), { key: "ArrowDown", ctrlKey: true, shiftKey: true });
  assert.ok(down.defaultPrevented, "~prevent cancels the default behavior at once");
  assert.equal(text("log"), "prevent:Ctrl+Shift+ArrowDown");
  const other = keydown(w, $("prevent"), { key: "a" });
  assert.ok(!other.defaultPrevented, "the handler returned None: no prevent");
  assert.equal(text("log"), "prevent:Ctrl+Shift+ArrowDown", "and no event");
  const plain = keydown(w, $("plain"), { key: "q" });
  assert.ok(!plain.defaultPrevented, "Some without ~prevent does not prevent");
  assert.match(text("log"), /plain:q/);
  $("keyup").dispatchEvent(new w.KeyboardEvent("keyup", { key: "z", bubbles: true }));
  assert.match(text("log"), /up:z/);
});

test("pointer enter and leave fire when the pointer crosses the element edge", () => {
  const w = setup("");
  App.mount_events("root");
  const over = (target, related) =>
    target.dispatchEvent(new w.MouseEvent("pointerover", { bubbles: true, relatedTarget: related }));
  const out = (target, related) =>
    target.dispatchEvent(new w.MouseEvent("pointerout", { bubbles: true, relatedTarget: related }));
  over($("hover"), document.body);
  assert.equal(text("log"), "enter");
  over($("inner"), $("hover"));
  assert.equal(text("log"), "enter", "moving to a child is not an enter");
  out($("inner"), $("hover"));
  assert.equal(text("log"), "enter", "nor a leave");
  out($("hover"), document.body);
  assert.equal(text("log"), "enter;leave");
});

test("~rate Debounce delivers the last value once", async () => {
  const w = setup("");
  App.mount_events("root");
  const field = $("debounced");
  for (const value of ["a", "ab", "abc"]) {
    field.value = value;
    field.dispatchEvent(new w.Event("input", { bubbles: true }));
    await wait(5);
  }
  assert.equal(text("log"), "", "nothing yet");
  await until(() => text("log") !== "", "debounced event");
  assert.equal(text("log"), "debounced:abc");
  await wait(60);
  assert.equal(text("log"), "debounced:abc", "only once");
});

test("~rate Throttle delivers the first at once and the last of the window", async () => {
  const w = setup("");
  App.mount_events("root");
  const field = $("throttled");
  for (const key of ["a", "b", "c"]) keydown(w, field, { key });
  assert.equal(text("log"), "throttled:a", "the first goes through at once");
  await until(() => text("log").includes("throttled:c"), "trailing value");
  assert.equal(text("log"), "throttled:a;throttled:c", "the middle value was dropped");
});

class FakeObserver {
  static instances = [];
  constructor(callback) {
    this.callback = callback;
    this.observed = [];
    this.disconnected = false;
    FakeObserver.instances.push(this);
  }
  observe(el) {
    this.observed.push(el);
  }
  disconnect() {
    this.disconnected = true;
  }
}

test("resize and intersection observers report portable values and disconnect", () => {
  const w = setup("");
  class Resize extends FakeObserver { static instances = []; constructor(cb) { super(cb); Resize.instances.push(this); } }
  class Intersect extends FakeObserver { static instances = []; constructor(cb) { super(cb); Intersect.instances.push(this); } }
  globalThis.ResizeObserver = Resize;
  globalThis.IntersectionObserver = Intersect;
  App.mount_dialog("observers", "root");
  click(w, $("opener"));
  assert.equal(Resize.instances.length, 1);
  assert.ok(Resize.instances[0].observed[0] === $("box"), "it observes the ref element");
  Resize.instances[0].callback([{ contentRect: { x: 1, y: 2, width: 30, height: 40 } }]);
  assert.match(text("info"), /width=30/);
  Intersect.instances[0].callback([{ isIntersecting: true }]);
  assert.match(text("info"), /seen=visible/);
  Intersect.instances[0].callback([{ isIntersecting: false }]);
  assert.match(text("info"), /seen=hidden/);
  click(w, $("close"));
  assert.ok(Resize.instances[0].disconnected && Intersect.instances[0].disconnected, "both stop with the state");
});

const gadgetLog = () => text("gadget-log");

test("scroll_to scrolls the window or the ref element", () => {
  const w = setup("");
  const calls = [];
  w.scrollTo = (x, y) => calls.push([x, y]);
  App.mount_gadgets("root");
  click(w, $("scroll-window"));
  assert.deepEqual(calls, [[5, 7]], "the window scrolls to left 5, top 7");
  click(w, $("scroll-box"));
  assert.equal($("gadget-box").scrollLeft, 3);
  assert.equal($("gadget-box").scrollTop, 4);
  assert.deepEqual(calls, [[5, 7]], "the window does not scroll for a ref");
});

test("measure answers the rect of the ref, or none", () => {
  const w = setup("");
  App.mount_gadgets("root");
  $("gadget-box").getBoundingClientRect = () => ({ left: 1, top: 2, width: 30, height: 40 });
  click(w, $("measure-box"));
  assert.equal(gadgetLog(), "rect:1,2,30,40");
  click(w, $("measure-missing"));
  assert.equal(gadgetLog(), "rect:1,2,30,40|rect:none");
});

test("copy writes the clipboard and answers the result", async () => {
  const w = setup("");
  const written = [];
  let fail = false;
  Object.defineProperty(w.navigator, "clipboard", {
    configurable: true,
    value: { writeText: (t) => (fail ? Promise.reject(new Error("denied")) : (written.push(t), Promise.resolve())) },
  });
  App.mount_gadgets("root");
  click(w, $("copy"));
  await until(() => gadgetLog() === "copy:true", "copy answers true");
  assert.deepEqual(written, ["hello"]);
  click(w, $("copy-quiet"));
  await wait(10);
  assert.deepEqual(written, ["hello", "quiet"], "a copy with no result still writes");
  assert.equal(gadgetLog(), "copy:true", "and raises no event");
  fail = true;
  click(w, $("copy"));
  await until(() => gadgetLog() === "copy:true|copy:false", "a refused write answers false");
});

test("copy answers false with no clipboard", async () => {
  const w = setup("");
  App.mount_gadgets("root");
  click(w, $("copy"));
  await until(() => gadgetLog() === "copy:false", "no clipboard answers false");
});

test("load_script loads a src once per page", () => {
  const w = setup("");
  App.mount_gadgets("root");
  const scripts = (src) => [...document.querySelectorAll("script")].filter((s) => s.getAttribute("src") === src);
  click(w, $("load-a"));
  click(w, $("load-a"));
  assert.equal(scripts("/a.js").length, 1, "one script element for two actions");
  assert.equal(gadgetLog(), "", "no answer before the load");
  scripts("/a.js")[0].dispatchEvent(new w.Event("load"));
  assert.equal(gadgetLog(), "load:/a.js:true|load:/a.js:true", "both actions get the load");
  click(w, $("load-a"));
  assert.equal(gadgetLog(), "load:/a.js:true|load:/a.js:true|load:/a.js:true", "a loaded src answers at once");
  assert.equal(scripts("/a.js").length, 1, "and is not loaded again");
  click(w, $("load-b"));
  scripts("/b.js")[0].dispatchEvent(new w.Event("error"));
  assert.match(gadgetLog(), /load:\/b.js:false$/, "a failed load answers false");
  assert.equal(scripts("/b.js").length, 0, "the failed script element is removed");
  click(w, $("load-b"));
  assert.equal(scripts("/b.js").length, 1, "a later action tries again");
});

test("scroll, visibility and online sources report and stop", () => {
  const w = setup("");
  App.mount_gadgets("root");
  const box = $("gadget-box");
  for (const [k, v] of Object.entries({ scrollWidth: 300, scrollHeight: 400, clientWidth: 100, clientHeight: 50 }))
    Object.defineProperty(box, k, { configurable: true, value: v });
  click(w, $("watch"));
  assert.equal(gadgetLog(), "page:0,0,0,0,0,0|box:0,0,300,400,100,50|visible:true|online:true", "each source reports at the start");
  box.scrollTop = 20;
  box.scrollLeft = 10;
  box.dispatchEvent(new w.Event("scroll"));
  assert.match(gadgetLog(), /\|box:10,20,300,400,100,50$/);
  w.scrollY = 30;
  w.dispatchEvent(new w.Event("scroll"));
  assert.match(gadgetLog(), /\|page:0,30,0,0,0,0$/);
  Object.defineProperty(document, "visibilityState", { configurable: true, value: "hidden" });
  document.dispatchEvent(new w.Event("visibilitychange"));
  assert.match(gadgetLog(), /\|visible:false$/);
  Object.defineProperty(w.navigator, "onLine", { configurable: true, value: false });
  w.dispatchEvent(new w.Event("offline"));
  assert.match(gadgetLog(), /\|online:false$/);
  click(w, $("unwatch"));
  const before = gadgetLog();
  box.dispatchEvent(new w.Event("scroll"));
  w.dispatchEvent(new w.Event("scroll"));
  document.dispatchEvent(new w.Event("visibilitychange"));
  w.dispatchEvent(new w.Event("online"));
  assert.equal(gadgetLog(), before, "nothing arrives after the state is left");
});

function stubMedia(w, matches) {
  const listeners = new Set();
  const list = {
    matches,
    addEventListener: (type, f) => type === "change" && listeners.add(f),
    removeEventListener: (type, f) => type === "change" && listeners.delete(f),
  };
  const queries = [];
  w.matchMedia = (query) => {
    queries.push(query);
    return list;
  };
  return {
    queries,
    listeners,
    set(value) {
      list.matches = value;
      for (const f of [...listeners]) f({ matches: value });
    },
  };
}

test("document attribute and cookie actions, cookie reader", () => {
  const w = setup("");
  App.mount_document("root");
  const root = document.documentElement;
  click(w, $("attr-ocean"));
  assert.equal(root.getAttribute("data-theme"), "ocean", "the attribute is set on <html>");
  click(w, $("attr-none"));
  assert.equal(root.hasAttribute("data-theme"), false, "None removes it");
  assert.equal(App.read_cookie("silica-theme"), "(none)", "no cookie yet");
  click(w, $("cookie-dark"));
  assert.match(document.cookie, /(^|; )silica-theme=dark($|;)/, "the cookie is stored");
  assert.equal(App.read_cookie("silica-theme"), "dark", "and read back");
  document.cookie = "xsilica-theme=other; Path=/";
  assert.equal(App.read_cookie("silica-theme"), "dark", "a longer name does not match");
  click(w, $("attr-bad"));
  assert.match(document.body.textContent, /bad document attribute name/, "a bad name is an error of the page");
  assert.deepEqual(root.getAttributeNames(), [], "and sets nothing");
});

test("media source reports at the start, follows changes and stops", () => {
  const w = setup("");
  const media = stubMedia(w, true);
  App.mount_document("root");
  click(w, $("watch"));
  assert.deepEqual(media.queries, ["(prefers-color-scheme: dark)"]);
  assert.equal(text("document-log"), "dark:true", "the value at the start");
  media.set(false);
  assert.equal(text("document-log"), "dark:true|dark:false", "a change");
  click(w, $("unwatch"));
  assert.equal(media.listeners.size, 0, "the listener is removed");
  media.set(true);
  assert.equal(text("document-log"), "dark:true|dark:false", "nothing after the state is left");
});

test("portal: a later portal keeps its nodes and focus when an earlier one opens and closes", () => {
  const w = setup("");
  App.mount_twin("root");
  click(w, $("toggle-second"));
  const second = $("second-popup");
  const button = $("second-button");
  button.focus();
  assert.equal(document.activeElement, button);
  click(w, $("toggle-first"));
  assert.ok($("first-popup"), "the earlier portal opens");
  assert.ok($("second-popup") === second, "the later portal keeps its element when the earlier one opens");
  assert.equal(document.activeElement, button, "and keeps focus");
  click(w, $("toggle-first"));
  assert.equal($("first-popup"), null, "the earlier portal closes");
  assert.ok($("second-popup") === second, "the later portal keeps its element when the earlier one closes");
  assert.equal(document.activeElement, button, "and keeps focus");
  assert.equal(document.querySelectorAll("[data-silica-portal]").length, 1, "one container is left");
});

test("portal: children paint elsewhere, events still work, removal cleans up", () => {
  const w = setup("");
  App.mount_portal("root");
  assert.equal($("popup"), null);
  click(w, $("toggle"));
  const popup = $("popup");
  assert.ok(popup, "the popup exists");
  assert.equal(document.getElementById("root").contains(popup), false, "not inside the root");
  assert.equal(popup.parentNode.parentNode, document.body, "inside a container in the body");
  assert.equal(document.getElementById("root").textContent.includes("inc"), false);
  assert.ok($("root").textContent.includes("beforeafter"), "texts around the portal are still adjacent");
  click(w, $("popup-inc"));
  click(w, $("popup-inc"));
  assert.equal(text("count"), "2", "a press inside the portal reaches the machine");
  assert.ok($("popup") === popup, "the portal node is kept");
  click(w, $("toggle"));
  assert.equal($("popup"), null);
  assert.equal(document.querySelectorAll("[data-silica-portal]").length, 0, "the container is removed");
});

test("portal in a theme scope: the container in the body has the theme of the nearest scope, and follows it", () => {
  const w = setup("");
  App.mount_scoped("root");
  const scoped = $("scoped-text");
  click(w, $("scoped-toggle"));
  const container = (name) => $(name + "-popup").parentNode;
  for (const name of ["outer", "inner", "page"])
    assert.equal(container(name).parentNode, document.body, `${name}: the container is in the body`);
  assert.equal(container("outer").getAttribute("data-theme"), "ocean", "the scope theme");
  assert.equal(container("inner").getAttribute("data-theme"), "dark", "the nearest scope wins");
  assert.equal(container("page").hasAttribute("data-theme"), false, "outside a scope: the page theme");
  assert.ok($("scoped-text") === scoped, "the scope keeps its children");
  const outer = $("outer-popup");
  click(w, $("scoped-switch"));
  assert.equal(container("outer").getAttribute("data-theme"), "high-contrast", "the container follows the scope");
  assert.ok($("outer-popup") === outer, "the popup node is kept");
  assert.equal(container("inner").getAttribute("data-theme"), "dark");
  click(w, $("scoped-toggle"));
  assert.equal(document.querySelectorAll("[data-silica-portal]").length, 0, "the containers are removed");
});

test("portal with a target id paints into that element", () => {
  const w = setup("");
  App.mount_portal("root");
  click(w, $("toggle"));
  click(w, $("move"));
  const popup = $("popup");
  assert.ok(popup);
  assert.ok($("modal-root").contains(popup), "inside the target element");
});

test("boundary: a failing child shows the fallback, the parent keeps its state, reset remounts", () => {
  const w = setup("");
  const errors = silenceErrors();
  try {
    App.mount_boundary(true, false, "root");
    click(w, $("bump"));
    click(w, $("bomb-inc"));
    click(w, $("bomb-inc"));
    assert.equal(text("bomb-count"), "2");
    click(w, $("bomb-boom"));
    assert.ok($("fallback"), "fallback shows");
    assert.equal($("bomb"), null);
    assert.match(text("fallback-text"), /Something went wrong\. no-detail/);
    assert.ok(!document.body.textContent.includes("secret boom"), "prod: no exception text on the page");
    assert.equal(text("bumps"), "1", "the parent state is kept");
    click(w, $("retry"));
    assert.equal($("fallback"), null);
    assert.equal(text("bomb-count"), "0", "reset restarts the child from init");
    click(w, $("bomb-break"));
    assert.ok($("fallback"), "a view exception is caught as well");
    click(w, $("retry"));
    assert.equal(text("bomb-count"), "0");
  } finally {
    errors.restore();
  }
  assert.equal(App.reported().length >= 2, true, "on_error ran");
});

test("boundary in dev mode shows the details", () => {
  const w = setup("");
  const errors = silenceErrors();
  try {
    App.mount_boundary(true, true, "root");
    click(w, $("bomb-boom"));
    assert.match(text("fallback-text"), /secret boom/);
    assert.match(text("fallback-text"), /machine: bomb/);
  } finally {
    errors.restore();
  }
});

test("top-level boundary: error page, no exception text in prod, Try again restarts", () => {
  const w = setup("");
  const errors = silenceErrors();
  try {
    App.mount_boundary(false, false, "root");
    click(w, $("bump"));
    assert.equal(text("bumps"), "1");
    click(w, $("bomb-boom"));
    const page = document.querySelector("[data-silica-error]");
    assert.ok(page, "error page");
    assert.match(page.textContent, /Something went wrong/);
    assert.match(page.textContent, /Error id: [0-9a-f]{6}/);
    assert.ok(!page.textContent.includes("secret boom"), "prod hides the exception");
    assert.equal(errors.seen.length > 0, true, "the console got it");
    click(w, [...page.querySelectorAll("button")].find((b) => b.textContent === "Try again"));
    assert.equal(document.querySelector("[data-silica-error]"), null);
    assert.equal(text("bumps"), "0", "the root starts again with fresh state");
    click(w, $("bump"));
    assert.equal(text("bumps"), "1", "and works");
  } finally {
    errors.restore();
  }
});

test("top-level boundary in dev mode shows the details", () => {
  const w = setup("");
  const errors = silenceErrors();
  try {
    App.mount_boundary(false, true, "root");
    click(w, $("bomb-boom"));
    assert.match(document.querySelector("[data-silica-error]").textContent, /secret boom/);
  } finally {
    errors.restore();
  }
});

test("a step that raises changes nothing on screen before the error page", () => {
  const w = setup("");
  const errors = silenceErrors();
  try {
    App.mount_boundary(false, false, "root");
    click(w, $("bomb-inc"));
    click(w, $("bomb-break"));
    assert.ok(document.querySelector("[data-silica-error]"), "view exception reaches the top");
    assert.equal($("bomb"), null);
  } finally {
    errors.restore();
  }
});

test("the runtime runs with another renderer and no DOM", () => {
  setup("");
  App.mount_fake();
  let log = App.fake_log();
  assert.equal(log.length, 1, "only the first paint at the start");
  assert.match(log[0], /^paint <main><button id="opener"/);
  App.fake_press();
  log = App.fake_log();
  assert.equal(log.length, 3);
  assert.match(log[1], /^paint <main>.*<div id="dialog"/, "the new view is painted");
  assert.equal(log[2], "start 1.1/dialog-trap/opened/invoke0 focus_trap", "then the source starts, after the paint");
  assert.equal(document.getElementById("root").children.length, 0, "the DOM was not used");
});

const rows = () => $("rows").children;

function childMoves(w, node, action) {
  const observer = new w.MutationObserver(() => {});
  observer.observe(node, { childList: true });
  action();
  const records = observer.takeRecords();
  observer.disconnect();
  let added = 0;
  let removed = 0;
  for (const r of records) {
    added += r.addedNodes.length;
    removed += r.removedNodes.length;
  }
  return { added, removed };
}

test("rows: create 10,000, clear, create 10,000 again", () => {
  const w = setup("");
  App.mount_rows("root");
  click(w, $("create-10000"));
  assert.equal(rows().length, 10000);
  click(w, $("clear"));
  assert.equal(rows().length, 0);
  click(w, $("create-10000"));
  assert.equal(rows().length, 10000, "no error page");
  assert.equal(rows()[9999].id, "row-20000");
  click(w, $("remove-20000"));
  assert.equal(rows().length, 9999, "a press on the last row reaches its handler");
  const second = rows()[1].id;
  click(w, $("swap"));
  assert.equal(rows().length, 9999, "a patch with the same count keeps every row");
  assert.notEqual(rows()[1].id, second, "the swap reached the DOM");
  click(w, $("clear"));
  assert.equal(rows().length, 0, "clear after a 10,000-row patch");
});

test("rows: append keeps the existing row nodes", () => {
  const w = setup("");
  App.mount_rows("root");
  click(w, $("create-1000"));
  const first = rows()[0];
  const moves = childMoves(w, $("rows"), () => click(w, $("append")));
  assert.equal(rows().length, 2000);
  assert.ok(rows()[0] === first, "the first row is the same node");
  assert.deepEqual(moves, { added: 1000, removed: 0 });
});

test("rows: remove takes out one node and keeps the others", () => {
  const w = setup("");
  App.mount_rows("root");
  click(w, $("create-1000"));
  const before = [...rows()];
  const moves = childMoves(w, $("rows"), () => click(w, $("remove-500")));
  assert.equal(rows().length, 999);
  assert.deepEqual(moves, { added: 0, removed: 1 });
  assert.equal($("row-500"), null);
  assert.ok(rows()[499] === before[500], "the next row is the same node");
});

test("rows: a swap moves only the two rows", () => {
  const w = setup("");
  App.mount_rows("root");
  click(w, $("create-1000"));
  const second = rows()[1];
  const moves = childMoves(w, $("rows"), () => click(w, $("swap")));
  assert.equal(rows()[1].id, "row-999");
  assert.ok(rows()[998] === second, "the row node moved");
  assert.ok(moves.added <= 2, `at most two moves, got ${moves.added}`);
});

test("rows: select changes the class of two rows and nothing else", () => {
  const w = setup("");
  App.mount_rows("root");
  click(w, $("create-1000"));
  click(w, $("select-10"));
  const observer = new w.MutationObserver(() => {});
  observer.observe($("rows"), { childList: true, attributes: true, subtree: true, characterData: true });
  click(w, $("select-20"));
  const records = observer.takeRecords();
  observer.disconnect();
  assert.deepEqual(
    records.map((r) => `${r.type} ${r.target.id} ${r.attributeName}`).sort(),
    ["attributes row-10 class", "attributes row-20 class"],
  );
  assert.equal($("row-20").className, "danger");
  assert.equal($("row-10").className, "");
});

test("rows: replace all, then reverse order through clear", () => {
  const w = setup("");
  App.mount_rows("root");
  click(w, $("create-1000"));
  click(w, $("create-1000"));
  assert.equal(rows().length, 1000);
  assert.equal(rows()[0].id, "row-1001");
  assert.equal(rows()[999].id, "row-2000");
});

test("a field whose event leaves the state as it was gets the view value back", () => {
  const w = setup("");
  App.mount_clamp("root");
  const input = $("clamp");
  input.value = "abcd";
  input.dispatchEvent(new w.Event("input", { bubbles: true }));
  assert.equal(input.value, "abc");
  input.value = "ab";
  input.dispatchEvent(new w.Event("input", { bubbles: true }));
  assert.equal(input.value, "ab");
  assert.equal(input.getAttribute("value"), "ab");
});
