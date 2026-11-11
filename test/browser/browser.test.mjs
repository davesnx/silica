import { test, afterEach } from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import * as App from "./out/test/browser/app.mjs";

let scroll;

function installScrollStub(w) {
  scroll = { x: 0, y: 0, calls: [], intoView: [] };
  w.scrollTo = (x, y) => {
    scroll.calls.push([x, y]);
    scroll.x = x;
    scroll.y = y;
  };
  Object.defineProperty(w, "scrollX", { get: () => scroll.x, configurable: true });
  Object.defineProperty(w, "scrollY", { get: () => scroll.y, configurable: true });
  w.Element.prototype.scrollIntoView = function () {
    scroll.intoView.push(this.id);
  };
}

function setup(url = "http://localhost/", csrf, virtualConsole) {
  const meta = csrf
    ? `<meta name="silica-csrf" content="${csrf}">`
    : "";
  const dom = new JSDOM(
    `<head>${meta}</head><body><div id="root"><p id="stale">server</p></div></body>`,
    { url, pretendToBeVisual: true, virtualConsole },
  );
  const w = dom.window;
  installScrollStub(w);
  Object.assign(globalThis, {
    window: w,
    document: w.document,
    location: w.location,
    history: w.history,
    sessionStorage: w.sessionStorage,
  });
  return w;
}

const $ = (id) => document.getElementById(id);
const text = (id) => $(id).textContent;

async function until(fn, label) {
  for (let i = 0; i < 100; i++) {
    if (fn()) return;
    await new Promise((r) => setTimeout(r, 10));
  }
  assert.fail("timeout: " + label);
}

function click(w, el, init = {}) {
  const ev = new w.MouseEvent("click", {
    bubbles: true,
    cancelable: true,
    button: 0,
    ...init,
  });
  el.dispatchEvent(ev);
  return ev;
}

const items = () =>
  [...document.querySelectorAll("#list li")].map((li) => li.textContent);

test("initial render replaces the root children", () => {
  setup();
  App.mount_basic("root");
  assert.equal($("stale"), null);
  assert.equal(text("count"), "0");
  assert.deepEqual(items(), ["a", "b", "c"]);
});

test("press re-renders", () => {
  const w = setup();
  App.mount_basic("root");
  click(w, $("inc"));
  click(w, $("inc"));
  assert.equal(text("count"), "2");
});

test("Enter and Space press a non-button, a real button is left to click", () => {
  const w = setup();
  App.mount_basic("root");
  const key = (el, k) =>
    el.dispatchEvent(
      new w.KeyboardEvent("keydown", { key: k, bubbles: true, cancelable: true }),
    );
  key($("fake"), "Enter");
  key($("fake"), " ");
  key($("fake"), "a");
  assert.equal(text("count"), "2");
  key($("inc"), "Enter");
  assert.equal(text("count"), "2");
});

test("double press reaches the handler from a child", () => {
  const w = setup();
  App.mount_basic("root");
  $("dbl-child").dispatchEvent(
    new w.MouseEvent("dblclick", { bubbles: true, cancelable: true }),
  );
  assert.equal(text("doubles"), "1");
});

test("input delivers the value", () => {
  const w = setup();
  App.mount_basic("root");
  const input = $("input");
  input.value = "hello";
  input.dispatchEvent(new w.Event("input", { bubbles: true }));
  assert.equal(text("typed"), "hello");
  assert.equal($("input").value, "hello");
  assert.equal($("input"), input);
});

test("submit is prevented and delivered", () => {
  const w = setup();
  App.mount_basic("root");
  const ev = new w.Event("submit", { bubbles: true, cancelable: true });
  $("form").dispatchEvent(ev);
  assert.equal(ev.defaultPrevented, true);
  assert.equal(text("submits"), "1");
});

test("focus action moves focus, focus and blur events are delivered", () => {
  const w = setup();
  App.mount_basic("root");
  click(w, $("focus-btn"));
  assert.equal(document.activeElement, $("input"));
  assert.equal(text("focus"), "in");
  $("input").blur();
  assert.equal(text("focus"), "out");
});

test("keyed reorder moves the same DOM nodes", () => {
  const w = setup();
  App.mount_basic("root");
  const [a, b, c] = [...document.querySelectorAll("#list li")];
  a.expando = "a";
  click(w, $("reverse"));
  const after = [...document.querySelectorAll("#list li")];
  assert.deepEqual(items(), ["c", "b", "a"]);
  assert.equal(after[0], c);
  assert.equal(after[1], b);
  assert.equal(after[2], a);
  assert.equal(after[2].expando, "a");
});

test("After fires later", async () => {
  const w = setup();
  globalThis.fetch = async () => ({ status: 200, text: async () => "1" });
  App.mount_basic("root");
  click(w, $("ask"));
  assert.equal(text("ticks"), "0");
  await until(() => text("ticks") === "1", "tick");
});

test("Call delivers success and sends the wire request", async () => {
  const w = setup();
  const seen = [];
  globalThis.fetch = async (url, init) => {
    seen.push([url, init.method, init.body, init.headers["content-type"]]);
    return { status: 200, text: async () => "42" };
  };
  App.mount_basic("root");
  click(w, $("ask"));
  await until(() => text("answer") === "ok 42", "answer");
  assert.deepEqual(seen, [["/api/double", "POST", "21", "application/json"]]);
});

test("Call sends the CSRF token from the meta tag", async () => {
  const w = setup("http://localhost/", "tok-1");
  const seen = [];
  globalThis.fetch = async (url, init) => {
    seen.push(init.headers);
    return { status: 200, text: async () => "42" };
  };
  App.mount_basic("root");
  click(w, $("ask"));
  await until(() => text("answer") === "ok 42", "answer");
  assert.deepEqual(seen, [
    { "content-type": "application/json", "x-silica-csrf": "tok-1" },
  ]);
});

test("Call maps non-200, bad JSON and fetch failure", async () => {
  const w = setup();
  App.mount_basic("root");
  globalThis.fetch = async () => ({ status: 500, text: async () => "boom" });
  click(w, $("ask"));
  await until(() => text("answer") === "http 500", "http");
  globalThis.fetch = async () => ({ status: 200, text: async () => "{nope" });
  click(w, $("ask"));
  await until(() => text("answer") === "decode", "decode");
  globalThis.fetch = async () => {
    throw new Error("offline");
  };
  click(w, $("ask"));
  await until(() => text("answer") === "network", "network");
});

test("a call with a timeout ends with a timeout error and drops the late answer", async () => {
  const w = setup();
  let release;
  globalThis.fetch = () =>
    new Promise((resolve) => {
      release = () => resolve({ status: 200, text: async () => "42" });
    });
  App.mount_basic("root");
  click(w, $("ask-timed"));
  assert.equal(text("answer"), "");
  await until(() => text("answer") === "timeout", "timeout");
  release();
  await new Promise((r) => setTimeout(r, 40));
  assert.equal(text("answer"), "timeout");
});

test("an answer before the timeout wins and the timeout never fires", async () => {
  const w = setup();
  globalThis.fetch = async () => ({ status: 200, text: async () => "7" });
  App.mount_basic("root");
  click(w, $("ask-timed"));
  await until(() => text("answer") === "ok 7", "answer");
  await new Promise((r) => setTimeout(r, 80));
  assert.equal(text("answer"), "ok 7");
});

test("a call with no timeout waits", async () => {
  const w = setup();
  globalThis.fetch = () => new Promise(() => {});
  App.mount_basic("root");
  click(w, $("ask"));
  await new Promise((r) => setTimeout(r, 80));
  assert.equal(text("answer"), "");
});

test("link press pushes history, ignores modified clicks, sets title", () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  assert.equal(text("root").includes("home"), true);
  assert.equal(document.title, "Home");
  const before = history.length;
  click(w, $("to-about"), { ctrlKey: true });
  assert.equal(location.pathname, "/home");
  const ev = click(w, $("to-about"));
  assert.equal(ev.defaultPrevented, true);
  assert.equal(location.pathname, "/about");
  assert.equal(history.length, before + 1);
  assert.equal(document.querySelector("h1").textContent, "about");
  assert.equal(document.title, "About");
});

test("a link press does not reload the page", () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  const heading = document.querySelector("h1");
  heading.expando = "kept";
  click(w, $("to-item"));
  assert.equal(location.pathname, "/item/7");
  assert.equal(document.querySelector("h1"), heading);
  assert.equal(heading.expando, "kept");
  assert.equal(heading.textContent, "item 7");
  assert.equal(document.title, "Item 7");
});

test("a replace link replaces the history entry", () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  const before = history.length;
  click(w, $("to-about-replace"));
  assert.equal(location.pathname, "/about");
  assert.equal(history.length, before);
  assert.equal(document.querySelector("h1").textContent, "about");
  assert.equal(document.title, "About");
  assert.deepEqual(scroll.calls, []);
});

test("Navigate action pushes history and changes the view", () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  click(w, $("action-nav"));
  assert.equal(location.pathname, "/about");
  assert.equal(document.querySelector("h1").textContent, "about");
});

test("popstate goes back and forward", async () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  click(w, $("to-item"));
  assert.equal(document.querySelector("h1").textContent, "item 7");
  history.back();
  await until(
    () => document.querySelector("h1").textContent === "home",
    "popstate back",
  );
  assert.equal(location.pathname, "/home");
  assert.equal(document.title, "Home");
  history.forward();
  await until(
    () => document.querySelector("h1").textContent === "item 7",
    "popstate forward",
  );
  assert.equal(document.title, "Item 7");
});

test("unknown URL renders not_found", () => {
  setup("http://localhost/nowhere");
  App.mount_router("root");
  assert.equal(document.querySelector("h1").textContent, "404");
  assert.equal(document.title, "Not found");
});

test("each history entry has a key in history.state", () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  const first = history.state.silicaKey;
  assert.equal(typeof first, "number");
  click(w, $("to-about"));
  const second = history.state.silicaKey;
  assert.equal(typeof second, "number");
  assert.notEqual(second, first);
});

test("scroll: top on push, saved position restored on back and forward", async () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  scroll.y = 300;
  click(w, $("to-about"));
  assert.deepEqual(scroll.calls, [[0, 0]]);
  scroll.y = 50;
  history.back();
  await until(() => location.pathname === "/home", "back");
  await until(() => scroll.calls.length === 2, "restore");
  assert.deepEqual(scroll.calls[1], [0, 300]);
  history.forward();
  await until(() => scroll.calls.length === 3, "restore forward");
  assert.deepEqual(scroll.calls[2], [0, 50]);
  assert.equal(location.pathname, "/about");
});

function clampScroll(w) {
  scroll.max = Infinity;
  w.scrollTo = (x, y) => {
    scroll.calls.push([x, y]);
    const top = Math.min(y, scroll.max);
    const moved = top !== scroll.y || x !== scroll.x;
    scroll.x = x;
    scroll.y = top;
    if (moved) setTimeout(() => w.dispatchEvent(new w.Event("scroll")), 0);
  };
}

async function forwardToLateData() {
  const w = setup("http://localhost/home");
  clampScroll(w);
  const answers = [];
  globalThis.fetch = () =>
    new Promise((ok) =>
      answers.push(() => ok({ status: 200, text: async () => '"seven"' })),
    );
  App.mount_router("root");
  click(w, $("to-data"));
  answers.shift()();
  await until(() => $("data")?.textContent === "seven", "first load");
  scroll.y = 600;
  history.back();
  await until(() => location.pathname === "/home", "back");
  scroll.max = 100;
  history.forward();
  await until(() => location.pathname === "/data/7", "forward");
  await wait(10);
  assert.equal(text("data"), "loading");
  assert.equal(scroll.y, 100, "clamped while the page is short");
  return { w, answers };
}

test("scroll: a restore waits for content that arrives later", async () => {
  const { answers } = await forwardToLateData();
  scroll.max = 1000;
  answers.shift()();
  await until(() => text("data") === "seven", "late content");
  assert.equal(scroll.y, 600);
});

test("scroll: a user scroll ends a restore that waits", async () => {
  const { w, answers } = await forwardToLateData();
  const calls = scroll.calls.length;
  scroll.y = 40;
  w.dispatchEvent(new w.Event("scroll"));
  scroll.max = 1000;
  answers.shift()();
  await until(() => text("data") === "seven", "late content");
  assert.equal(scroll.y, 40);
  assert.equal(scroll.calls.length, calls);
});

test("scroll: a push with a hash scrolls to its target after paint", () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  click(w, $("action-hash"));
  assert.equal(location.pathname, "/about");
  assert.equal(location.hash, "#section");
  assert.deepEqual(scroll.intoView, ["section"]);
  assert.deepEqual(scroll.calls, []);
});

test("scroll: a window without scrollTo does not fail", () => {
  const w = setup("http://localhost/home");
  w.scrollTo = undefined;
  App.mount_router("root");
  click(w, $("to-about"));
  assert.equal(document.querySelector("h1").textContent, "about");
});

const h1 = () => document.querySelector("h1").textContent;
const editStatus = () => text("edit-status");

function type(w, el, value) {
  el.value = value;
  el.dispatchEvent(new w.Event("input", { bubbles: true }));
}

function editDraft(w) {
  App.mount_router("root");
  click(w, $("to-edit"));
  type(w, $("draft"), "draft");
  assert.equal(editStatus(), "dirty");
}

test("history.state keeps the index of each entry", () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  assert.equal(history.state.silicaIndex, 0);
  click(w, $("to-about"));
  assert.equal(history.state.silicaIndex, 1);
});

test("a block stops a link press and Navigate before the URL changes; Stay and Leave", () => {
  const w = setup("http://localhost/home");
  editDraft(w);
  const length = history.length;
  click(w, $("edit-about"));
  assert.equal(location.pathname, "/edit");
  assert.equal(editStatus(), "asking push /about");
  click(w, $("stay"));
  assert.equal(editStatus(), "dirty");
  click(w, $("edit-home"));
  assert.equal(location.pathname, "/edit");
  assert.equal(history.length, length);
  assert.equal(editStatus(), "asking push /home");
  click(w, $("leave"));
  assert.equal(location.pathname, "/home");
  assert.equal(h1(), "home");
  assert.equal(history.length, length + 1);
});

test("a block stops back: the position and the URL are restored; Resume goes back", async () => {
  const w = setup("http://localhost/home");
  editDraft(w);
  history.back();
  await until(() => editStatus() === "asking traverse -1 /home", "attempt");
  await until(() => history.state.silicaIndex === 1, "position restored");
  assert.equal(location.pathname, "/edit");
  await wait(30);
  assert.equal(editStatus(), "asking traverse -1 /home");
  click(w, $("leave"));
  await until(() => h1() === "home", "resumed back");
  assert.equal(location.pathname, "/home");
  assert.equal(history.state.silicaIndex, 0);
  history.forward();
  await until(() => h1() === "edit", "forward again");
  assert.equal(editStatus(), "clean");
});

test("beforeunload asks while a block is held", () => {
  const w = setup("http://localhost/home");
  const unload = () => {
    const ev = new w.Event("beforeunload", { cancelable: true });
    w.dispatchEvent(ev);
    return ev.defaultPrevented;
  };
  App.mount_router("root");
  click(w, $("to-edit"));
  assert.equal(unload(), false);
  type(w, $("draft"), "draft");
  assert.equal(unload(), true);
  click(w, $("save"));
  assert.equal(unload(), false);
});

test("guard Cancel on a link press goes back to the entry before it", async () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  click(w, $("to-locked"));
  await until(() => location.pathname === "/home", "reverted");
  await wait(30);
  assert.equal(location.pathname, "/home");
  assert.equal(history.state.silicaIndex, 0);
  assert.equal(h1(), "home");
  assert.equal(document.title, "Home");
  assert.equal(document.querySelector("[data-silica-announcer]"), null);
});

test("guard Cancel on back keeps the history position", async () => {
  const w = setup("http://localhost/locked");
  App.mount_router("root");
  click(w, $("locked-home"));
  assert.equal(history.state.silicaIndex, 1);
  const seen = [];
  w.addEventListener("popstate", () => seen.push(location.pathname));
  history.back();
  await until(() => seen.length === 2, "back, then the revert");
  await wait(30);
  assert.deepEqual(seen, ["/locked", "/home"]);
  assert.equal(location.pathname, "/home");
  assert.equal(history.state.silicaIndex, 1);
  assert.equal(h1(), "home");
});

test("scroll positions are kept in sessionStorage and restored after a reload", () => {
  let w = setup("http://localhost/home");
  App.mount_router("root");
  scroll.y = 420;
  click(w, $("to-about"));
  const key = history.state.silicaKey;
  const saved = sessionStorage.getItem("silica-scroll");
  assert.ok(saved.includes(":0:420"), saved);
  scroll.y = 77;
  w.dispatchEvent(new w.Event("pagehide"));
  assert.ok(sessionStorage.getItem("silica-scroll").includes(`${key}:0:77`));
  const table = sessionStorage.getItem("silica-scroll");
  w = setup("http://localhost/about");
  sessionStorage.setItem("silica-scroll", table);
  history.replaceState({ silicaKey: key, silicaIndex: 1 }, "", "/about");
  App.mount_router("root");
  assert.deepEqual(scroll.calls, [[0, 77]]);
});

test("focus moves to the route focus target after a link press, and the title is announced", async () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  click(w, $("to-about"));
  await wait(0);
  assert.equal(document.activeElement, document.querySelector("[data-silica-route-focus]"));
  assert.equal(document.activeElement.getAttribute("tabindex"), "-1");
  const region = document.querySelector("[data-silica-announcer]");
  assert.equal(region.getAttribute("aria-live"), "polite");
  assert.equal(region.textContent, "About");
  history.back();
  await until(() => h1() === "home", "back");
  await wait(0);
  assert.equal(document.activeElement, $("root"));
  assert.equal($("root").getAttribute("tabindex"), "-1");
  assert.equal(region.textContent, "Home");
});

test("a replace does not move focus or announce", async () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  $("to-item").focus();
  click(w, $("to-about-replace"));
  await wait(0);
  assert.equal(document.activeElement, $("to-item") ?? document.activeElement);
  assert.equal(document.querySelector("[data-silica-announcer]"), null);
});

test("preload on intent: pointer enter and focus fetch once, the press uses the answer", async () => {
  const w = setup("http://localhost/home");
  const seen = [];
  globalThis.fetch = async (url, init) => {
    seen.push([url, init.body]);
    return { status: 200, text: async () => '"seven"' };
  };
  App.mount_router("root");
  const link = $("to-data");
  link.dispatchEvent(new w.Event("pointerover", { bubbles: true }));
  assert.deepEqual(seen, [["/api/data", '"7"']]);
  link.focus();
  link.dispatchEvent(new w.Event("pointerover", { bubbles: true }));
  await wait(10);
  assert.equal(seen.length, 1);
  click(w, link);
  await until(() => text("data") === "seven", "kept answer");
  assert.equal(seen.length, 1);
});

test("preload on focus", () => {
  const w = setup("http://localhost/home");
  const seen = [];
  globalThis.fetch = async (url, init) => {
    seen.push([url, init.body]);
    return { status: 200, text: async () => '"seven"' };
  };
  App.mount_router("root");
  $("to-data").focus();
  assert.deepEqual(seen, [["/api/data", '"7"']]);
  $("to-about").dispatchEvent(new w.Event("pointerover", { bubbles: true }));
  assert.equal(seen.length, 1);
});

test("preload on viewport: the observer starts when the link is created", () => {
  const w = setup("http://localhost/home");
  const observed = [];
  let fire;
  globalThis.IntersectionObserver = class {
    constructor(callback) {
      fire = (target) => callback([{ isIntersecting: true, target }]);
    }
    observe(el) {
      observed.push(el.id);
    }
    unobserve() {}
  };
  const seen = [];
  globalThis.fetch = async (url, init) => {
    seen.push([url, init.body]);
    return { status: 200, text: async () => '"nine"' };
  };
  try {
    App.mount_router("root");
    assert.deepEqual(observed, ["to-data-viewport"]);
    fire($("to-data-viewport"));
    assert.deepEqual(seen, [["/api/data", '"9"']]);
  } finally {
    delete globalThis.IntersectionObserver;
  }
});

test("a link with download is left to the browser", () => {
  const w = setup("http://localhost/home");
  App.mount_router("root");
  let prevented;
  w.addEventListener("click", (e) => {
    prevented = e.defaultPrevented;
    e.preventDefault();
  });
  click(w, $("to-home"));
  assert.equal(prevented, false);
  assert.equal(history.state.silicaIndex, 0);
});

let current = { sockets: [], due: 0 };

afterEach(async () => {
  if (current.sockets.length < 1 + current.due) await wait(300);
  for (const ws of current.sockets) ws.handlers.close?.({ code: 4004 });
  current = { sockets: [], due: 0 };
  globalThis.WebSocket = class {
    addEventListener() {}
    send() {}
    close() {}
  };
});

const wait = (ms) => new Promise((r) => setTimeout(r, ms));

function fakeSockets() {
  const sockets = [];
  current = { sockets, due: 0 };
  globalThis.WebSocket = class {
    constructor(url) {
      this.url = url;
      this.sent = [];
      this.handlers = {};
      this.closed = false;
      this.closeCode = undefined;
      this.listens = [];
      sockets.push(this);
    }
    addEventListener(name, f) {
      this.listens.push(name);
      this.handlers[name] = f;
    }
    send(m) {
      this.sent.push(m);
    }
    close(code) {
      this.closed = true;
      this.closeCode = code;
    }
    open() {
      this.handlers.open?.({});
    }
    receive(data) {
      this.handlers.message?.({ data });
    }
    welcome() {
      this.open();
      this.receive('{"t":"hello","v":2}');
    }
    state(seq, value, actions = []) {
      this.receive(
        JSON.stringify({ t: "state", seq, ack: 0, state: value, actions }),
      );
    }
    drop(code = 1006) {
      if (code < 4000 && !this.closed) current.due += 1;
      this.handlers.close?.({ code });
    }
    messages() {
      return this.sent.map((m) => JSON.parse(m));
    }
  };
  return sockets;
}

const live_url = "ws://localhost:8080/_silica/live";

test("live renders server state, sends events, runs host actions", () => {
  const w = setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  const ws = sockets[0];
  assert.equal(ws.url, live_url);
  assert.ok($("stale"), "server HTML stays until the first state");
  ws.welcome();
  ws.state(1, 3, [{ focus: "plus" }]);
  assert.equal($("stale"), null);
  assert.equal(text("n"), "3");
  assert.equal(document.activeElement, $("plus"));
  click(w, $("plus"));
  assert.deepEqual(ws.messages().slice(1), [
    { t: "event", id: 1, event: "inc" },
  ]);
  assert.equal(text("n"), "3");
  ws.state(2, 4);
  assert.equal(text("n"), "4");
  assert.equal(ws.binaryType, "arraybuffer");
  ws.receive(
    new TextEncoder().encode(
      '{"t":"state","seq":3,"ack":1,"state":5,"actions":[]}',
    ).buffer,
  );
  assert.equal(text("n"), "5");
  ws.receive('{"t":"state","sta');
  assert.equal(text("n"), "5");
});

test("one listener per event name on each socket", () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  const names = sockets[0].listens;
  assert.equal(new Set(names).size, names.length);
});

test("live island survives a parent re-render, closes on removal, reopens", async () => {
  const w = setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_island("root");
  assert.equal(sockets.length, 1);
  assert.equal(sockets[0].url, live_url);
  sockets[0].welcome();
  sockets[0].state(1, 7);
  const island = document.querySelector("[data-silica-live]");
  const content = $("n");
  content.expando = "kept";
  assert.equal(text("n"), "7");

  click(w, $("bump"));
  assert.equal(text("bumps"), "1");
  assert.equal(document.querySelector("[data-silica-live]"), island);
  assert.equal($("n"), content);
  assert.equal($("n").expando, "kept");
  assert.equal(sockets.length, 1);
  assert.equal(sockets[0].closed, false);

  click(w, $("plus"));
  assert.deepEqual(sockets[0].messages().slice(1), [
    { t: "event", id: 1, event: "inc" },
  ]);
  assert.equal(text("bumps"), "1");
  click(w, island);
  assert.equal(text("bumps"), "1");
  click(w, $("wrap"));
  assert.equal(text("bumps"), "2");

  click(w, $("toggle"));
  assert.equal(document.querySelector("[data-silica-live]"), null);
  assert.equal(sockets[0].closed, true);
  assert.equal(sockets[0].closeCode, 1000);
  await wait(300);
  assert.equal(sockets.length, 1, "no reconnect after removal");

  click(w, $("toggle"));
  assert.equal(sockets.length, 2);
  assert.equal(sockets[1].closed, false);
  sockets[1].welcome();
  sockets[1].state(1, 1);
  assert.equal(text("n"), "1");
});

test("island with an unregistered channel logs and opens nothing", () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  const logs = [];
  const log = console.log;
  console.log = (...a) => logs.push(a.join(" "));
  try {
    App.mount_unknown_island("root");
  } finally {
    console.log = log;
  }
  assert.equal(sockets.length, 0);
  assert.equal(logs.some((l) => l.includes("no live registered")), true);
});

test("a timer does not fire after its state exited", async () => {
  const w = setup();
  App.mount_timer("root");
  click(w, $("leave"));
  assert.equal(text("phase"), "left");
  await new Promise((r) => setTimeout(r, 80));
  assert.equal(text("phase"), "left");
});

test("a timer restarted by re-entering fires once", async () => {
  const w = setup();
  App.mount_timer("root");
  click(w, $("leave"));
  click(w, $("back"));
  assert.equal(text("phase"), "waiting");
  await until(() => text("phase") === "fired", "fired");
  await new Promise((r) => setTimeout(r, 60));
  assert.equal(text("phase"), "fired");
});

test("the answer of a stopped call is dropped", async () => {
  const w = setup();
  let release;
  globalThis.fetch = () =>
    new Promise((resolve) => {
      release = () => resolve({ status: 200, text: async () => "42" });
    });
  App.mount_basic("root");
  click(w, $("ask"));
  click(w, $("cancel-ask"));
  release();
  await new Promise((r) => setTimeout(r, 60));
  assert.equal(text("answer"), "");
  assert.equal(text("ticks"), "0");
});

test("starting a call again drops the answer of the first", async () => {
  const w = setup();
  const releases = [];
  globalThis.fetch = () => {
    const n = releases.length + 1;
    return new Promise((resolve) => {
      releases.push(() =>
        resolve({ status: 200, text: async () => String(n) }),
      );
    });
  };
  App.mount_basic("root");
  click(w, $("ask"));
  click(w, $("ask"));
  assert.equal(releases.length, 2);
  releases[0]();
  await new Promise((r) => setTimeout(r, 40));
  assert.equal(text("answer"), "");
  releases[1]();
  await until(() => text("answer") === "ok 2", "answer of the second call");
  assert.equal(text("ticks"), "1");
});

const children = (selector) =>
  [...document.querySelectorAll(selector)].map((el) => ({
    n: el.dataset.n,
    clicks: el.querySelector(".child-clicks").textContent,
    prop: el.querySelector(".child-prop").textContent,
  }));

const pressChild = (w, n) =>
  click(w, document.querySelector(`#kids [data-n="${n}"] .child-press`));

test("a child component renders and its machine handles its own events", () => {
  const w = setup();
  App.mount_parent("root");
  const only = () => children("#single .child")[0];
  assert.deepEqual(only(), { n: "0", clicks: "0", prop: "0" });
  click(w, document.querySelector("#single .child-press"));
  click(w, document.querySelector("#single .child-press"));
  assert.equal(only().clicks, "2");
  assert.equal(text("presses"), "0");
  click(w, $("parent-press"));
  assert.equal(text("presses"), "1");
  assert.equal(only().clicks, "2");
});

test("the props event reaches the child when the parent re-renders", () => {
  const w = setup();
  App.mount_parent("root");
  click(w, document.querySelector("#single .child-press"));
  click(w, $("prop-up"));
  click(w, $("prop-up"));
  const [only] = children("#single .child");
  assert.equal(only.prop, "2");
  assert.equal(only.n, "2");
  assert.equal(only.clicks, "1");
  click(w, $("parent-press"));
  assert.equal(children("#single .child")[0].prop, "2");
});

test("function-prop output reaches the parent", () => {
  const w = setup();
  App.mount_parent("root");
  assert.equal(text("heard"), "0");
  click(w, document.querySelector("#single .child-press"));
  assert.equal(text("heard"), "1");
  click(w, document.querySelector("#single .child-press"));
  assert.equal(text("heard"), "2");
  assert.equal(text("presses"), "0");
});

test("keyed children keep their state when reordered", () => {
  const w = setup();
  App.mount_parent("root");
  pressChild(w, 1);
  pressChild(w, 1);
  pressChild(w, 3);
  assert.deepEqual(
    children("#kids .child").map((c) => [c.n, c.clicks]),
    [["1", "2"], ["2", "0"], ["3", "1"]],
  );
  click(w, $("reverse-children"));
  assert.deepEqual(
    children("#kids .child").map((c) => [c.n, c.clicks]),
    [["3", "1"], ["2", "0"], ["1", "2"]],
  );
});

test("reordering keyed components moves their DOM nodes with them", () => {
  const w = setup();
  App.mount_parent("root");
  const nodes = [...document.querySelectorAll("#kids .child")];
  nodes.forEach((node) => (node.expando = node.dataset.n));
  const [first, second, third] = nodes;
  click(w, $("reverse-children"));
  const after = [...document.querySelectorAll("#kids .child")];
  assert.equal(after[0], third);
  assert.equal(after[1], second);
  assert.equal(after[2], first);
  assert.equal(after[0].expando, "3");
});

function trackTimers() {
  const original = { set: globalThis.setTimeout, clear: globalThis.clearTimeout };
  const active = new Set();
  globalThis.setTimeout = (fn, ms, ...rest) => {
    const handle = original.set(
      (...args) => {
        active.delete(handle);
        fn(...args);
      },
      ms,
      ...rest,
    );
    if (ms === 20) active.add(handle);
    return handle;
  };
  globalThis.clearTimeout = (handle) => {
    active.delete(handle);
    original.clear(handle);
  };
  return {
    active,
    restore() {
      globalThis.setTimeout = original.set;
      globalThis.clearTimeout = original.clear;
    },
  };
}

const tick = (label) =>
  Number(document.querySelector(`.tick[data-label="${label}"]`).textContent);

test("two instances of one machine have independent timers; an unrendered child stops its timer", async () => {
  const w = setup();
  const timers = trackTimers();
  try {
    App.mount_tickers("root");
    assert.equal(timers.active.size, 2);
    await until(() => tick("a") >= 2 && tick("b") >= 2, "both tick");
    assert.equal(timers.active.size, 2);

    click(w, $("toggle-a"));
    assert.equal(document.querySelector('.tick[data-label="a"]'), null);
    assert.equal(timers.active.size, 1);
    const before = tick("b");
    await until(() => tick("b") > before, "b still ticks");
    assert.equal(timers.active.size, 1);

    click(w, $("toggle-b"));
    assert.equal(timers.active.size, 0);

    click(w, $("toggle-a"));
    assert.equal(tick("a"), 0);
    assert.equal(timers.active.size, 1);
    click(w, $("toggle-a"));
    assert.equal(timers.active.size, 0);
  } finally {
    for (const label of ["a", "b"]) {
      if (document.querySelector(`.tick[data-label="${label}"]`)) {
        click(w, $(`toggle-${label}`));
      }
    }
    timers.restore();
  }
});

test("island hello: version, csrf from the meta, channel, instance id", async () => {
  setup("http://localhost:8080/", "tok-9");
  const sockets = fakeSockets();
  App.mount_live("root");
  assert.deepEqual(sockets[0].sent, [], "nothing is sent before open");
  sockets[0].open();
  const [hello] = sockets[0].messages();
  assert.equal(hello.t, "hello");
  assert.equal(hello.v, 2);
  assert.equal(hello.csrf, "tok-9");
  assert.equal(hello.sub.island, "counter");
  assert.match(hello.sub.id, /^[0-9a-f]{16}$/);
  sockets[0].drop();
  await wait(300);
  sockets[1].open();
  assert.equal(sockets[1].messages()[0].sub.id, hello.sub.id);
});

test("the island instance id comes from crypto.getRandomValues", () => {
  setup("http://localhost:8080/");
  const original = globalThis.crypto.getRandomValues.bind(globalThis.crypto);
  let calls = 0;
  Object.defineProperty(globalThis, "crypto", {
    configurable: true,
    value: {
      getRandomValues(bytes) {
        calls++;
        bytes.fill(0xab);
        return bytes;
      },
    },
  });
  try {
    const sockets = fakeSockets();
    App.mount_live("root");
    sockets[0].open();
    assert.equal(calls, 1);
    assert.equal(sockets[0].messages()[0].sub.id, "abababababababab");
  } finally {
    Object.defineProperty(globalThis, "crypto", {
      configurable: true,
      value: { getRandomValues: original },
    });
  }
});

test("hello has an empty csrf when the page has no meta tag", () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  sockets[0].open();
  assert.equal(sockets[0].messages()[0].csrf, "");
});

test("a state before Hello_ok is ignored", () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  sockets[0].open();
  sockets[0].state(1, 9);
  assert.ok($("stale"));
  sockets[0].receive('{"t":"hello","v":2}');
  sockets[0].state(1, 9);
  assert.equal(text("n"), "9");
});

test("a state with a seq that is not higher is dropped", () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  const ws = sockets[0];
  ws.welcome();
  ws.state(5, 50);
  ws.state(4, 40);
  assert.equal(text("n"), "50");
  ws.state(5, 55);
  assert.equal(text("n"), "50");
  ws.state(6, 60);
  assert.equal(text("n"), "60");
});

test("after a reconnect a lower seq is accepted", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  sockets[0].welcome();
  sockets[0].state(9, 90);
  sockets[0].drop();
  await wait(300);
  assert.equal(sockets.length, 2);
  assert.equal(sockets[1].url, live_url);
  sockets[1].welcome();
  sockets[1].state(1, 11);
  assert.equal(text("n"), "11");
});

test("reconnect waits for the backoff, and a state resets it", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  sockets[0].welcome();
  sockets[0].state(1, 1);
  sockets[0].drop();
  assert.equal(sockets.length, 1, "not at once");
  await wait(300);
  assert.equal(sockets.length, 2);
  sockets[1].welcome();
  sockets[1].state(1, 2);
  sockets[1].drop();
  await wait(170);
  assert.equal(sockets.length, 3, "attempt went back to 0 after a state");
});

test("the delay grows when connects keep failing", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  sockets[0].drop();
  await wait(170);
  assert.equal(sockets.length, 2);
  sockets[1].drop();
  await wait(150);
  assert.equal(sockets.length, 2, "second delay is at least 200 ms");
  await wait(200);
  assert.equal(sockets.length, 3);
});

test("a socket error reconnects once, even when close follows", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  sockets[0].welcome();
  sockets[0].handlers.error({});
  sockets[0].drop();
  await wait(300);
  assert.equal(sockets.length, 2);
});

for (const code of [4001, 4003, 4004]) {
  test(`close code ${code} is fatal: no reconnect`, async () => {
    setup("http://localhost:8080/");
    const sockets = fakeSockets();
    App.mount_live("root");
    sockets[0].welcome();
    sockets[0].drop(code);
    await wait(300);
    assert.equal(sockets.length, 1);
  });
}

test("close code 4005 means the session is gone: the hook runs once, no reconnect", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  const before = App.gone_live_count();
  App.mount_live_gone("root");
  sockets[0].welcome();
  sockets[0].drop(4005);
  await wait(300);
  assert.equal(sockets.length, 1);
  assert.equal(App.gone_live_count(), before + 1);
});

test("close code 4005 before the hello is answered is the same", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  const before = App.gone_live_count();
  App.mount_live_gone("root");
  sockets[0].drop(4005);
  await wait(300);
  assert.equal(sockets.length, 1);
  assert.equal(App.gone_live_count(), before + 1);
});

test("topic: close code 4005 fails with http 401, keeps the data and runs the hook", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  const before = App.gone_count();
  App.mount_topic_gone("root");
  sockets[0].welcome();
  sockets[0].state(1, 5);
  sockets[0].drop(4005);
  assert.equal(status(), "failed http 401 last 5 seq 0");
  await wait(300);
  assert.equal(sockets.length, 1, "no reconnect");
  assert.equal(App.gone_count(), before + 1);
});

test("session gone, default hook: the page reloads once; a second time within 10 s it gives up and says so", async () => {
  const { VirtualConsole } = await import("jsdom");
  const reloads = [];
  const virtualConsole = new VirtualConsole();
  virtualConsole.on("jsdomError", (e) => reloads.push(e.message));
  const logged = [];
  const original = console.error;
  console.error = (m) => logged.push(String(m));
  try {
    setup("http://localhost:8080/", undefined, virtualConsole);
    let sockets = fakeSockets();
    App.mount_topic("root");
    sockets[0].welcome();
    sockets[0].drop(4005);
    assert.equal(reloads.filter((m) => m.includes("navigation")).length, 1, "first time: reload");
    document.body.innerHTML = '<div id="root"></div>';
    sockets = fakeSockets();
    App.mount_topic("root");
    sockets[0].welcome();
    sockets[0].drop(4005);
    assert.equal(reloads.filter((m) => m.includes("navigation")).length, 1, "second time: no reload");
    assert.ok(logged.some((m) => m.includes("session gone")), logged.join("|"));
  } finally {
    console.error = original;
  }
});

test("an error message is fatal: no reconnect", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  sockets[0].welcome();
  sockets[0].receive('{"t":"error","code":"too_big"}');
  sockets[0].drop(1000);
  await wait(300);
  assert.equal(sockets.length, 1);
});

test("heartbeat: ping after the interval, a pong keeps the socket", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live_fast("root");
  const ws = sockets[0];
  ws.welcome();
  assert.equal(ws.messages().filter((m) => m.t === "ping").length, 0);
  await until(() => ws.messages().some((m) => m.t === "ping"), "ping");
  ws.receive('{"t":"pong"}');
  await until(
    () => ws.messages().filter((m) => m.t === "ping").length === 2,
    "second ping",
  );
  ws.receive('{"t":"pong"}');
  assert.equal(ws.closed, false);
  assert.equal(sockets.length, 1);
});

test("heartbeat: no pong closes the socket and reconnects", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live_fast("root");
  sockets[0].welcome();
  await until(() => sockets[0].closed, "close after missing pong");
  await until(() => sockets.length === 2, "reconnect");
});

test("events are dropped while the socket is down; offline attribute; ids continue", async () => {
  const w = setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  const root = $("root");
  sockets[0].welcome();
  sockets[0].state(1, 1);
  click(w, $("plus"));
  assert.equal(root.hasAttribute("data-silica-offline"), false);
  sockets[0].drop();
  assert.equal(root.hasAttribute("data-silica-offline"), true);
  click(w, $("plus"));
  assert.equal(sockets[0].messages().filter((m) => m.t === "event").length, 1);
  await wait(300);
  const ws = sockets[1];
  ws.open();
  click(w, $("plus"));
  assert.equal(ws.messages().filter((m) => m.t === "event").length, 0);
  ws.receive('{"t":"hello","v":2}');
  assert.equal(root.hasAttribute("data-silica-offline"), true);
  ws.state(1, 2);
  assert.equal(root.hasAttribute("data-silica-offline"), false);
  click(w, $("plus"));
  assert.deepEqual(ws.messages().filter((m) => m.t === "event"), [
    { t: "event", id: 2, event: "inc" },
  ]);
});

const status = () => text("status");
const updates = () => text("updates");

test("topic: hello, loading, ready, stale after loss, fresh after reconnect", async () => {
  setup("http://localhost:8080/", "tok-t");
  const sockets = fakeSockets();
  App.mount_topic("root");
  assert.equal(status(), "loading seq 0");
  assert.equal(updates(), "1");
  const ws = sockets[0];
  assert.equal(ws.url, live_url);
  ws.open();
  assert.deepEqual(ws.messages(), [
    {
      t: "hello",
      v: 2,
      csrf: "tok-t",
      sub: { topic: "news", key: "room" },
    },
  ]);
  ws.receive('{"t":"hello","v":2}');
  ws.state(3, 30);
  assert.equal(status(), "ready 30 seq 3");
  ws.state(3, 31);
  ws.state(2, 32);
  assert.equal(status(), "ready 30 seq 3");
  ws.drop();
  assert.equal(status(), "stale 30 seq 0");
  assert.equal(updates(), "3");
  await wait(300);
  assert.equal(updates(), "3", "stale is delivered once per loss");
  sockets[1].welcome();
  sockets[1].state(1, 40);
  assert.equal(status(), "ready 40 seq 1");
});

test("topic: a loss before any data delivers nothing new", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_topic("root");
  sockets[0].drop();
  assert.equal(status(), "loading seq 0");
  assert.equal(updates(), "1");
});

test("topic: a state that does not decode fails but keeps the socket", () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_topic("root");
  const ws = sockets[0];
  ws.welcome();
  ws.state(1, 5);
  ws.state(2, "text");
  assert.equal(status(), "failed decode last 5 seq 0");
  assert.equal(ws.closed, false);
  ws.state(3, 6);
  assert.equal(status(), "ready 6 seq 3");
});

for (const [code, expected] of [
  [4003, "failed http 403 last 5 seq 0"],
  [4004, "failed http 404 last 5 seq 0"],
  [4001, "failed http 426 last 5 seq 0"],
]) {
  test(`topic: close code ${code} fails with the last data`, () => {
    setup("http://localhost:8080/");
    const sockets = fakeSockets();
    App.mount_topic("root");
    sockets[0].welcome();
    sockets[0].state(1, 5);
    sockets[0].drop(code);
    assert.equal(status(), expected);
  });
}

test("topic: an error message fails with a network error", () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_topic("root");
  sockets[0].welcome();
  sockets[0].receive('{"t":"error","code":"boom"}');
  assert.equal(status(), "failed network boom seq 0");
});

for (const [code, expected] of [
  [4001, "failed http 426"],
  [4003, "failed http 403"],
  [4004, "failed http 404"],
]) {
  test(`topic: close code ${code} is fatal and fails with ${expected}`, async () => {
    setup("http://localhost:8080/");
    const sockets = fakeSockets();
    App.mount_topic("root");
    sockets[0].welcome();
    sockets[0].drop(code);
    assert.equal(status(), `${expected} seq 0`);
    await wait(300);
    assert.equal(sockets.length, 1, "no reconnect");
  });
}

test("a close code that is not fatal reconnects", async () => {
  setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_live("root");
  sockets[0].welcome();
  sockets[0].drop(1011);
  await until(() => sockets.length === 2, "reconnect after a server error close");
});

test("topic: stop closes the socket and no update arrives afterwards", async () => {
  const w = setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_topic("root");
  const ws = sockets[0];
  ws.welcome();
  ws.state(1, 5);
  assert.equal(updates(), "2");
  click(w, $("leave"));
  assert.equal(ws.closed, true);
  assert.equal(ws.closeCode, 1000);
  ws.state(2, 6);
  ws.drop();
  await wait(300);
  assert.equal(sockets.length, 1, "no reconnect after stop");
  assert.equal(updates(), "2");
  assert.equal(status(), "ready 5 seq 1");
});

test("topic: starting again after a stop opens a new socket", () => {
  const w = setup("http://localhost:8080/");
  const sockets = fakeSockets();
  App.mount_topic("root");
  click(w, $("leave"));
  click(w, $("return"));
  assert.equal(sockets.length, 2);
  assert.equal(sockets[1].closed, false);
});
