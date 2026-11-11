import { test } from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";

const bundle = new URL("../../devtools/dist/silica-dev.js", import.meta.url);
let loads = 0;

class FakeSocket {
  static opened = [];
  constructor(url) {
    this.url = url;
    this.listeners = {};
    FakeSocket.opened.push(this);
  }
  addEventListener(name, listener) {
    (this.listeners[name] ??= []).push(listener);
  }
  receive(message) {
    for (const listener of this.listeners.message ?? []) listener({ data: JSON.stringify(message) });
  }
  close() {
    for (const listener of this.listeners.close ?? []) listener({});
  }
}

async function start(head = "") {
  const dom = new JSDOM(`<head>${head}</head><body><div id="silica-root"><h1>App</h1></div></body>`, {
    url: "http://127.0.0.1:8080/page",
  });
  const fakeLocation = {
    protocol: "http:",
    host: "127.0.0.1:8080",
    href: "http://127.0.0.1:8080/page",
    reloads: 0,
    reload() {
      this.reloads++;
    },
  };
  FakeSocket.opened = [];
  Object.assign(globalThis, {
    window: dom.window,
    document: dom.window.document,
    location: fakeLocation,
    WebSocket: FakeSocket,
  });
  loads++;
  await import(`${bundle.href}?load=${loads}`);
  assert.equal(FakeSocket.opened.length, 1);
  return { socket: FakeSocket.opened[0], location: fakeLocation, document: dom.window.document };
}

const shadow = (document) => document.getElementById("silica-dev")?.shadowRoot;
const overlay = (document) => shadow(document)?.querySelector("[data-silica-dev-overlay]") ?? null;

const failed = {
  t: "build",
  status: "failed",
  errors: [
    {
      file: "app/shared/counter.mlx",
      line: 12,
      col: 5,
      end_line: 12,
      end_col: 9,
      severity: "error",
      message: "Unbound value cont",
      excerpt: [
        [10, "let view state ="],
        [11, "  <div>"],
        [12, "    (cont state)"],
        [13, "  </div>"],
        [14, ""],
      ],
    },
    { severity: "error", message: "esbuild: no such file" },
  ],
};

test("dev client: it connects to the dev socket of the page host", async () => {
  const { socket } = await start();
  assert.equal(socket.url, "ws://127.0.0.1:8080/_silica/dev");
});

test("dev client: a failed build shows each error with file, line and source lines", async () => {
  const { socket, document } = await start();
  socket.receive(failed);
  const root = overlay(document);
  assert.ok(root, "the overlay shows");
  const errors = root.querySelectorAll(".error");
  assert.equal(errors.length, 2);
  assert.equal(errors[0].getAttribute("data-file"), "app/shared/counter.mlx");
  assert.equal(errors[0].querySelector(".where").textContent, "app/shared/counter.mlx:12:5");
  assert.equal(errors[0].querySelector(".message").textContent, "error: Unbound value cont");
  const lines = [...errors[0].querySelectorAll(".line")];
  assert.deepEqual(
    lines.map((line) => line.getAttribute("data-line")),
    ["10", "11", "12", "13", "14"],
  );
  assert.equal(errors[0].querySelector(".line[data-error]").textContent, "   12 |     (cont state)");
  assert.equal(errors[1].querySelector(".where"), null, "an error with no location shows its message only");
  assert.equal(shadow(document).querySelector(".indicator").textContent, "Build failed");
  assert.equal(document.querySelector("h1").textContent, "App", "the app stays under the overlay");
});

test("dev client: the overlay goes when a build succeeds", async () => {
  const { socket, document, location } = await start();
  socket.receive(failed);
  socket.receive({ t: "build", status: "started" });
  socket.receive({ t: "progress", complete: 3, remaining: 7 });
  assert.equal(shadow(document).querySelector(".indicator").textContent, "Building 3/10");
  socket.receive({ t: "build", status: "ok" });
  assert.equal(overlay(document), null);
  assert.equal(shadow(document).querySelector(".indicator"), null);
  assert.equal(location.reloads, 0);
});

test("dev client: reload reloads the page", async () => {
  const { socket, location } = await start();
  socket.receive({ t: "reload", what: "page" });
  assert.equal(location.reloads, 1);
});

test("dev client: a stylesheet reload swaps the link and keeps the page", async () => {
  const { socket, document, location } = await start(
    '<link rel="stylesheet" href="/static/app-AAAA.css"><link rel="stylesheet" href="/static/other.css">',
  );
  socket.receive({ t: "reload", what: "css", css: [{ from: "/static/app-AAAA.css", to: "/static/app-BBBB.css" }] });
  assert.deepEqual(
    [...document.querySelectorAll("link")].map((link) => link.getAttribute("href")),
    ["/static/app-BBBB.css", "/static/other.css"],
  );
  assert.equal(location.reloads, 0);
});

test("dev client: a crashed server shows its output until a server starts", async () => {
  const { socket, document } = await start();
  socket.receive({ t: "server", status: "crashed", code: "exit 2", output: "Fatal error: boom" });
  assert.match(overlay(document).textContent, /The server stopped \(exit 2\)/);
  assert.match(overlay(document).textContent, /Fatal error: boom/);
  socket.receive({ t: "server", status: "restarted", pid: 3, ms: 20 });
  assert.equal(overlay(document), null);
});

test("dev client: after the socket drops, it connects again and reloads on a good build", async () => {
  const { socket, location } = await start();
  socket.receive({ t: "build", status: "ok" });
  socket.close();
  await new Promise((resolve) => setTimeout(resolve, 1100));
  assert.equal(FakeSocket.opened.length, 2, "a second socket");
  FakeSocket.opened[1].receive({ t: "build", status: "ok" });
  assert.equal(location.reloads, 1);
});
