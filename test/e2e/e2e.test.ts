import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, type ChildProcess } from "node:child_process";
import { createServer, connect as tcpConnect, type Server as TcpServer, type Socket } from "node:net";
import { request as httpRequest } from "node:http";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type Page, type WebSocket } from "playwright-core";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const exe = resolve(root, "_build/default/example/server/main.exe");
const dist = resolve(root, "_build/default/example/client/dist");
const metafile = resolve(root, "_build/default/example/client/meta.json");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";

let server: ChildProcess;
let browser: Browser;
let base: string;
let serverLog = "";

const startServer = (port: number): ChildProcess => {
  const child = spawn(exe, ["--port", String(port), "--static", dist, "--metafile", metafile], { cwd: root });
  child.stdout?.on("data", (d: Buffer) => (serverLog += d.toString()));
  child.stderr?.on("data", (d: Buffer) => (serverLog += d.toString()));
  return child;
};

const freePort = (): Promise<number> =>
  new Promise((ok, fail) => {
    const s = createServer();
    s.once("error", fail);
    s.listen(0, "127.0.0.1", () => {
      const addr = s.address();
      const port = typeof addr === "object" && addr ? addr.port : 0;
      s.close(() => ok(port));
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
  server = startServer(port);
  await waitUp(base + "/");
  browser = await chromium.launch({ executablePath: chromePath, headless: true });
});

after(async () => {
  await browser?.close();
  server?.kill("SIGTERM");
});


class Proxy {
  private sockets = new Set<Socket>();
  private paused = false;
  private server: TcpServer;
  constructor(target: number) {
    this.server = createServer((client) => {
      if (this.paused) {
        client.destroy();
        return;
      }
      const upstream = tcpConnect(target, "127.0.0.1");
      this.sockets.add(client);
      this.sockets.add(upstream);
      client.pipe(upstream);
      upstream.pipe(client);
      const drop = () => {
        client.destroy();
        upstream.destroy();
        this.sockets.delete(client);
        this.sockets.delete(upstream);
      };
      client.on("error", drop);
      upstream.on("error", drop);
      client.on("close", drop);
      upstream.on("close", drop);
    });
  }
  listen(): Promise<number> {
    return new Promise((ok) =>
      this.server.listen(0, "127.0.0.1", () => {
        const addr = this.server.address();
        ok(typeof addr === "object" && addr ? addr.port : 0);
      }),
    );
  }
  pause(): void {
    this.paused = true;
    for (const socket of this.sockets) socket.destroy();
    this.sockets.clear();
  }
  resume(): void {
    this.paused = false;
  }
  close(): void {
    this.pause();
    this.server.close();
  }
}

type Reply = { status: number; headers: Record<string, string | string[] | undefined>; body: string };

const raw = (method: string, path: string, headers: Record<string, string>, body = ""): Promise<Reply> =>
  new Promise((ok, fail) => {
    const url = new URL(base + path);
    const req = httpRequest(
      { agent: false, host: url.hostname, port: url.port, path: url.pathname + url.search, method, headers: { ...headers, "content-length": String(Buffer.byteLength(body)) } },
      (res) => {
        let text = "";
        res.setEncoding("utf8");
        res.on("data", (chunk: string) => (text += chunk));
        res.on("end", () => ok({ status: res.statusCode ?? 0, headers: res.headers, body: text }));
      },
    );
    req.on("error", fail);
    req.end(body);
  });

const upgradeStatus = (headers: Record<string, string>): Promise<number> =>
  new Promise((ok, fail) => {
    const url = new URL(base + "/_silica/live");
    const req = httpRequest({
      agent: false,
      host: url.hostname,
      port: url.port,
      path: url.pathname,
      headers: {
        connection: "Upgrade",
        upgrade: "websocket",
        "sec-websocket-key": "dGhlIHNhbXBsZSBub25jZQ==",
        "sec-websocket-version": "13",
        ...headers,
      },
    });
    req.on("upgrade", (res, socket) => {
      socket.destroy();
      ok(res.statusCode ?? 0);
    });
    req.on("response", (res) => {
      res.resume();
      ok(res.statusCode ?? 0);
    });
    req.on("error", fail);
    req.end();
  });

const open = async (path: string): Promise<Page> => {
  const page = await browser.newPage();
  page.setDefaultTimeout(5000);
  await page.goto(base + path);
  return page;
};

const counterText = (page: Page) => page.locator("[data-silica-live=counter] span");
const interactive = async (page: Page) => {
  await page.evaluate(() => {
    (window as unknown as { __marker?: number }).__marker = 1;
  });
};
const marker = (page: Page) =>
  page.evaluate(() => (window as unknown as { __marker?: number }).__marker);

describe("silica example", () => {
  test("server renders HTML, then the app becomes interactive", async () => {
    const res = await fetch(base + "/");
    assert.equal(res.status, 200);
    const html = await res.text();
    assert.match(html, /Silica example/);
    assert.match(html, /What needs doing\?/);
    assert.match(html, /data-silica-live="counter"/);
    assert.match(html, /\/static\/client_main-[A-Z0-9]{8,}\.js/);

    const page = await open("/");
    await page.getByLabel("New todo").fill("probe");
    assert.equal(await page.getByLabel("New todo").inputValue(), "probe");
    await page.getByLabel("New todo").fill("");
    await page.close();
  });

  test("todo: add, focus returns, toggle, delete", async () => {
    const page = await open("/");
    const input = page.getByLabel("New todo");
    await input.fill("buy milk");
    await page.getByRole("button", { name: "Add", exact: true }).click();
    const todo = page.locator("li button", { hasText: "buy milk" });
    await todo.waitFor();
    assert.equal(await todo.textContent(), "[ ] buy milk");
    assert.equal(await input.inputValue(), "");
    assert.equal(
      await page.evaluate(() => (document.activeElement as HTMLInputElement | null)?.placeholder),
      "What needs doing?",
    );

    await input.fill("second");
    await input.press("Enter");
    await page.locator("li button", { hasText: "second" }).waitFor();
    assert.equal(await page.locator("li").count(), 2);

    await todo.click();
    await page.locator("li button", { hasText: "[x] buy milk" }).waitFor();
    assert.match((await todo.getAttribute("class")) ?? "", /done/);
    await todo.click();
    await page.locator("li button", { hasText: "[ ] buy milk" }).waitFor();

    await todo.dblclick();
    await page.locator("li button", { hasText: "buy milk" }).waitFor({ state: "detached" });
    assert.equal(await page.locator("li").count(), 1);
    assert.equal(await page.locator("li button", { hasText: "second" }).count(), 1);
    await page.close();
  });

  test("live counter over WebSocket, per connection", async () => {
    const page = await open("/");
    const counter = counterText(page);
    const dec = page.getByRole("button", { name: "Decrement" });
    const inc = page.getByRole("button", { name: "Increment" });
    await counter.waitFor();
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "0");
    assert.equal(await dec.isDisabled(), true);
    await inc.click();
    await inc.click();
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "2");
    assert.equal(await dec.isDisabled(), false);
    await dec.click();
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "1");
    await dec.click();
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "0");
    assert.equal(await dec.isDisabled(), true);
    await inc.click();
    await inc.click();
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "2");

    const other = await open("/");
    await other.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "0");
    assert.equal(await counterText(other).textContent(), "0");
    assert.equal(await counterText(page).textContent(), "2");
    await other.getByRole("button", { name: "Increment" }).click();
    await other.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "1");
    assert.equal(await counterText(page).textContent(), "2");
    await other.close();
    await page.close();
  });

  test("presses on the Live counter before its first state count", async () => {
    const page = await browser.newPage();
    page.setDefaultTimeout(5000);
    let release = () => {};
    const released = new Promise<void>((ok) => (release = ok));
    await page.routeWebSocket(/\/_silica\/live/, (socket) => {
      const server = socket.connectToServer();
      let island = false;
      let holding = true;
      const held: (string | Buffer)[] = [];
      socket.onMessage((message) => {
        if (String(message).includes('"island"')) island = true;
        server.send(message);
      });
      server.onMessage((message) => {
        if (island && holding) held.push(message);
        else socket.send(message);
      });
      void released.then(() => {
        holding = false;
        for (const message of held) socket.send(message);
      });
    });
    await page.goto(base + "/");
    await page.locator("#silica-root[data-silica-ready]").waitFor();
    const inc = page.getByRole("button", { name: "Increment" });
    for (let press = 0; press < 3; press++) await inc.click();
    assert.equal(await counterText(page).textContent(), "0", "no state yet");
    release();
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "3");
    await page.close();
  });

  test("a todo typed and added before the code of the page runs is added", async () => {
    const page = await browser.newPage();
    page.setDefaultTimeout(5000);
    let release = () => {};
    const released = new Promise<void>((ok) => (release = ok));
    await page.route(/\/static\/client_main-[A-Z0-9]+\.js$/, async (route) => {
      await released;
      await route.continue();
    });
    await page.goto(base + "/", { waitUntil: "commit" });
    await page.getByLabel("New todo").fill("early milk");
    await page.getByRole("button", { name: "Add", exact: true }).click();
    assert.equal(await page.locator("#silica-root[data-silica-ready]").count(), 0, "the code has not run");
    release();
    await page.locator("li button", { hasText: "early milk" }).waitFor();
    assert.equal(await page.locator("li").count(), 1, "added once");
    await page.close();
  });

  test("link navigation without reload; remote item; failing item", async () => {
    const page = await open("/");
    await interactive(page);
    await page.locator("a", { hasText: "Item 1" }).first().click();
    await page.waitForURL(base + "/item/1");
    await page.getByRole("heading", { name: "Item 1", exact: true, level: 2 }).waitFor();
    await page.getByText("$2.00").waitFor();
    await page.locator("li", { hasText: "demo" }).waitFor();
    assert.equal(await marker(page), 1);
    assert.equal(await page.title(), "Item 1");

    await page.locator("a", { hasText: "Item 2" }).first().click();
    await page.waitForURL(base + "/item/2");
    await page.getByText("$3.00").waitFor();
    assert.equal(await marker(page), 1);

    await page.close();

    const bad = await open("/item/-1");
    await bad.getByText("no item -1").waitFor();
    assert.equal(await bad.locator("article").count(), 0);
    await bad.close();
  });

  test("back, forward, About back button, titles", async () => {
    const page = await open("/");
    await interactive(page);
    assert.equal(await page.title(), "Silica example");
    await page.locator("a", { hasText: "Item 1" }).first().click();
    await page.waitForURL(base + "/item/1");
    await page.getByText("$2.00").waitFor();

    await page.goBack();
    await page.waitForURL(base + "/");
    await page.getByRole("heading", { name: "Todos" }).waitFor();
    assert.equal(await page.title(), "Silica example");
    assert.equal(await marker(page), 1);

    await page.goForward();
    await page.waitForURL(base + "/item/1");
    await page.getByText("$2.00").waitFor();
    assert.equal(await page.title(), "Item 1");
    assert.equal(await marker(page), 1);

    await page.locator("a", { hasText: "About" }).first().click();
    await page.waitForURL(base + "/about");
    await page.getByRole("heading", { name: "About", level: 1 }).waitFor();
    assert.equal(await page.title(), "About");
    await page.getByRole("button", { name: "Back" }).click();
    await page.waitForURL(base + "/item/1");
    await page.getByText("$2.00").waitFor();
    assert.equal(await marker(page), 1);
    await page.close();
  });

  test("the page sockets close on leaving Home and reopen on return", async () => {
    const page = await browser.newPage();
    page.setDefaultTimeout(5000);
    const sockets: { ws: WebSocket; closed: boolean }[] = [];
    page.on("websocket", (ws) => {
      const entry = { ws, closed: false };
      ws.on("close", () => (entry.closed = true));
      sockets.push(entry);
    });
    await page.goto(base + "/");
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "0");
    await page.waitForFunction(() => document.querySelector("#votes-count")?.textContent !== "...");
    assert.equal(sockets.length, 2, "one socket for the island and one for the topic");
    for (const entry of sockets) {
      assert.match(entry.ws.url(), /\/_silica\/live$/);
      assert.equal(entry.closed, false);
    }

    await page.locator("a", { hasText: "About" }).first().click();
    await page.waitForURL(base + "/about");
    await page.waitForFunction(() => !document.querySelector("[data-silica-live]"));
    await page.waitForTimeout(300);
    assert.deepEqual(
      sockets.map((entry) => entry.closed),
      [true, true],
    );
    assert.equal(sockets.length, 2);

    await page.locator("a", { hasText: "Home" }).first().click();
    await page.waitForURL(base + "/");
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "0");
    await page.waitForFunction(() => document.querySelector("#votes-count")?.textContent !== "...");
    assert.equal(sockets.length, 4);
    assert.deepEqual(
      sockets.map((entry) => entry.closed),
      [true, true, false, false],
    );
    await page.close();
  });

  test("reload renders the same route", async () => {
    const page = await open("/item/2");
    await page.getByText("$3.00").waitFor();
    await page.reload();
    assert.equal(new URL(page.url()).pathname, "/item/2");
    await page.getByText("$3.00").waitFor();
    assert.equal(await page.title(), "Item 2");

    const about = await open("/about");
    await about.reload();
    await about.getByRole("heading", { name: "About", level: 1 }).waitFor();
    assert.equal(new URL(about.url()).pathname, "/about");
    await about.close();
    await page.close();
  });

  test("unknown URL answers 404 with not_found", async () => {
    const res = await fetch(base + "/no/such/page");
    assert.equal(res.status, 404);
    assert.match(await res.text(), /Not found/);

    const page = await browser.newPage();
    const nav = await page.goto(base + "/no/such/page");
    assert.equal(nav?.status(), 404);
    await page.getByRole("heading", { name: "Not found" }).waitFor();
    await page.close();
  });

  test("hydration keeps the server nodes, the focus and the text typed before it", async () => {
    const page = await browser.newPage();
    page.setDefaultTimeout(5000);
    let release: () => void = () => {};
    const gate = new Promise<void>((ok) => (release = ok));
    await page.route("**/static/client_main-*.js", async (route) => {
      await gate;
      await route.continue();
    });
    await page.goto(base + "/", { waitUntil: "commit" });
    const input = page.getByLabel("New todo");
    await input.waitFor({ state: "attached" });
    await input.fill("early");
    await page.evaluate(() => {
      const root = document.getElementById("silica-root")!;
      const w = window as unknown as {
        __before: Element[];
        __removed: { target: Node; removed: Node }[];
      };
      w.__before = [...root.querySelectorAll("*")];
      w.__removed = [];
      new MutationObserver((records) => {
        for (const record of records)
          for (const removed of record.removedNodes)
            if (removed.nodeType === 1) w.__removed.push({ target: record.target, removed });
      }).observe(root, { childList: true, subtree: true });
    });
    assert.equal(
      await page.evaluate(() => document.getElementById("votes-count")?.textContent),
      "...",
      "the client has not run yet",
    );
    release();
    await page.waitForFunction(() => /^[0-9]+$/.test(document.getElementById("votes-count")?.textContent ?? ""));
    const result = await page.evaluate(() => {
      const w = window as unknown as {
        __before: Element[];
        __removed: { target: Node; removed: Node }[];
      };
      const inIsland = (n: Node) => !!(n as Element).closest?.("[data-silica-live]");
      return {
        checked: w.__before.length,
        replaced: w.__before.filter((el) => !el.isConnected && !inIsland(el)).length,
        removedOutsideIsland: w.__removed.filter((r) => !inIsland(r.target)).length,
        sameInput:
          document.activeElement === w.__before.find((el) => el.getAttribute("aria-label") === "New todo"),
        activeLabel: document.activeElement?.getAttribute("aria-label"),
      };
    });
    assert.ok(result.checked > 10, `checked ${result.checked} elements`);
    assert.equal(result.replaced, 0, "no element was replaced after load");
    assert.equal(result.removedOutsideIsland, 0, "no element was removed after load");
    assert.equal(result.sameInput, true, `focus stayed on the input (active: ${result.activeLabel})`);
    assert.equal(await input.inputValue(), "early", "the text typed before hydration is kept");
    await input.press("Enter");
    await page.getByRole("button", { name: "[ ] early" }).waitFor();
    assert.equal(await input.inputValue(), "", "the machine had the typed text and cleared it on add");
    await page.getByLabel("New todo").fill("still works");
    assert.equal(await page.getByLabel("New todo").inputValue(), "still works");
    await page.close();
  });

  test("the settle step embeds the answer and the browser does not ask again", async () => {
    const res = await fetch(base + "/item/2");
    const html = await res.text();
    assert.match(html, /<script type="application\/json" id="silica-answers" nonce="[^"]+">/);
    assert.match(html, /\$3\.00/, "the settled state is in the HTML");
    assert.doesNotMatch(html, /Loading item/);

    const page = await browser.newPage();
    page.setDefaultTimeout(5000);
    const asked: string[] = [];
    page.on("request", (request) => {
      if (request.url().includes("/api/item")) asked.push(request.postData() ?? "");
    });
    await page.goto(base + "/item/2");
    await interactive(page);
    await page.getByText("$3.00").waitFor();
    await page.waitForTimeout(300);
    assert.deepEqual(asked, [], "no call ran on load");
    await page.locator("a", { hasText: "Item 1" }).first().click();
    await page.waitForURL(base + "/item/1");
    await page.getByText("$2.00").waitFor();
    assert.deepEqual(asked, ["1"], "a later navigation still calls the server");
    assert.equal(await marker(page), 1);
    await page.close();
  });

  test("live reconnect: offline shows old data, a press made offline counts after the reconnect, and the island state is kept", async () => {
    const proxy = new Proxy(Number(new URL(base).port));
    const proxied = await proxy.listen();
    const page = await browser.newPage();
    page.setDefaultTimeout(10000);
    try {
      await page.goto(`http://127.0.0.1:${proxied}/`);
      const votes = () => page.locator("#votes-count").textContent();
      await page.waitForFunction(() => document.querySelector("#votes-count")?.textContent !== "...");
      const start = Number(await votes());
      const inc = page.getByRole("button", { name: "Increment" });
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "0");
      await inc.click();
      await inc.click();
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "2");
      await page.locator("#vote").click();
      await page.waitForFunction((n) => document.querySelector("#votes-count")?.textContent === String(n), start + 1);

      proxy.pause();
      await page.locator("#votes-offline").waitFor();
      await page.waitForFunction(() => document.querySelector("[data-silica-live]")?.hasAttribute("data-silica-offline"));
      assert.equal(await votes(), String(start + 1), "old data stays while offline");
      await inc.click();
      assert.equal(await counterText(page).textContent(), "2", "an event made while offline is held");

      const other = await open("/");
      await other.waitForFunction(() => document.querySelector("#votes-count")?.textContent !== "...");
      await other.locator("#vote").click();
      await other.locator("#vote").click();
      await other.waitForFunction((n) => document.querySelector("#votes-count")?.textContent === String(n), start + 3);
      await other.close();
      await page.waitForTimeout(1200);
      assert.equal(await votes(), String(start + 1), "no change reaches the offline page");

      proxy.resume();
      await page.locator("#votes-offline").waitFor({ state: "detached" });
      await page.waitForFunction((n) => document.querySelector("#votes-count")?.textContent === String(n), start + 3);
      await page.waitForFunction(() => !document.querySelector("[data-silica-live]")?.hasAttribute("data-silica-offline"));
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "3");
      assert.equal(
        await counterText(page).textContent(),
        "3",
        "the island state was kept on the server, and the held press counted once",
      );
      await inc.click();
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "4");
    } finally {
      await page.close();
      proxy.close();
    }
  });

  test("a vote in one page reaches another page through the topic", async () => {
    const a = await open("/");
    const b = await open("/");
    await a.waitForFunction(() => document.querySelector("#votes-count")?.textContent !== "...");
    await b.waitForFunction(() => document.querySelector("#votes-count")?.textContent !== "...");
    const before = Number(await b.locator("#votes-count").textContent());
    await a.locator("#vote").click();
    await b.waitForFunction((n) => document.querySelector("#votes-count")?.textContent === String(n), before + 1);
    await a.close();
    await b.close();
  });

  test("server package: cookie, Origin, CSRF token and cache headers over real HTTP", async () => {
    const page = await raw("GET", "/", {});
    assert.equal(page.status, 200);
    const cookie = String(page.headers["set-cookie"]);
    assert.match(cookie, /^silica_sid=[0-9a-f]{32}; /);
    assert.match(cookie, /HttpOnly/);
    assert.match(cookie, /SameSite=Lax/);
    assert.doesNotMatch(cookie, /Max-Age|Expires/i);
    assert.equal(page.headers["cache-control"], "private, no-store");
    assert.equal(page.headers["x-content-type-options"], "nosniff");
    assert.match(String(page.headers["content-security-policy"]), /script-src 'self' 'nonce-[^']+'/);
    const sid = cookie.split(";")[0]!;
    const token = /name="silica-csrf" content="([^"]+)"/.exec(page.body)![1]!;
    const asset = /\/static\/client_main-[A-Z0-9]+\.js/.exec(page.body)![0];

    const call = (headers: Record<string, string>) =>
      raw("POST", "/api/item", { "content-type": "application/json", cookie: sid, "x-silica-csrf": token, origin: base, ...headers }, "1");
    const ok = await call({});
    assert.equal(ok.status, 200);
    assert.match(ok.body, /"title":"Item 1"/);
    assert.equal(ok.headers["set-cookie"], undefined, "no new cookie for a known session");
    assert.equal((await call({ origin: "http://evil.test" })).status, 403, "another Origin");
    assert.equal((await call({ origin: base + ".evil.test" })).status, 403, "an Origin that only starts the same");
    assert.equal((await call({ "x-silica-csrf": "forged" })).status, 403, "a wrong token");
    assert.equal((await call({ "content-type": "text/plain" })).status, 415, "a form content type");
    assert.equal((await call({ cookie: "silica_sid=00000000000000000000000000000000" })).status, 403, "an unknown session");
    const noOrigin = await raw("POST", "/api/item", { "content-type": "application/json", cookie: sid, "x-silica-csrf": token }, "1");
    assert.equal(noOrigin.status, 403, "no Origin");

    const hashed = await raw("GET", asset, {});
    assert.equal(hashed.status, 200);
    assert.equal(hashed.headers["cache-control"], "public, max-age=31536000, immutable");
    assert.match(String(hashed.headers["content-type"]), /javascript/);

    assert.equal(await upgradeStatus({ cookie: sid, origin: base }), 101, "a good upgrade");
    assert.equal(await upgradeStatus({ cookie: sid, origin: "http://evil.test" }), 403, "an upgrade from another site");
    assert.equal(await upgradeStatus({ cookie: sid }), 403, "an upgrade with no Origin");
    assert.equal(await upgradeStatus({ origin: base }), 101, "an upgrade with no session is accepted, then closed with 4005");
  });
});

const historyIndex = (page: Page) =>
  page.evaluate(() => (history.state as { silicaIndex?: number } | null)?.silicaIndex);
const at = (page: Page, path: string, index: number) =>
  page.waitForFunction(
    ([path, index]) =>
      location.pathname === path &&
      (history.state as { silicaIndex?: number } | null)?.silicaIndex === index,
    [path, index] as const,
  );
const scrollY = (page: Page) => page.evaluate(() => Math.round(window.scrollY));
const pressLink = (page: Page, href: string) =>
  page.evaluate((href) => (document.querySelector(`nav a[href="${href}"]`) as HTMLElement).click(), href);

describe("silica router", () => {
  test("a block stops back and forward: the history position and the URL stay", async () => {
    const page = await open("/");
    await interactive(page);
    await page.locator("a", { hasText: "Edit" }).first().click();
    await at(page, "/edit", 1);
    await page.locator("a", { hasText: "About" }).first().click();
    await at(page, "/about", 2);
    await page.goBack();
    await at(page, "/edit", 1);
    await page.getByRole("textbox", { name: "Draft" }).fill("unsaved");

    const dialog = page.getByRole("alertdialog");
    await page.goForward();
    await dialog.waitFor();
    await page.waitForTimeout(200);
    await at(page, "/edit", 1);
    await page.getByRole("button", { name: "Stay" }).click();
    await dialog.waitFor({ state: "detached" });

    await page.goBack();
    await dialog.waitFor();
    await page.waitForTimeout(200);
    await at(page, "/edit", 1);
    assert.equal(await page.getByRole("textbox", { name: "Draft" }).inputValue(), "unsaved");

    await page.locator("a", { hasText: "About" }).first().click();
    await page.waitForTimeout(200);
    await at(page, "/edit", 1);

    await page.getByRole("button", { name: "Leave" }).click();
    await at(page, "/about", 2);
    await page.goBack();
    await at(page, "/edit", 1);
    await page.goBack();
    await at(page, "/", 0);
    assert.equal(await marker(page), 1, "no page load");
    await page.close();
  });

  test("a Leave after back goes back, and forward still works", async () => {
    const page = await open("/");
    await interactive(page);
    await page.locator("a", { hasText: "Edit" }).first().click();
    await at(page, "/edit", 1);
    await page.getByRole("textbox", { name: "Draft" }).fill("unsaved");
    await page.goBack();
    await page.getByRole("alertdialog").waitFor();
    await page.getByRole("button", { name: "Leave" }).click();
    await at(page, "/", 0);
    await page.getByRole("heading", { name: "Todos" }).waitFor();
    await page.goForward();
    await at(page, "/edit", 1);
    assert.equal(await page.getByRole("textbox", { name: "Draft" }).inputValue(), "");
    assert.equal(await marker(page), 1);
    await page.close();
  });

  test("a guard Cancel on back keeps the history position", async () => {
    const page = await open("/locked");
    await interactive(page);
    await page.locator("a", { hasText: "Home" }).first().click();
    await at(page, "/", 1);
    await page.getByRole("heading", { name: "Todos" }).waitFor();
    await page.goBack();
    await page.waitForTimeout(300);
    await at(page, "/", 1);
    await page.getByRole("heading", { name: "Todos" }).waitFor();
    assert.equal(await page.locator("h1", { hasText: "Locked" }).count(), 0);
    assert.equal(await marker(page), 1);
    await page.close();
  });

  test("leaving the page asks while a block is held", async () => {
    const page = await open("/edit");
    await interactive(page);
    const dialogs: string[] = [];
    page.on("dialog", async (dialog) => {
      dialogs.push(dialog.type());
      await dialog.dismiss();
    });
    await page.getByRole("textbox", { name: "Draft" }).click();
    await page.keyboard.type("unsaved");
    await page.evaluate(() => location.assign("/about"));
    for (let i = 0; i < 100 && dialogs.length === 0; i++) await new Promise((r) => setTimeout(r, 20));
    assert.deepEqual(dialogs, ["beforeunload"]);
    await page.waitForTimeout(200);
    assert.equal(new URL(page.url()).pathname, "/edit", "dismissing the dialog keeps the page");
    assert.equal(await marker(page), 1);

    await page.getByRole("button", { name: "Save" }).click();
    await page.evaluate(() => location.assign("/about"));
    await page.waitForURL(base + "/about");
    assert.deepEqual(dialogs, ["beforeunload"], "no dialog once the draft is saved");
    await page.close();
  });

  test("scroll positions come back on back and forward, and after a reload", async () => {
    const page = await browser.newPage({ viewport: { width: 800, height: 400 } });
    page.setDefaultTimeout(5000);
    await page.goto(base + "/");
    await interactive(page);
    await pressLink(page, "/about");
    await at(page, "/about", 1);
    assert.equal(await scrollY(page), 0);
    await page.evaluate(() => window.scrollTo(0, 700));
    await page.waitForFunction(() => Math.round(window.scrollY) === 700);

    await page.goBack();
    await at(page, "/", 0);
    await page.getByRole("heading", { name: "Todos" }).waitFor();
    await page.goForward();
    await at(page, "/about", 1);
    await page.waitForFunction(() => Math.round(window.scrollY) === 700);

    await pressLink(page, "/");
    await at(page, "/", 2);
    assert.equal(await scrollY(page), 0, "a push to another path starts at the top");
    await page.goBack();
    await at(page, "/about", 1);
    await page.waitForFunction(() => Math.round(window.scrollY) === 700);
    assert.equal(await marker(page), 1);

    await page.evaluate(() => window.scrollTo(0, 650));
    await page.waitForFunction(() => Math.round(window.scrollY) === 650);
    await page.reload();
    assert.equal(await marker(page), undefined, "a real reload");
    await page.waitForFunction(() => document.getElementById("silica-root")?.hasAttribute("data-silica-ready"));
    await page.waitForFunction(() => Math.round(window.scrollY) === 650);
    await page.close();
  });

  test("a hash link scrolls to its target", async () => {
    const page = await browser.newPage({ viewport: { width: 800, height: 400 } });
    page.setDefaultTimeout(5000);
    await page.goto(base + "/");
    await interactive(page);
    await page.locator("a", { hasText: "Credits" }).first().click();
    await page.waitForURL(base + "/about#credits");
    await page.waitForFunction(() => {
      const top = document.getElementById("credits")?.getBoundingClientRect().top ?? -1;
      return window.scrollY > 0 && top >= 0 && top < window.innerHeight;
    });
    assert.equal(await marker(page), 1);
    await page.close();
  });

  test("focus moves to the route focus target and the title is announced", async () => {
    const page = await open("/");
    await interactive(page);
    await page.locator("a", { hasText: "About" }).first().click();
    await page.waitForURL(base + "/about");
    await page.waitForFunction(() => document.activeElement?.hasAttribute("data-silica-route-focus") === true);
    assert.equal(await page.evaluate(() => document.activeElement?.textContent), "About");
    await page.waitForFunction(
      () => document.querySelector("[data-silica-announcer]")?.textContent === "About",
    );
    assert.equal(
      await page.evaluate(() => document.querySelector("[data-silica-announcer]")?.getAttribute("aria-live")),
      "polite",
    );

    await page.locator("a", { hasText: "Item 1" }).first().click();
    await page.waitForURL(base + "/item/1");
    await page.getByText("$2.00").waitFor();
    await page.waitForFunction(() => document.activeElement?.id === "silica-root");
    await page.waitForFunction(
      () => document.querySelector("[data-silica-announcer]")?.textContent === "Item",
    );
    await page.close();
  });

  test("hover preloads the item, and the click then makes no second request", async () => {
    const page = await open("/");
    await interactive(page);
    const asked: string[] = [];
    page.on("request", (request) => {
      if (request.url().includes("/api/item")) asked.push(request.postData() ?? "");
    });
    const link = page.locator("a", { hasText: "Item 2" }).first();
    assert.equal(await link.getAttribute("data-silica-preload"), "intent");
    const preload = page.waitForRequest((request) => request.url().includes("/api/item"));
    await link.hover();
    await preload;
    assert.deepEqual(asked, ["2"]);
    await page.waitForTimeout(200);
    await link.click();
    await page.waitForURL(base + "/item/2");
    await page.getByText("$3.00").waitFor();
    await page.waitForTimeout(300);
    assert.deepEqual(asked, ["2"], "the press used the preloaded answer");
    await page.close();
  });
});
