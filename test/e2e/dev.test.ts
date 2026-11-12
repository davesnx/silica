import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, type ChildProcess } from "node:child_process";
import { createServer } from "node:net";
import {
  copyFileSync,
  cpSync,
  existsSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  readlinkSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { delimiter, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type Page } from "playwright-core";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const silica = resolve(root, "_build/default/bin/silica.exe");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";
const opamBin = resolve(root, "_opam/bin");

type Event = { t: string; status?: string; [field: string]: unknown };

class Dev {
  events: Event[] = [];
  stderr = "";
  child: ChildProcess;
  exited: Promise<number | null>;
  private listeners: (() => void)[] = [];

  constructor(cwd: string, port: number) {
    this.child = spawn(
      silica,
      ["dev", "--server", "example/server/main.exe", "--client", "example/client", "--port", String(port), "--json"],
      { cwd, env: environment() },
    );
    let pending = "";
    this.child.stdout?.on("data", (data: Buffer) => {
      pending += data.toString();
      const lines = pending.split("\n");
      pending = lines.pop() ?? "";
      for (const line of lines) {
        if (line.trim() === "") continue;
        this.events.push(JSON.parse(line) as Event);
      }
      for (const listener of this.listeners) listener();
    });
    this.child.stderr?.on("data", (data: Buffer) => (this.stderr += data.toString()));
    this.exited = new Promise((ok) => this.child.on("exit", (code) => ok(code)));
  }

  waitFor(predicate: (event: Event) => boolean, from: number, timeout = 120000): Promise<Event> {
    return new Promise((ok, fail) => {
      const check = () => {
        const found = this.events.slice(from).find(predicate);
        if (found) {
          clearTimeout(timer);
          this.listeners = this.listeners.filter((listener) => listener !== check);
          ok(found);
        }
      };
      const timer = setTimeout(() => {
        this.listeners = this.listeners.filter((listener) => listener !== check);
        fail(new Error(`no matching event in ${timeout} ms\nevents: ${JSON.stringify(this.events)}\nstderr: ${this.stderr}`));
      }, timeout);
      this.listeners.push(check);
      check();
    });
  }
}

const environment = () => ({
  ...process.env,
  PATH: existsSync(opamBin) ? `${opamBin}${delimiter}${process.env["PATH"] ?? ""}` : process.env["PATH"],
});

const freePort = (): Promise<number> =>
  new Promise((ok, fail) => {
    const server = createServer();
    server.once("error", fail);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      const port = typeof address === "object" && address ? address.port : 0;
      server.close(() => ok(port));
    });
  });

const portFree = (port: number): Promise<boolean> =>
  new Promise((ok) => {
    const server = createServer();
    server.once("error", () => ok(false));
    server.listen(port, "127.0.0.1", () => server.close(() => ok(true)));
  });

const processesIn = (directory: string): number[] =>
  readdirSync("/proc")
    .filter((entry) => /^\d+$/.test(entry))
    .filter((entry) => {
      try {
        return readlinkSync(`/proc/${entry}/cwd`) === directory;
      } catch {
        return false;
      }
    })
    .map(Number);

const hotComponent = `open Silica

type state = Idle of int * string | Waiting of int * string
[@@deriving state_shape]

type event = Bump | Wait | Done | Typed of string [@@deriving event_shape]

let machine =
  Machine.make ~id:"hot" ~init:(Idle (0, "")) ~shape:state_shape
    ~events:event_shape
    ~owns:(function Waiting _ -> [ Source.after 2500 Done ] | _ -> [])
    (fun state event ->
      match (state, event) with
      | Idle (count, text), Bump ->
          Go (Idle (count + 1, text))
      | Waiting (count, text), Bump ->
          Go (Waiting (count + 1, text))
      | Idle (count, text), Wait ->
          Go (Waiting (count, text))
      | Waiting (count, text), Done ->
          Go (Idle (count + 100, text))
      | Idle (count, _), Typed text ->
          Go (Idle (count, text))
      | Waiting (count, _), Typed text ->
          Go (Waiting (count, text))
      | _ ->
          Ignore
    )

let parts = function
  | Idle (count, text) ->
      (count, text, "idle")
  | Waiting (count, text) ->
      (count, text, "waiting")

let view state : event Node.t =
  let count, text, status = parts state in
  <section id="hot">
    <div style="height: 3000px" />
    <p id="hot-label">"Hot count"</p>
    <button id="hot-bump" on_press=Bump>(Node.text (string_of_int count))</button>
    <button id="hot-wait" on_press=Wait>"Wait"</button>
    <input id="hot-input" aria_label="Hot draft" value=text on_input=(fun v -> Typed v) />
    <span id="hot-status">(Node.text status)</span>
  </section>

let component = Component.make machine view
let make ?key () = Component.node ?key component
`;

const makeProject = (): string => {
  const project = mkdtempSync(join(tmpdir(), "silica-dev-"));
  for (const file of ["dune-project", "dune", "silica.opam", "silica-ui.opam", "silica-jsonkit-melange.opam"])
    copyFileSync(join(root, file), join(project, file));
  for (const directory of ["lib", "browser", "server", "ppx"]) symlinkSync(join(root, directory), join(project, directory));
  cpSync(join(root, "example"), join(project, "example"), { recursive: true, verbatimSymlinks: true });
  writeFileSync(join(project, "example/app/shared/example_hot.mlx"), hotComponent);
  const home = join(project, "example/app/shared/example_home.mlx");
  writeFileSync(
    home,
    readFileSync(home, "utf8").replace("<Example_vote_button />", "<Example_vote_button />\n    <Example_hot />"),
  );
  return project;
};

let project: string;
let browser: Browser;
let dev: Dev;
let base: string;
let port: number;

const about = () => join(project, "example/app/shared/example_about.mlx");
const hot = () => join(project, "example/app/shared/example_hot.mlx");
const home = () => join(project, "example/app/shared/example_home.mlx");
const serverMain = () => join(project, "example/server/main.ml");

const edit = (path: string, from: string, to: string) => {
  const text = readFileSync(path, "utf8");
  assert.ok(text.includes(from), `${path} holds ${from}`);
  writeFileSync(path, text.replace(from, to));
};

before(async () => {
  assert.ok(existsSync(silica), `missing ${silica}; run dune build`);
  project = makeProject();
  port = await freePort();
  base = `http://127.0.0.1:${port}`;
  dev = new Dev(project, port);
  await dev.waitFor((event) => event.t === "server" && event.status === "started", 0, 600000);
  browser = await chromium.launch({ executablePath: chromePath, headless: true });
});

after(async () => {
  await browser?.close();
  dev?.child.kill("SIGKILL");
  if (project) rmSync(project, { recursive: true, force: true });
});

const islandLive = (page: Page): Promise<void> =>
  new Promise((ok) => {
    page.on("websocket", (socket) => {
      let island = false;
      socket.on("framesent", (frame) => {
        if (String(frame.payload).includes('"island":"counter"')) island = true;
      });
      socket.on("framereceived", (frame) => {
        if (island && String(frame.payload).includes('"t":"state"')) ok();
      });
    });
  });

const open = async (path: string): Promise<Page> => {
  const page = await browser.newPage();
  page.setDefaultTimeout(30000);
  await page.goto(base + path);
  await page.locator("#silica-root[data-silica-ready]").waitFor();
  return page;
};

type Counts = { listeners: number; timers: number; sockets: number };

const countLeaks = () => {
  const own = () => /\/static\//.test(new Error().stack ?? "");
  const listeners = new Set<string>();
  const ids = new WeakMap<object, number>();
  let next = 0;
  const id = (value: object) => {
    if (!ids.has(value)) ids.set(value, ++next);
    return ids.get(value);
  };
  const key = (target: object, type: string, listener: unknown, options: unknown) =>
    `${id(target)} ${type} ${typeof listener === "object" || typeof listener === "function" ? id(listener as object) : "?"} ${
      typeof options === "boolean" ? options : !!(options as { capture?: boolean } | undefined)?.capture
    }`;
  const add = EventTarget.prototype.addEventListener;
  const remove = EventTarget.prototype.removeEventListener;
  EventTarget.prototype.addEventListener = function (this: object, type: string, listener: unknown, options?: unknown) {
    if (own() && !(this instanceof WebSocket)) listeners.add(key(this, type, listener, options));
    return add.call(this, type, listener as EventListener, options as AddEventListenerOptions);
  };
  EventTarget.prototype.removeEventListener = function (this: object, type: string, listener: unknown, options?: unknown) {
    listeners.delete(key(this, type, listener, options));
    return remove.call(this, type, listener as EventListener, options as EventListenerOptions);
  };
  const timers = new Set<number>();
  const set = window.setTimeout;
  const clear = window.clearTimeout;
  window.setTimeout = ((run: () => void, ms?: number) => {
    const timer: number = set(() => {
      timers.delete(timer);
      run();
    }, ms);
    if (own()) timers.add(timer);
    return timer;
  }) as typeof window.setTimeout;
  window.clearTimeout = ((timer: number) => {
    timers.delete(timer);
    clear(timer);
  }) as typeof window.clearTimeout;
  const sockets: WebSocket[] = [];
  const Socket = window.WebSocket;
  window.WebSocket = class extends Socket {
    constructor(url: string | URL, protocols?: string | string[]) {
      super(url, protocols);
      if (!String(url).includes("/_silica/dev")) sockets.push(this);
    }
  };
  (window as unknown as { leaks: () => Counts }).leaks = () => ({
    listeners: listeners.size,
    timers: timers.size,
    sockets: sockets.filter((socket) => socket.readyState === 0 || socket.readyState === 1).length,
  });
};

const leaks = (page: Page): Promise<Counts> =>
  page.evaluate(() => (window as unknown as { leaks: () => Counts }).leaks());

describe("hot reload", () => {
  test("a view edit keeps every machine, the timers, the island, focus and scroll; a state type edit restarts that machine only", async (context) => {
    const page = await browser.newPage();
    page.setDefaultTimeout(30000);
    await page.addInitScript(countLeaks);
    const live = islandLive(page);
    await page.goto(base + "/");
    await page.locator("#silica-root[data-silica-ready]").waitFor();
    const originals = { hot: readFileSync(hot(), "utf8"), home: readFileSync(home(), "utf8") };
    try {
      await live;
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "0");
      for (let press = 0; press < 3; press++) await page.getByRole("button", { name: "Increment" }).click();
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "3");
      for (let press = 0; press < 5; press++) await page.locator("#hot-bump").click();
      await page.locator("#hot-input").fill("hello");
      await page.locator("#hot-wait").click();
      assert.equal(await page.locator("#hot-status").textContent(), "waiting");
      await page.locator("#hot-input").focus();
      await page.evaluate(() => window.scrollTo(0, 1500));
      const scrolled = await page.evaluate(() => window.scrollY);
      assert.ok(scrolled > 1000, `the page scrolled: ${scrolled}`);
      await page.evaluate(() => ((window as unknown as { marker: number }).marker = 1));
      const mounted = await leaks(page);
      assert.ok(mounted.listeners > 0 && mounted.sockets === 2 && mounted.timers >= 1, JSON.stringify(mounted));

      const swap = async (path: string, from: string, to: string) => {
        const before = dev.events.length;
        const started = Date.now();
        edit(path, from, to);
        const event = await dev.waitFor((event) => event.t === "hot-reload", before);
        return { event, ms: Date.now() - started, events: dev.events.slice(before) };
      };

      const first = await swap(home(), '<h1>"Silica example"</h1>', '<h1>"Silica example, edited"</h1>');
      await page.locator("h1", { hasText: "Silica example, edited" }).waitFor();
      context.diagnostic(`edit to new view with state kept: ${first.ms} ms (browser swap ${first.event["ms"]} ms)`);
      assert.equal(first.event["status"], "kept", JSON.stringify(first.event));
      assert.deepEqual(first.event["restarted"], [], JSON.stringify(first.event));
      const kept = first.event["kept"] as string[];
      for (const machine of ["router", ":home", ":hot", ":todo", ":votes", ":vote"])
        assert.ok(kept.some((path) => path.endsWith(machine) || path === machine), `${machine} kept: ${kept.join(", ")}`);
      assert.deepEqual(
        first.events.map((event) => `${event.t}:${event.status ?? event["what"]}`).filter((kind) => kind !== "progress:undefined"),
        ["build:started", "build:ok", "server:restarted", "reload:code", "hot-reload:kept"],
      );
      assert.equal(await page.evaluate(() => (window as unknown as { marker?: number }).marker), 1, "no page load");
      assert.equal(await page.locator("#hot-bump").textContent(), "5");
      assert.equal(await page.locator("#hot-input").inputValue(), "hello");
      assert.equal(await page.locator("#hot-status").textContent(), "waiting");
      assert.equal(await page.evaluate(() => document.activeElement?.id), "hot-input", "focus stays");
      assert.equal(await page.evaluate(() => window.scrollY), scrolled, "scroll stays");
      assert.equal(await page.locator("[data-silica-live=counter] span").textContent(), "3", "the island keeps its server state");
      await page.waitForFunction(() => document.querySelector("#hot-status")?.textContent === "idle", undefined, { timeout: 10000 });
      assert.equal(await page.locator("#hot-bump").textContent(), "105", "the timer ran to its end once");
      await page.getByRole("button", { name: "Increment" }).click();
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "4");
      const afterFirst = await leaks(page);
      assert.deepEqual(afterFirst, { ...mounted, timers: afterFirst.timers }, "the same listeners and sockets");

      const second = await swap(home(), '<h1>"Silica example, edited"</h1>', '<h1>"Silica example"</h1>');
      assert.deepEqual(second.event["restarted"], [], JSON.stringify(second.event));
      await page.locator("h1", { hasText: /^Silica example$/ }).waitFor();
      const third = await swap(hot(), '"Hot count"', '"Hot count, edited"');
      assert.deepEqual(third.event["restarted"], [], JSON.stringify(third.event));
      await page.locator("#hot-label", { hasText: "Hot count, edited" }).waitFor();
      await page.waitForTimeout(500);
      const afterThird = await leaks(page);
      assert.deepEqual(
        { listeners: afterThird.listeners, sockets: afterThird.sockets },
        { listeners: afterFirst.listeners, sockets: afterFirst.sockets },
        "a second and a third swap leave no listener and no socket behind",
      );
      assert.equal(afterThird.timers, afterThird.sockets, "only the heartbeat timer of each Live socket runs");
      await page.locator("#hot-bump").click();
      assert.equal(await page.locator("#hot-bump").textContent(), "106", "a press runs once");
      await page.getByRole("button", { name: "Increment" }).click();
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "5");

      const before = dev.events.length;
      writeFileSync(
        hot(),
        readFileSync(hot(), "utf8")
          .replace("| Waiting of int * string\n", "| Waiting of int * string\n  | Paused\n")
          .replace('(count, text, "waiting")', '(count, text, "waiting")\n  | Paused ->\n      (0, "", "paused")'),
      );
      const changed = await dev.waitFor((event) => event.t === "hot-reload", before);
      const restarted = changed["restarted"] as { path: string; machine: string; reason: string }[];
      assert.deepEqual(
        restarted.map((entry) => [entry.machine, entry.reason]),
        [["hot", "its state type changed"]],
      );
      assert.equal(await page.locator("#hot-bump").textContent(), "0", "the changed machine started again");
      assert.equal(await page.locator("[data-silica-live=counter] span").textContent(), "5", "the others keep their state");
      const overlay = page.locator("[data-silica-dev-restarted]");
      await overlay.waitFor();
      assert.match((await overlay.textContent()) ?? "", /hot \(router\/[^)]*:hot\) started again: its state type changed/);
    } finally {
      const from = dev.events.length;
      const changed = readFileSync(hot(), "utf8") !== originals.hot || readFileSync(home(), "utf8") !== originals.home;
      writeFileSync(hot(), originals.hot);
      writeFileSync(home(), originals.home);
      if (changed) await dev.waitFor((event) => event.t === "hot-reload", from);
      await page.close();
    }
  });
  test("fifty hot reloads in a row keep the state and the heap", async (context) => {
    const page = await open("/");
    for (let press = 0; press < 3; press++) await page.locator("#hot-bump").click();
    const client = await page.context().newCDPSession(page);
    await client.send("HeapProfiler.enable");
    const heap = async () => {
      await client.send("HeapProfiler.collectGarbage");
      const { usedSize } = (await client.send("Runtime.getHeapUsage")) as { usedSize: number };
      return usedSize;
    };
    const before = await heap();
    const swaps = await page.evaluate(async () => {
      const entry = (document.querySelector('script[src*="client_main"]') as HTMLScriptElement).src;
      const times: number[] = [];
      for (let swap = 1; swap <= 50; swap++) {
        const started = performance.now();
        const resumed = new Promise((ok) => addEventListener("silica-resumed", ok, { once: true }));
        const dev = (globalThis as unknown as { __silica_dev: { suspend: () => unknown } }).__silica_dev;
        (globalThis as unknown as { __silica_handoff: unknown }).__silica_handoff = dev.suspend();
        await import(`${entry}#swap=${swap}`);
        await resumed;
        times.push(performance.now() - started);
      }
      return times;
    });
    const after = await heap();
    const median = [...swaps].sort((a, b) => a - b)[25] ?? 0;
    context.diagnostic(
      `heap ${Math.round(before / 1e6)} MB before, ${Math.round(after / 1e6)} MB after 50 swaps; median swap ${Math.round(median)} ms`,
    );
    assert.equal(await page.locator("#hot-bump").textContent(), "3", "the state survives fifty swaps");
    await page.locator("#hot-bump").click();
    assert.equal(await page.locator("#hot-bump").textContent(), "4", "a press runs once after fifty swaps");
    assert.ok(after - before < 200e6, `the heap grew by ${Math.round((after - before) / 1e6)} MB`);
    await page.close();
  });
});

describe("silica dev", () => {
  test("the page loads with the dev client", async () => {
    const page = await open("/about");
    assert.equal(await page.locator("h1").textContent(), "About");
    assert.equal(await page.locator('script[src="/_silica/dev/client.js"]').count(), 1);
    await page.close();
  });

  test("an edit of a view text shows with no page load", async (context) => {
    const page = await open("/about");
    const from = dev.events.length;
    const original = readFileSync(about(), "utf8");
    const started = Date.now();
    edit(about(), 'Node.text "About"', 'Node.text "About us"');
    try {
      await page.locator("h1", { hasText: "About us" }).waitFor({ timeout: 60000 });
      context.diagnostic(`edit to browser: ${Date.now() - started} ms`);
      const kinds = dev.events.slice(from, from + 4).map((event) => `${event.t}:${event.status ?? event["what"]}`);
      assert.deepEqual(kinds, ["build:started", "build:ok", "server:restarted", "reload:code"]);
    } finally {
      writeFileSync(about(), original);
      await dev.waitFor((event) => event.t === "reload", from + 4);
      await page.close();
    }
  });

  test("a compile error shows its file, line and source lines, and goes when fixed", async () => {
    const page = await open("/about");
    const from = dev.events.length;
    const original = readFileSync(about(), "utf8");
    edit(about(), 'Node.text "About"', "Node.text About_broken");
    try {
      const failed = await dev.waitFor((event) => event.t === "build" && event.status === "failed", from);
      const errors = failed["errors"] as { file: string; line: number; message: string; excerpt: [number, string][] }[];
      assert.equal(errors.length, 1, "one error, not one for each copy of the file");
      assert.equal(errors[0]?.file, "example/app/shared/example_about.mlx");
      assert.equal(errors[0]?.line, 20);
      assert.match(errors[0]?.message ?? "", /Unbound constructor About_broken/);
      assert.deepEqual(
        errors[0]?.excerpt.map(([line]) => line),
        [18, 19, 20, 21, 22],
      );
      const overlay = page.locator("[data-silica-dev-overlay]");
      await overlay.waitFor();
      assert.match((await overlay.textContent()) ?? "", /example\/app\/shared\/example_about\.mlx:20:/);
      assert.match((await page.locator('#silica-dev .line[data-error]').textContent()) ?? "", /20 \|\s+\[ Node.text About_broken \]/);
      assert.equal(await page.locator("h1", { hasText: "About" }).first().isVisible(), true, "the old page still runs under the overlay");
    } finally {
      writeFileSync(about(), original);
    }
    await dev.waitFor((event) => event.t === "build" && event.status === "ok", from);
    await page.locator("[data-silica-dev-overlay]").waitFor({ state: "detached" });
    await page.close();
  });

  test("the Live counter and the session keep their values across a server restart", async () => {
    const page = await browser.newPage();
    page.setDefaultTimeout(30000);
    const live = islandLive(page);
    await page.goto(base + "/");
    await page.locator("#silica-root[data-silica-ready]").waitFor();
    await live;
    const counter = page.locator("[data-silica-live=counter] span");
    const increment = page.getByRole("button", { name: "Increment" });
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "0");
    for (let press = 0; press < 3; press++) await increment.click();
    await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "3");
    const cookies = await page.context().cookies();
    await page.evaluate(() => {
      const state = window as unknown as { marker: number; wentOffline: boolean };
      state.marker = 1;
      state.wentOffline = false;
      const island = document.querySelector("[data-silica-live]");
      if (island)
        new MutationObserver(() => {
          if (island.hasAttribute("data-silica-offline")) state.wentOffline = true;
        }).observe(island, { attributes: true });
    });
    const from = dev.events.length;
    const original = readFileSync(serverMain(), "utf8");
    edit(serverMain(), '"silica example server"', '"silica example server, edited"');
    try {
      await dev.waitFor((event) => event.t === "server" && event.status === "restarted", from);
      await page.waitForFunction(() => {
        const island = document.querySelector("[data-silica-live]");
        return (window as unknown as { wentOffline: boolean }).wentOffline && !island?.hasAttribute("data-silica-offline");
      });
      await increment.click();
      await page.waitForFunction(() => document.querySelector("[data-silica-live=counter] span")?.textContent === "4");
      assert.equal(await counter.textContent(), "4");
      assert.equal(
        await page.evaluate(() => (window as unknown as { marker?: number }).marker),
        1,
        "a server-only change does not reload the page",
      );
      assert.deepEqual(await page.context().cookies(), cookies, "the session cookie is the same");
      assert.equal(
        dev.events.slice(from).some((event) => event.t === "reload"),
        false,
      );
    } finally {
      writeFileSync(serverMain(), original);
      await dev.waitFor((event) => event.t === "server" && event.status === "restarted", from + 3);
      await page.close();
    }
  });

  test("a server that stops at start is reported, and starts after the next good build", async () => {
    const from = dev.events.length;
    const original = readFileSync(serverMain(), "utf8");
    edit(serverMain(), "let () =\n", 'let () =\n  if true then failwith "server boom";\n');
    try {
      const crashed = await dev.waitFor((event) => event.t === "server" && event.status === "crashed", from);
      assert.match(String(crashed["output"]), /server boom/);
      assert.match(String(crashed["code"]), /^exit /);
      const page = await browser.newPage();
      await page.goto(base + "/");
      await page.locator("[data-silica-dev-overlay]", { hasText: "server boom" }).waitFor();
      await page.close();
    } finally {
      writeFileSync(serverMain(), original);
    }
    await dev.waitFor((event) => event.t === "server" && event.status === "restarted", from);
    const page = await open("/about");
    await page.close();
  });

  test("a second silica dev in the same project stops with a clear error", async () => {
    const second = new Dev(project, await freePort());
    assert.equal(await second.exited, 1);
    assert.match(second.stderr, /another silica dev runs in this project \(pid \d+\)/);
  });
});

const stopTest = (signal: "SIGINT" | "SIGTERM", restart: boolean) => async () => {
  let running = dev;
  if (restart) {
    running = new Dev(project, port);
    await running.waitFor((event) => event.t === "server" && event.status === "started", 0, 300000);
  }
  const children = processesIn(project).filter((pid) => pid !== running.child.pid);
  assert.ok(children.length >= 2, `dune and the server run: ${children.join(", ")}`);
  running.child.kill(signal);
  assert.equal(await running.exited, 0);
  assert.deepEqual(processesIn(project), [], "no process is left in the project");
  assert.equal(await portFree(port), true, "the port is free");
  assert.ok(existsSync(join(project, "_build/silica-dev/state.json")), "the server saved its state");
};

describe("stopping silica dev", () => {
  test("SIGINT stops the server and dune, and frees the port", stopTest("SIGINT", false));
  test("SIGTERM stops the server and dune, and frees the port", stopTest("SIGTERM", true));
});
