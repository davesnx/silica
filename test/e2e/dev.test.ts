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

const makeProject = (): string => {
  const project = mkdtempSync(join(tmpdir(), "silica-dev-"));
  for (const file of ["dune-project", "dune", "silica.opam", "silica-ui.opam", "silica-jsonkit-melange.opam"])
    copyFileSync(join(root, file), join(project, file));
  for (const directory of ["lib", "browser", "server", "ppx"]) symlinkSync(join(root, directory), join(project, directory));
  cpSync(join(root, "example"), join(project, "example"), { recursive: true, verbatimSymlinks: true });
  return project;
};

let project: string;
let browser: Browser;
let dev: Dev;
let base: string;
let port: number;

const about = () => join(project, "example/app/shared/example_about.mlx");
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

const open = async (path: string): Promise<Page> => {
  const page = await browser.newPage();
  page.setDefaultTimeout(30000);
  await page.goto(base + path);
  await page.locator("#silica-root[data-silica-ready]").waitFor();
  return page;
};

describe("silica dev", () => {
  test("the page loads with the dev client", async () => {
    const page = await open("/about");
    assert.equal(await page.locator("h1").textContent(), "About");
    assert.equal(await page.locator('script[src="/_silica/dev/client.js"]').count(), 1);
    await page.close();
  });

  test("an edit of a view text shows after the page reloads", async (context) => {
    const page = await open("/about");
    const from = dev.events.length;
    const original = readFileSync(about(), "utf8");
    const started = Date.now();
    edit(about(), 'Node.text "About"', 'Node.text "About us"');
    try {
      await page.locator("h1", { hasText: "About us" }).waitFor({ timeout: 60000 });
      context.diagnostic(`edit to browser: ${Date.now() - started} ms`);
      const kinds = dev.events.slice(from, from + 4).map((event) => `${event.t}:${event.status ?? event["what"]}`);
      assert.deepEqual(kinds, ["build:started", "build:ok", "server:restarted", "reload:page"]);
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
    const page = await open("/");
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
