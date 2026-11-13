import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, spawnSync, type ChildProcess } from "node:child_process";
import { createServer } from "node:net";
import { existsSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { delimiter, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type Page } from "playwright-core";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const silica = resolve(root, "_build/default/bin/silica.exe");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";
const opamBin = resolve(root, "_opam/bin");

type Event = { t: string; status?: string; [field: string]: unknown };

const environment = (extra: Record<string, string> = {}) => ({
  ...process.env,
  PATH: existsSync(opamBin) ? `${opamBin}${delimiter}${process.env["PATH"] ?? ""}` : process.env["PATH"],
  ...extra,
});

class Dev {
  events: Event[] = [];
  stderr = "";
  child: ChildProcess;
  private listeners: (() => void)[] = [];

  constructor(cwd: string, arguments_: string[], extra: Record<string, string> = {}) {
    this.child = spawn(silica, ["dev", ...arguments_, "--json"], { cwd, env: environment(extra) });
    let pending = "";
    this.child.stdout?.on("data", (data: Buffer) => {
      pending += data.toString();
      const lines = pending.split("\n");
      pending = lines.pop() ?? "";
      for (const line of lines) if (line.trim() !== "") this.events.push(JSON.parse(line) as Event);
      for (const listener of this.listeners) listener();
    });
    this.child.stderr?.on("data", (data: Buffer) => (this.stderr = (this.stderr + data.toString()).slice(-4000)));
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

const edit = (path: string, from: string, to: string) => {
  const text = readFileSync(path, "utf8");
  assert.ok(text.includes(from), `${path} holds ${from}`);
  writeFileSync(path, text.replace(from, to));
};

const open = async (browser: Browser, url: string): Promise<Page> => {
  const page = await browser.newPage();
  page.setDefaultTimeout(30000);
  await page.goto(url);
  await page.locator("#silica-root[data-silica-ready]").waitFor();
  return page;
};

let browser: Browser;
const projects: string[] = [];
const devs: Dev[] = [];

before(async () => {
  assert.ok(existsSync(silica), `missing ${silica}; run dune build`);
  browser = await chromium.launch({ executablePath: chromePath, headless: true });
});

after(async () => {
  await browser?.close();
  await Promise.all(
    devs.map(
      (dev) =>
        new Promise<void>((ok) => {
          if (dev.child.exitCode !== null) return ok();
          dev.child.once("exit", () => ok());
          dev.child.kill("SIGTERM");
        }),
    ),
  );
  for (const project of projects) rmSync(project, { recursive: true, force: true });
});

describe("hot reload on a silica new project", () => {
  test("the counter keeps its value after a view edit", async () => {
    const parent = mkdtempSync(join(tmpdir(), "silica-dev-new-"));
    projects.push(parent);
    const made = spawnSync(silica, ["new", "counter-app"], { cwd: parent, encoding: "utf8" });
    assert.equal(made.status, 0, made.stderr);
    const project = join(parent, "counter-app");
    symlinkSync(join(root, "example/client/node_modules"), join(project, "client/node_modules"));
    const port = await freePort();
    const dev = new Dev(project, ["--port", String(port)], {
      OCAMLPATH: [resolve(root, "_build/install/default/lib"), process.env["OCAMLPATH"] ?? ""].filter((part) => part !== "").join(delimiter),
    });
    devs.push(dev);
    await dev.waitFor((event) => event.t === "server" && event.status === "started", 0, 900000);
    const page = await open(browser, `http://127.0.0.1:${port}/`);
    for (let press = 0; press < 3; press++) await page.getByRole("button", { name: "Increment" }).click();
    await page.locator("output", { hasText: "3" }).waitFor();
    const from = dev.events.length;
    edit(join(project, "app/shared/counter.mlx"), '<h1>"Counter"</h1>', '<h1>"Counter, edited"</h1>');
    const swap = await dev.waitFor((event) => event.t === "hot-reload", from, 300000);
    assert.equal(swap["status"], "kept", JSON.stringify(swap));
    assert.deepEqual(swap["kept"], ["counter"]);
    assert.deepEqual(swap["restarted"], []);
    await page.locator("h1", { hasText: "Counter, edited" }).waitFor();
    assert.equal(await page.locator("output").textContent(), "3", "the counter keeps its value");
    await page.close();
  });
});
