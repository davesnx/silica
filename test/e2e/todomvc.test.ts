import { after, before, describe, test } from "node:test";
import assert from "node:assert/strict";
import { spawn, type ChildProcess } from "node:child_process";
import { createServer } from "node:net";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type Page } from "playwright-core";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const exe = resolve(root, "_build/default/todomvc/server/main.exe");
const dist = resolve(root, "_build/default/todomvc/client/dist");
const metafile = resolve(root, "_build/default/todomvc/client/meta.json");
const styles = resolve(root, "todomvc/styles");
const chromePath = process.env["CHROME"] ?? "/usr/bin/chromium";

let browser: Browser;

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

type Running = { base: string; child: ChildProcess; log: () => string };

const startServer = async (): Promise<Running> => {
  const port = await freePort();
  const base = `http://127.0.0.1:${port}`;
  let log = "";
  const child = spawn(
    exe,
    ["--port", String(port), "--static", dist, "--metafile", metafile, "--styles", styles],
    { cwd: root },
  );
  child.stdout?.on("data", (d: Buffer) => (log += d.toString()));
  child.stderr?.on("data", (d: Buffer) => (log += d.toString()));
  for (let i = 0; i < 100; i++) {
    try {
      await fetch(base + "/_silica/health");
      return { base, child, log: () => log };
    } catch {
      await new Promise((r) => setTimeout(r, 100));
    }
  }
  throw new Error(`server did not start\n${log}`);
};

const withServer = async (body: (running: Running) => Promise<void>): Promise<void> => {
  const running = await startServer();
  try {
    await body(running);
  } finally {
    running.child.kill("SIGTERM");
  }
};

let nextId = 1;

const tool = async (base: string, name: string, args: { [key: string]: unknown } = {}): Promise<{ isError: boolean; text: string }> => {
  const response = await fetch(base + "/mcp", {
    method: "POST",
    headers: {
      "content-type": "application/json",
      accept: "application/json, text/event-stream",
      "MCP-Protocol-Version": "2026-07-28",
      "Mcp-Method": "tools/call",
      "Mcp-Name": name,
    },
    body: JSON.stringify({
      jsonrpc: "2.0",
      id: nextId++,
      method: "tools/call",
      params: {
        name,
        arguments: args,
        _meta: {
          "io.modelcontextprotocol/protocolVersion": "2026-07-28",
          "io.modelcontextprotocol/clientCapabilities": {},
        },
      },
    }),
  });
  assert.equal(response.status, 200);
  const answer = (await response.json()) as { result: { isError: boolean; content: { text: string }[] } };
  return { isError: answer.result.isError, text: answer.result.content.map((item) => item.text).join("\n") };
};

const open = async (base: string, path: string): Promise<{ page: Page; errors: string[] }> => {
  const context = await browser.newContext();
  const page = await context.newPage();
  page.setDefaultTimeout(5000);
  const errors: string[] = [];
  page.on("console", (m) => {
    if (m.type() === "error" && !m.text().includes("favicon") && !m.text().includes("404")) errors.push(m.text());
  });
  page.on("pageerror", (e) => errors.push(String(e)));
  await page.goto(base + path);
  await page.locator("#silica-root[data-silica-ready]").waitFor({ state: "attached" });
  await page.locator('[data-silica-live] .new-todo').waitFor();
  return { page, errors };
};

const labels = (page: Page) => page.locator(".todo-list li label").allTextContents();

const settled = async (page: Page, expected: string[]): Promise<void> => {
  await page.waitForFunction(
    (want) =>
      JSON.stringify(Array.from(document.querySelectorAll(".todo-list li label")).map((l) => l.textContent)) ===
      JSON.stringify(want),
    expected,
  );
};

before(async () => {
  assert.ok(existsSync(exe), `missing ${exe}; run dune build`);
  assert.ok(existsSync(metafile), `missing ${metafile}; run dune build`);
  browser = await chromium.launch({ executablePath: chromePath, headless: true });
});

after(async () => {
  await browser?.close();
});

describe("todomvc: one list on the server", () => {
  test("an MCP tool call shows in an open tab at once", async () => {
    await withServer(async ({ base }) => {
      const { page, errors } = await open(base, "/");
      assert.deepEqual(await labels(page), []);
      const added = await tool(base, "todo.add", { text: "  Buy milk " });
      assert.equal(added.isError, false, added.text);
      await settled(page, ["Buy milk"]);
      await tool(base, "todo.toggle", { id: 1 });
      await page.locator('.todo-list li.completed input[aria-label="Todo 1: Buy milk"]').waitFor();
      assert.equal(await page.locator(".todo-count").textContent(), "0 items left");
      assert.deepEqual(errors, []);
    });
  });

  test("two tabs share the list; each tab keeps its own filter", async () => {
    await withServer(async ({ base }) => {
      const first = await open(base, "/");
      const second = await open(base, "/active");
      await first.page.locator(".new-todo").fill("Walk the dog");
      await first.page.locator(".new-todo").press("Enter");
      await settled(first.page, ["Walk the dog"]);
      await settled(second.page, ["Walk the dog"]);
      await second.page.locator(".new-todo").fill("Write code");
      await second.page.locator(".new-todo").press("Enter");
      await settled(first.page, ["Walk the dog", "Write code"]);
      await second.page.locator('input[aria-label="Todo 1: Walk the dog"]').click();
      await settled(second.page, ["Write code"]);
      await first.page.locator('.todo-list li.completed input[aria-label="Todo 1: Walk the dog"]').waitFor();
      await settled(first.page, ["Walk the dog", "Write code"]);
      assert.equal(second.page.url(), base + "/active");
      const read = await tool(base, "read");
      assert.match(read.text, /Todo 1: Walk the dog\\" checked \[press\]/);
      assert.deepEqual([...first.errors, ...second.errors], []);
    });
  });

  test("a new tab gets the list in its first HTML", async () => {
    await withServer(async ({ base }) => {
      await tool(base, "todo.add", { text: "From the agent" });
      const html = await (await fetch(base + "/")).text();
      assert.match(html, /aria-label="Todo 1: From the agent"/);
    });
  });

  test("Enter right after typing adds the whole text", async () => {
    await withServer(async ({ base }) => {
      const { page, errors } = await open(base, "/");
      const field = page.locator(".new-todo");
      await field.click();
      await page.keyboard.type("Buy bread and butter", { delay: 0 });
      await page.keyboard.press("Enter");
      await settled(page, ["Buy bread and butter"]);
      assert.equal(await field.inputValue(), "");
      assert.deepEqual(errors, []);
    });
  });

  test("editing in one tab saves on Enter; the other tab sees the new text", async () => {
    await withServer(async ({ base }) => {
      const first = await open(base, "/");
      const second = await open(base, "/");
      await tool(base, "todo.add", { text: "Old text" });
      await settled(first.page, ["Old text"]);
      await first.page.locator(".todo-list li label").dblclick();
      const edit = first.page.locator('input[aria-label="Edit todo 1"]');
      await edit.waitFor();
      assert.equal(await first.page.evaluate(() => document.activeElement?.getAttribute("aria-label")), "Edit todo 1");
      await edit.fill("New text");
      await edit.press("Enter");
      await settled(first.page, ["New text"]);
      await settled(second.page, ["New text"]);
      assert.deepEqual([...first.errors, ...second.errors], []);
    });
  });
});
