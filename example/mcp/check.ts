import { spawn } from "node:child_process";
import { createInterface } from "node:readline";
import { performance } from "node:perf_hooks";

type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

type Tool = { name: string; description: string; inputSchema: Json };

type ToolResult = {
  content: { type: "text"; text: string }[];
  isError: boolean;
};

type Response =
  | { jsonrpc: "2.0"; id: number; result: Json }
  | { jsonrpc: "2.0"; id: number; error: { code: number; message: string } };

type Notification = { jsonrpc: "2.0"; method: string };

const executable = process.argv[2] ?? "_build/default/example/mcp/example_mcp.exe";
const server = spawn(executable, [], { stdio: ["pipe", "pipe", "inherit"] });
const lines = createInterface({ input: server.stdout });

const waiting = new Map<number, (response: Response) => void>();
const notifications: string[] = [];
let failures = 0;
let nextId = 1;

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

lines.on("line", (line) => {
  console.log(`<- ${line}`);
  const message: unknown = JSON.parse(line);
  if (!isObject(message)) throw new Error(`not an object: ${line}`);
  if (typeof message.id === "number") {
    const resolve = waiting.get(message.id);
    waiting.delete(message.id);
    resolve?.(message as Response);
  } else if (typeof message.method === "string") {
    notifications.push((message as Notification).method);
  }
});

lines.on("close", () => {
  if (waiting.size > 0) {
    console.log(`FAIL: the server closed its output with ${waiting.size} request(s) unanswered`);
    process.exitCode = 1;
  }
});

function send(message: Json): void {
  const line = JSON.stringify(message);
  console.log(`-> ${line}`);
  server.stdin.write(line + "\n");
}

async function request(method: string, params?: Json): Promise<Json> {
  const id = nextId++;
  const answered = new Promise<Response>((resolve) => waiting.set(id, resolve));
  const started = performance.now();
  send(params === undefined ? { jsonrpc: "2.0", id, method } : { jsonrpc: "2.0", id, method, params });
  const response = await answered;
  console.log(`   (${(performance.now() - started).toFixed(1)} ms)`);
  if ("error" in response) throw new Error(`${method}: ${response.error.message}`);
  return response.result;
}

function expect(condition: boolean, what: string): void {
  console.log(`${condition ? "PASS" : "FAIL"}: ${what}`);
  if (!condition) failures++;
}

async function listTools(): Promise<string[]> {
  const result = await request("tools/list");
  if (!isObject(result) || !Array.isArray(result.tools)) throw new Error("tools/list: no tools");
  return (result.tools as Tool[]).map((tool) => tool.name);
}

async function call(name: string, args: Json = {}): Promise<ToolResult> {
  const result = await request("tools/call", { name, arguments: args });
  if (!isObject(result) || !Array.isArray(result.content)) throw new Error(`${name}: no content`);
  return result as unknown as ToolResult;
}

function text(result: ToolResult): string {
  return result.content.map((item) => item.text).join("\n");
}

const editTools = ["edit.draft", "edit.save", "edit.leave", "edit.stay"];

async function main(): Promise<void> {
  const initialized = await request("initialize", {
    protocolVersion: "2025-11-25",
    capabilities: {},
    clientInfo: { name: "silica-check", version: "0.0.0" },
  });
  expect(
    isObject(initialized) &&
      initialized.protocolVersion === "2025-11-25" &&
      isObject(initialized.capabilities) &&
      isObject(initialized.capabilities.tools) &&
      initialized.capabilities.tools.listChanged === true,
    "initialize gives 2025-11-25 and tools.listChanged",
  );
  send({ jsonrpc: "2.0", method: "notifications/initialized" });
  await request("ping");

  const onHome = await listTools();
  expect(
    onHome.includes("navigate") &&
      onHome.includes("read") &&
      onHome.includes("vote.cast") &&
      !editTools.some((tool) => onHome.includes(tool)),
    "the home page offers navigate, read and vote.cast, and no edit tool",
  );

  const early = await call("edit.draft", { text: "Too early" });
  expect(early.isError && text(early).includes("no instance of edit runs"), "an edit tool on the home page is refused");

  const refused = await call("navigate", { url: "/nowhere/at/all" });
  expect(refused.isError && text(refused).includes("not published"), "navigate to the Not_found route is refused");

  const locked = await call("navigate", { url: "/locked" });
  expect(locked.isError && text(locked).includes("/locked, which is not published"), "navigate to the Locked route is refused");

  const notificationsBeforeVote = notifications.length;
  const voted = await call("vote.cast");
  expect(
    !voted.isError && text(voted).includes("Before:\n  vote at router/:home/4:vote: idle\nAfter:\n  vote at router/:home/4:vote: ready"),
    "vote.cast gets the answer of the real vote handler: vote is ready",
  );

  const afterVote = await listTools();
  expect(notifications.length > notificationsBeforeVote, "the server says the tool list changed after the vote");
  expect(!afterVote.includes("vote.cast"), "vote.cast left the list");

  const again = await call("vote.cast");
  expect(again.isError && text(again).includes("in the state ready"), "vote.cast in the state ready is refused");

  const notificationsBeforeEdit = notifications.length;
  const opened = await call("navigate", { url: "/edit" });
  expect(!opened.isError && text(opened).includes("After:\n  edit at router/:edit: clean"), "navigate opens the edit page: edit is clean");

  const onEdit = await listTools();
  expect(notifications.length > notificationsBeforeEdit, "the server says the tool list changed");
  expect(editTools.every((tool) => onEdit.includes(tool)), "the edit page offers every edit tool");

  const badDraft = await call("edit.draft", { text: 5 });
  expect(badDraft.isError && text(badDraft).includes("do not decode"), "a draft that is not a string does not decode");

  const ignored = await call("edit.leave");
  expect(ignored.isError && text(ignored).includes("in the state clean ignores this edit.leave event"), "edit.leave in the state clean is refused");

  const drafted = await call("edit.draft", { text: "Made by an agent" });
  expect(
    !drafted.isError && text(drafted).includes("edit at router/:edit: dirty") && text(drafted).includes('value="Made by an agent"'),
    "the draft shows in the field: edit is dirty",
  );

  const held = await call("navigate", { url: "/about" });
  expect(
    !held.isError && text(held).includes("edit at router/:edit: asking") && text(held).includes("URL: /edit\n"),
    "navigate away from the draft is held: edit is asking and the URL stays /edit",
  );

  const left = await call("edit.leave");
  expect(!left.isError && text(left).includes("URL: /about\n"), "edit.leave drops the draft and opens /about");

  const afterLeave = await listTools();
  expect(!editTools.some((tool) => afterLeave.includes(tool)), "the edit tools left the list");

  const item = await call("navigate", { url: "/item/3" });
  expect(
    !item.isError && text(item).includes("item at router/:item: loaded") && text(item).includes('heading "Item 3"') && text(item).includes('"$4.00"'),
    "navigate to /item/3 gets the answer of the real item handler: Item 3 at $4.00",
  );

  const read = await call("read");
  expect(!read.isError && text(read).includes("URL: /item/3"), "read gives the URL");

  const gone = await call("edit.save");
  expect(gone.isError, "edit.save on the item page is refused");
}

main()
  .catch((error: unknown) => {
    console.log(`FAIL: ${error instanceof Error ? error.message : String(error)}`);
    failures++;
  })
  .finally(() => {
    server.stdin.end();
    console.log(failures === 0 ? "ALL PASSED" : `${failures} FAILED`);
    process.exitCode = failures === 0 ? 0 : 1;
  });
