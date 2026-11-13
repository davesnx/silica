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

const executable = process.argv[2] ?? "_build/default/linear/mcp/linear_mcp.exe";
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

const createTools = [
  "new_issue.title",
  "new_issue.description",
  "new_issue.status_picked",
  "new_issue.priority_picked",
  "new_issue.submit",
  "new_issue.cancel",
];

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

  const onList = await listTools();
  expect(
    onList.includes("navigate") && onList.includes("read") && !createTools.some((tool) => onList.includes(tool)),
    "the list page offers navigate and read, and no create tool",
  );

  const early = await call("new_issue.title", { title: "Too early" });
  expect(early.isError && text(early).includes("closed"), "a create tool while the dialog is closed is refused");

  const refused = await call("navigate", { url: "/nowhere/at/all" });
  expect(refused.isError && text(refused).includes("not published"), "navigate to the Not_found route is refused");

  const opened = await call("navigate", { url: "/?new=true" });
  expect(!opened.isError && text(opened).includes("create-issue at router/0.2:create-issue: editing"), "navigate opens the dialog: create-issue is editing");
  expect(notifications.includes("notifications/tools/list_changed"), "the server says the tool list changed");

  const inDialog = await listTools();
  expect(createTools.every((tool) => inDialog.includes(tool)), "the dialog offers every create tool");

  const badStatus = await call("new_issue.status_picked", { status: "Someday" });
  expect(badStatus.isError && text(badStatus).includes("do not decode"), "a status that is not a label does not decode");

  const empty = await call("new_issue.submit");
  expect(!empty.isError && text(empty).includes("Title is required"), "a submit with no title stays in editing with the error");

  const titled = await call("new_issue.title", { title: "Made by an agent" });
  expect(!titled.isError && text(titled).includes('value="Made by an agent"'), "the title shows in the field");

  await call("new_issue.priority_picked", { priority: "High" });

  const notificationsBefore = notifications.length;
  const submitted = await call("new_issue.submit");
  expect(!submitted.isError && text(submitted).includes("create-issue at router/0.2:create-issue: closed"), "submit creates the issue and closes the dialog");
  expect(notifications.length > notificationsBefore, "the server says the tool list changed after the submit");

  const afterSubmit = await listTools();
  expect(!createTools.some((tool) => afterSubmit.includes(tool)), "the create tools left the list");

  const issue = await call("navigate", { url: "/issue/31" });
  expect(!issue.isError && text(issue).includes("Made by an agent"), "the new issue ENG-31 opens from the store");

  const read = await call("read");
  expect(!read.isError && text(read).includes("URL: /issue/31"), "read gives the URL");

  const gone = await call("new_issue.submit");
  expect(gone.isError, "submit on the issue page is refused");
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
