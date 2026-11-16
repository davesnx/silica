import { spawn } from "node:child_process";
import { createInterface } from "node:readline";
import { performance } from "node:perf_hooks";
import { isDeepStrictEqual } from "node:util";

type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

type Tool = {
  name: string;
  description: string;
  inputSchema: { [key: string]: Json };
  annotations?: { [key: string]: Json };
};

type ToolResult = {
  resultType: string;
  content: { type: "text"; text: string }[];
  structuredContent?: { [key: string]: Json };
  isError: boolean;
};

type RpcError = { code: number; message: string; data?: Json };

type Response =
  | { jsonrpc: "2.0"; id: number; result: { [key: string]: Json } }
  | { jsonrpc: "2.0"; id: number; error: RpcError };

const version = "2026-07-28";
const meta = {
  "io.modelcontextprotocol/protocolVersion": version,
  "io.modelcontextprotocol/clientInfo": { name: "silica-check", version: "0.0.0" },
  "io.modelcontextprotocol/clientCapabilities": {},
};

const executable = process.argv[2] ?? "_build/default/example/mcp/example_mcp.exe";
const server = spawn(executable, [], { stdio: ["pipe", "pipe", "inherit"] });
const lines = createInterface({ input: server.stdout });

const waiting = new Map<number, (response: Response) => void>();
const unexpected: string[] = [];
let failures = 0;
let nextId = 1;

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

lines.on("line", (line) => {
  console.log(`<- ${line}`);
  const message: unknown = JSON.parse(line);
  if (!isObject(message)) throw new Error(`not an object: ${line}`);
  const resolve = typeof message.id === "number" ? waiting.get(message.id) : undefined;
  if (resolve && typeof message.id === "number") {
    waiting.delete(message.id);
    resolve(message as Response);
  } else {
    unexpected.push(line);
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

async function raw(method: string, params?: { [key: string]: Json }): Promise<Response> {
  const id = nextId++;
  const answered = new Promise<Response>((resolve) => waiting.set(id, resolve));
  const started = performance.now();
  send(params === undefined ? { jsonrpc: "2.0", id, method } : { jsonrpc: "2.0", id, method, params });
  const response = await answered;
  console.log(`   (${(performance.now() - started).toFixed(1)} ms)`);
  return response;
}

async function request(method: string, params: { [key: string]: Json } = {}): Promise<{ [key: string]: Json }> {
  const response = await raw(method, { ...params, _meta: meta });
  if ("error" in response) throw new Error(`${method}: ${response.error.message}`);
  return response.result;
}

async function rpcError(method: string, params?: { [key: string]: Json }): Promise<RpcError | undefined> {
  const response = await raw(method, params);
  return "error" in response ? response.error : undefined;
}

function expect(condition: boolean, what: string): void {
  console.log(`${condition ? "PASS" : "FAIL"}: ${what}`);
  if (!condition) failures++;
}

function serverInfo(result: { [key: string]: Json }): boolean {
  return (
    isObject(result._meta) &&
    isDeepStrictEqual(result._meta["io.modelcontextprotocol/serverInfo"], { name: "silica-example", version: "0.0.0" })
  );
}

async function listTools(): Promise<{ [key: string]: Json }> {
  const result = await request("tools/list");
  if (!Array.isArray(result.tools)) throw new Error("tools/list: no tools");
  return result;
}

async function call(name: string, args: { [key: string]: Json } = {}): Promise<ToolResult> {
  const result = await request("tools/call", { name, arguments: args });
  if (!Array.isArray(result.content)) throw new Error(`${name}: no content`);
  return result as unknown as ToolResult;
}

function text(result: ToolResult): string {
  return result.content.map((item) => item.text).join("\n");
}

function structured(result: ToolResult): { [key: string]: Json } {
  return result.structuredContent ?? {};
}

function hasInstance(result: ToolResult, machine: string, instance: string, state: string): boolean {
  const instances = structured(result).instances;
  return Array.isArray(instances) && instances.some((item) => isDeepStrictEqual(item, { machine, instance, state }));
}

const machineTools = ["vote.cast", "edit.draft", "edit.save", "edit.leave", "edit.stay"];

async function main(): Promise<void> {
  const discovered = await request("server/discover");
  expect(
    discovered.resultType === "complete" &&
      isDeepStrictEqual(discovered.supportedVersions, [version]) &&
      isDeepStrictEqual(discovered.capabilities, { tools: {} }) &&
      typeof discovered.instructions === "string" &&
      typeof discovered.ttlMs === "number" &&
      discovered.cacheScope === "public" &&
      serverInfo(discovered),
    "server/discover gives 2026-07-28, the tools capability with no listChanged, instructions, caching and serverInfo",
  );

  const old = await rpcError("tools/list", { _meta: { ...meta, "io.modelcontextprotocol/protocolVersion": "2025-11-25" } });
  expect(
    old?.code === -32022 && isDeepStrictEqual(old.data, { supported: [version], requested: "2025-11-25" }),
    "a request for 2025-11-25 gets UnsupportedProtocolVersionError with the supported versions",
  );

  const bare = await rpcError("tools/list");
  expect(bare?.code === -32602, "a request with no _meta is invalid params");

  const initialize = await rpcError("initialize", {
    protocolVersion: "2025-11-25",
    capabilities: {},
    clientInfo: { name: "silica-check", version: "0.0.0" },
  });
  expect(
    initialize?.code === -32601 && initialize.message.includes(version),
    "initialize is not a method, and the error names 2026-07-28",
  );

  const listed = await listTools();
  const tools = listed.tools as unknown as Tool[];
  expect(
    isDeepStrictEqual(
      tools.map((tool) => tool.name),
      ["navigate", "read", ...machineTools],
    ),
    "tools/list gives every tool, in a fixed order, on the home page",
  );
  expect(
    listed.resultType === "complete" &&
      typeof listed.ttlMs === "number" &&
      listed.cacheScope === "public" &&
      serverInfo(listed),
    "tools/list is complete, with ttlMs, cacheScope and serverInfo",
  );
  const byName = new Map(tools.map((tool) => [tool.name, tool]));
  expect(
    isDeepStrictEqual(byName.get("read")?.inputSchema, { type: "object", additionalProperties: false }) &&
      byName.get("read")?.annotations?.readOnlyHint === true,
    "read takes no arguments and is read-only",
  );
  const draftSchema = byName.get("edit.draft")?.inputSchema;
  expect(
    isObject(draftSchema?.properties) &&
      isObject(draftSchema.properties.instance) &&
      isDeepStrictEqual(draftSchema.required, ["text"]) &&
      draftSchema.additionalProperties === false,
    "edit.draft requires text, takes an optional instance and no other key",
  );
  expect(
    machineTools.every((name) => {
      const schema = byName.get(name)?.inputSchema;
      return isObject(schema?.properties) && isObject(schema.properties.instance);
    }),
    "every tool of a machine takes the optional argument instance",
  );

  const unknown = await raw("tools/call", { name: "no.such.tool", arguments: {}, _meta: meta });
  expect("error" in unknown && unknown.error.code === -32602, "an unknown tool is a JSON-RPC error -32602");

  const early = await call("edit.draft", { text: "Too early" });
  expect(early.isError && text(early).includes("No instance of edit runs at the URL /."), "an edit tool on the home page is refused");

  const refused = await call("navigate", { url: "/nowhere/at/all" });
  expect(refused.isError && text(refused).includes("not published"), "navigate to the Not_found route is refused");

  const locked = await call("navigate", { url: "/locked" });
  expect(locked.isError && text(locked).includes("/locked, which is not published"), "navigate to the Locked route is refused");

  const voted = await call("vote.cast");
  expect(
    !voted.isError &&
      voted.resultType === "complete" &&
      structured(voted).instance === "router/:home/4:vote" &&
      structured(voted).state === "ready" &&
      structured(voted).url === "/",
    "vote.cast gets the answer of the real vote handler: vote is ready",
  );
  expect(
    isDeepStrictEqual(JSON.parse(text(voted)), structured(voted)),
    "the text content is the structured content as JSON",
  );

  const afterVote = await listTools();
  expect(isDeepStrictEqual(afterVote.tools, listed.tools), "the tool list did not change after the vote");

  const again = await call("vote.cast");
  expect(
    !again.isError && structured(again).state === "ready",
    "vote.cast in the state ready votes again: a Remote machine loads in every state",
  );

  const opened = await call("navigate", { url: "/edit" });
  expect(
    !opened.isError && structured(opened).url === "/edit" && hasInstance(opened, "edit", "router/:edit", "clean"),
    "navigate opens the edit page: edit is clean",
  );

  const onEdit = await listTools();
  expect(isDeepStrictEqual(onEdit.tools, listed.tools), "the tool list did not change on the edit page");

  const badDraft = await call("edit.draft", { text: 5 });
  expect(
    badDraft.isError && text(badDraft) === "The arguments of edit.draft do not decode: text: expected a string, got a number.",
    "a draft that is not a string does not decode",
  );

  const extra = await call("edit.draft", { text: "Draft", colour: "red" });
  expect(extra.isError && text(extra).includes("unknown field colour"), "an argument that the schema does not have is refused");

  const missing = await call("edit.draft");
  expect(missing.isError && text(missing).includes("missing field text"), "a draft with no text is refused");

  const ignored = await call("edit.leave");
  expect(
    ignored.isError && text(ignored) === "edit at router/:edit is in the state clean, which ignores edit.leave.",
    "edit.leave in the state clean is refused",
  );

  const elsewhere = await call("edit.draft", { text: "Draft", instance: "router/:home" });
  expect(
    elsewhere.isError && text(elsewhere).includes("No instance of edit runs at router/:home. The running ones: router/:edit."),
    "an instance that does not run is refused, with the running ones",
  );

  const drafted = await call("edit.draft", { text: "Made by an agent", instance: "router/:edit" });
  expect(
    !drafted.isError && structured(drafted).instance === "router/:edit" && structured(drafted).state === "dirty",
    "edit.draft at the instance router/:edit: edit is dirty",
  );

  const page = await call("read");
  expect(
    !page.isError && typeof structured(page).page === "string" && String(structured(page).page).includes('value="Made by an agent"'),
    "read shows the draft in the field",
  );

  const held = await call("navigate", { url: "/about" });
  expect(
    !held.isError && hasInstance(held, "edit", "router/:edit", "asking") && structured(held).url === "/edit",
    "navigate away from the draft is held: edit is asking and the URL stays /edit",
  );

  const left = await call("edit.leave");
  expect(
    !left.isError && structured(left).state === null && structured(left).url === "/about",
    "edit.leave drops the draft and opens /about: the edit instance ended",
  );

  const item = await call("navigate", { url: "/item/3" });
  expect(!item.isError && hasInstance(item, "item", "router/:item", "loaded"), "navigate to /item/3: item is loaded");

  const read = await call("read");
  const itemPage = String(structured(read).page);
  expect(
    !read.isError && structured(read).url === "/item/3" && itemPage.includes('heading "Item 3"') && itemPage.includes('"$4.00"'),
    "read gets the answer of the real item handler: Item 3 at $4.00",
  );

  const gone = await call("edit.save");
  expect(gone.isError && text(gone).includes("No instance of edit runs"), "edit.save on the item page is refused");

  send({ jsonrpc: "2.0", method: "notifications/cancelled", params: { requestId: 1, _meta: meta } });
  const last = await listTools();
  expect(isDeepStrictEqual(last.tools, listed.tools), "the tool list is the same at the end");
  expect(unexpected.length === 0, "the server sent no notification and no message that answers nothing");
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
