import { isDeepStrictEqual } from "node:util";

type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

type Tool = {
  name: string;
  inputSchema: { [key: string]: Json };
  annotations?: { [key: string]: Json };
};

type ToolResult = {
  content: { type: "text"; text: string }[];
  structuredContent?: { [key: string]: Json };
  isError: boolean;
};

type Response =
  | { jsonrpc: "2.0"; id: number; result: { [key: string]: Json } }
  | { jsonrpc: "2.0"; id: number; error: { code: number; message: string } };

const version = "2026-07-28";
const meta = {
  "io.modelcontextprotocol/protocolVersion": version,
  "io.modelcontextprotocol/clientInfo": { name: "silica-todomvc-check", version: "0.0.0" },
  "io.modelcontextprotocol/clientCapabilities": {},
};

const endpoint = process.argv[2] ?? "http://127.0.0.1:5000/mcp";
const called = new Set<string>();
let failures = 0;
let nextId = 1;

type Http = { status: number; contentType: string | null; body: string };

async function post(body: string, headers: { [name: string]: string }): Promise<Http> {
  const response = await fetch(endpoint, {
    method: "POST",
    headers: { "content-type": "application/json", accept: "application/json, text/event-stream", ...headers },
    body,
  });
  const answer = { status: response.status, contentType: response.headers.get("content-type"), body: await response.text() };
  console.log(`-> ${body}\n<- ${answer.status} ${answer.body}`);
  return answer;
}

function standardHeaders(method: string, params: { [key: string]: Json }): { [name: string]: string } {
  const headers: { [name: string]: string } = { "MCP-Protocol-Version": version, "Mcp-Method": method };
  if (method === "tools/call" && typeof params.name === "string") headers["Mcp-Name"] = params.name;
  return headers;
}

async function rpc(method: string, params: { [key: string]: Json } = {}): Promise<{ http: Http; response: Response }> {
  const id = nextId++;
  const message = { jsonrpc: "2.0", id, method, params: { ...params, _meta: meta } };
  const http = await post(JSON.stringify(message), standardHeaders(method, params));
  const response = JSON.parse(http.body) as Response;
  if (response.id !== id) throw new Error(`${method}: the answer has the id ${String(response.id)}, not ${id}`);
  return { http, response };
}

async function request(method: string, params: { [key: string]: Json } = {}): Promise<{ [key: string]: Json }> {
  const { http, response } = await rpc(method, params);
  if ("error" in response) throw new Error(`${method}: ${response.error.message}`);
  if (http.status !== 200 || http.contentType !== "application/json")
    throw new Error(`${method}: status ${http.status}, content-type ${String(http.contentType)}`);
  return response.result;
}

async function call(name: string, args: { [key: string]: Json } = {}): Promise<ToolResult> {
  called.add(name);
  return (await request("tools/call", { name, arguments: args })) as unknown as ToolResult;
}

function expect(condition: boolean, what: string): void {
  console.log(`${condition ? "PASS" : "FAIL"}: ${what}`);
  if (!condition) failures++;
}

function text(result: ToolResult): string {
  return result.content.map((item) => item.text).join("\n");
}

function state(result: ToolResult): Json | undefined {
  return result.structuredContent?.state;
}

async function page(): Promise<{ url: Json | undefined; page: string }> {
  const result = await call("read");
  return { url: result.structuredContent?.url, page: String(result.structuredContent?.page) };
}

function refusal(result: ToolResult, inState: string, tool: string): boolean {
  return result.isError && text(result) === `todos at shared is in the state ${inState}, which ignores ${tool}.`;
}

function todo(id: number, title: string, completed: boolean): string {
  return `checkbox "Todo ${id}: ${title}" checked${completed ? "" : "=false"} [press]`;
}

const annotations: { [name: string]: { [key: string]: Json } } = {
  navigate: { openWorldHint: false },
  read: { readOnlyHint: true, openWorldHint: false },
  "todo.draft": { readOnlyHint: false, destructiveHint: false, idempotentHint: true, openWorldHint: false },
  "todo.add": { readOnlyHint: false, destructiveHint: false, idempotentHint: false, openWorldHint: false },
  "todo.toggle": { readOnlyHint: false, destructiveHint: true, idempotentHint: false, openWorldHint: false },
  "todo.toggle_all": { readOnlyHint: false, destructiveHint: true, idempotentHint: false, openWorldHint: false },
  "todo.destroy": { readOnlyHint: false, destructiveHint: true, idempotentHint: true, openWorldHint: false },
  "todo.clear_completed": { readOnlyHint: false, destructiveHint: true, idempotentHint: true, openWorldHint: false },
  "todo.edit": { readOnlyHint: false, destructiveHint: false, idempotentHint: true, openWorldHint: false },
  "todo.edit_draft": { readOnlyHint: false, destructiveHint: false, idempotentHint: true, openWorldHint: false },
  "todo.save": { readOnlyHint: false, destructiveHint: true, idempotentHint: true, openWorldHint: false },
  "todo.cancel": { readOnlyHint: false, destructiveHint: false, idempotentHint: true, openWorldHint: false },
};

async function main(): Promise<void> {
  const listed = await request("tools/list");
  const tools = listed.tools as unknown as Tool[];
  expect(
    isDeepStrictEqual(
      tools.map((tool) => tool.name),
      Object.keys(annotations),
    ),
    "tools/list gives navigate, read and a tool for each event of the todos machine",
  );
  expect(
    tools.every((tool) => isDeepStrictEqual(tool.annotations, annotations[tool.name])),
    "every tool has the expected annotations",
  );
  const byName = new Map(tools.map((tool) => [tool.name, tool]));
  expect(
    isDeepStrictEqual(byName.get("todo.toggle")?.inputSchema.required, ["id"]) &&
      isDeepStrictEqual(byName.get("todo.add")?.inputSchema.required, ["text"]),
    "todo.toggle requires id and todo.add requires text",
  );
  const navigate = String((byName.get("navigate") as unknown as { description: string }).description);
  expect(
    ["/ - ", "/active - ", "/completed - "].every((pattern) => navigate.includes(`\n${pattern}`)),
    "navigate publishes the three filter routes",
  );

  const empty = await page();
  expect(
    !empty.page.includes("Mark all as complete") && !empty.page.includes("items left"),
    "with no todos, the main section and the footer are not shown",
  );

  const toggleAllEmpty = await call("todo.toggle_all");
  expect(refusal(toggleAllEmpty, "empty", "todo.toggle_all"), "todo.toggle_all with no todos is refused");

  const addEmpty = await call("todo.add", { text: "   " });
  expect(refusal(addEmpty, "empty", "todo.add"), "todo.add with blank text is refused");

  const drafted = await call("todo.draft", { text: "Buy milk" });
  expect(!drafted.isError && state(drafted) === "empty", "todo.draft keeps the state empty");
  expect((await page()).page.includes('textbox "New todo" value="Buy milk"'), "read shows the draft in the new todo field");

  const added = await call("todo.add", { text: "  Buy milk  " });
  const afterAdd = await page();
  expect(
    !added.isError &&
      state(added) === "list" &&
      afterAdd.page.includes(todo(1, "Buy milk", false)) &&
      afterAdd.page.includes('textbox "New todo" value=""') &&
      afterAdd.page.includes('text "1 item left"'),
    "todo.add adds the trimmed text, clears the field and counts 1 item left",
  );

  await call("todo.add", { text: "Walk the dog" });
  await call("todo.add", { text: "Write code" });
  expect((await page()).page.includes('text "3 items left"'), "three todos: 3 items left");

  const toggled = await call("todo.toggle", { id: 1 });
  const afterToggle = await page();
  expect(
    !toggled.isError && afterToggle.page.includes(todo(1, "Buy milk", true)) && afterToggle.page.includes('text "2 items left"'),
    "todo.toggle completes todo 1: 2 items left",
  );
  expect(afterToggle.page.includes('button "Clear completed" [press]'), "a completed todo shows Clear completed");

  const missing = await call("todo.toggle", { id: 99 });
  expect(refusal(missing, "list", "todo.toggle"), "todo.toggle of a todo that does not exist is refused");

  const active = await call("navigate", { url: "/active" });
  const activePage = await page();
  expect(
    !active.isError &&
      activePage.url === "/active" &&
      !activePage.page.includes("Todo 1:") &&
      activePage.page.includes(todo(2, "Walk the dog", false)) &&
      activePage.page.includes('link "Active" current=page href="/active"'),
    "navigate /active shows the active todos only, and the todos stay",
  );

  const completed = await call("navigate", { url: "/completed" });
  const completedPage = await page();
  expect(
    !completed.isError &&
      completedPage.url === "/completed" &&
      completedPage.page.includes(todo(1, "Buy milk", true)) &&
      !completedPage.page.includes("Todo 2:") &&
      completedPage.page.includes('text "2 items left"'),
    "navigate /completed shows the completed todos only, and counts every active todo",
  );

  const nowhere = await call("navigate", { url: "/nowhere" });
  expect(nowhere.isError && text(nowhere).includes("not published"), "navigate to the not found route is refused");

  const all = await call("navigate", { url: "/" });
  const allPage = await page();
  expect(
    !all.isError && allPage.url === "/" && allPage.page.includes("Todo 1:") && allPage.page.includes("Todo 3:"),
    "navigate / shows every todo",
  );

  const opened = await call("todo.edit", { id: 2 });
  expect(
    !opened.isError && state(opened) === "editing" && (await page()).page.includes('textbox "Edit todo 2" value="Walk the dog"'),
    "todo.edit opens todo 2 with its text",
  );
  const again = await call("todo.edit", { id: 2 });
  expect(refusal(again, "editing", "todo.edit"), "todo.edit of the open todo is refused");

  await call("todo.edit_draft", { text: "  Walk the cat " });
  const saved = await call("todo.save");
  expect(
    !saved.isError && state(saved) === "list" && (await page()).page.includes(todo(2, "Walk the cat", false)),
    "todo.edit_draft and todo.save change the text, trimmed",
  );

  const saveClosed = await call("todo.save");
  expect(refusal(saveClosed, "list", "todo.save"), "todo.save with no open todo is refused");
  const draftClosed = await call("todo.edit_draft", { text: "x" });
  expect(refusal(draftClosed, "list", "todo.edit_draft"), "todo.edit_draft with no open todo is refused");
  const cancelClosed = await call("todo.cancel");
  expect(refusal(cancelClosed, "list", "todo.cancel"), "todo.cancel with no open todo is refused");

  await call("todo.edit", { id: 3 });
  await call("todo.edit_draft", { text: "Something else" });
  const cancelled = await call("todo.cancel");
  expect(
    !cancelled.isError && state(cancelled) === "list" && (await page()).page.includes(todo(3, "Write code", false)),
    "todo.cancel keeps the old text",
  );

  await call("todo.edit", { id: 3 });
  await call("todo.edit_draft", { text: "Write tests" });
  const toggledWhileEditing = await call("todo.toggle", { id: 2 });
  const afterCommit = await page();
  expect(
    !toggledWhileEditing.isError &&
      state(toggledWhileEditing) === "list" &&
      afterCommit.page.includes(todo(3, "Write tests", false)) &&
      afterCommit.page.includes(todo(2, "Walk the cat", true)),
    "a tool of another todo saves the open todo first, as a blur does",
  );

  await call("todo.edit", { id: 3 });
  await call("todo.edit_draft", { text: "   " });
  const erased = await call("todo.save");
  expect(
    !erased.isError && !(await page()).page.includes("Todo 3:"),
    "todo.save with blank text deletes the todo",
  );

  await call("todo.toggle", { id: 1 });
  const allDone = await call("todo.toggle_all");
  const allDonePage = await page();
  expect(
    !allDone.isError &&
      allDonePage.page.includes(todo(1, "Buy milk", true)) &&
      allDonePage.page.includes(todo(2, "Walk the cat", true)) &&
      allDonePage.page.includes('text "0 items left"') &&
      allDonePage.page.includes('checkbox "Mark all as complete" checked [press]'),
    "todo.toggle_all with some active completes every todo",
  );
  await call("todo.toggle_all");
  expect(
    (await page()).page.includes('text "2 items left"'),
    "todo.toggle_all with every todo completed makes them all active",
  );

  const clearNone = await call("todo.clear_completed");
  expect(refusal(clearNone, "list", "todo.clear_completed"), "todo.clear_completed with no completed todo is refused");

  await call("todo.toggle", { id: 2 });
  const cleared = await call("todo.clear_completed");
  const clearedPage = await page();
  expect(
    !cleared.isError && !clearedPage.page.includes("Todo 2:") && clearedPage.page.includes(todo(1, "Buy milk", false)),
    "todo.clear_completed deletes the completed todos only",
  );

  const destroyed = await call("todo.destroy", { id: 1 });
  const destroyedPage = await page();
  expect(
    !destroyed.isError && state(destroyed) === "empty" && !destroyedPage.page.includes("Mark all as complete"),
    "todo.destroy deletes the last todo: the state is empty and the main section is gone",
  );
  const destroyedAgain = await call("todo.destroy", { id: 1 });
  expect(refusal(destroyedAgain, "empty", "todo.destroy"), "todo.destroy of a deleted todo is refused");

  await call("todo.add", { text: "Keep me" });
  const before = await page();
  const unchanged = async (what: string): Promise<void> => {
    const after = await page();
    expect(after.page === before.page && after.url === before.url, `${what} changes nothing`);
  };

  const missingId = await call("todo.toggle", {});
  expect(missingId.isError && text(missingId).includes("id"), "todo.toggle with no id is refused and names the field id");
  await unchanged("todo.toggle with no id");
  const wrongType = await call("todo.toggle", { id: "2" });
  expect(wrongType.isError && text(wrongType).includes("id"), "todo.toggle with a string id is refused and names the field id");
  await unchanged("todo.toggle with a string id");
  const extraField = await call("todo.add", { text: "x", priority: 1 });
  expect(extraField.isError && text(extraField).includes("priority"), "todo.add with an unknown field is refused and names it");
  await unchanged("todo.add with an unknown field");
  const textNumber = await call("todo.add", { text: 5 });
  expect(textNumber.isError && text(textNumber).includes("text"), "todo.add with a number for text is refused");
  await unchanged("todo.add with a number for text");
  const otherInstance = await call("todo.add", { text: "x", instance: "router/:todos" });
  expect(
    otherInstance.isError && text(otherInstance) === "No instance of todos runs at router/:todos. The running ones: shared.",
    "a tool call with an unknown instance is refused and names the running one",
  );
  await unchanged("a tool call with an unknown instance");
  const named = await call("todo.toggle", { id: 4, instance: "shared" });
  expect(!named.isError && (await page()).page.includes(todo(4, "Keep me", true)), "a tool call with the instance shared works");
  await call("todo.toggle", { id: 4 });

  const unknown = await rpc("tools/call", { name: "todo.fly", arguments: {} });
  expect(
    "error" in unknown.response && unknown.response.error.code === -32602 && unknown.response.error.message === "Unknown tool: todo.fly",
    "an unknown tool is a JSON-RPC error -32602 that names it",
  );
  await unchanged("an unknown tool");
  const notObject = await rpc("tools/call", { name: "todo.add", arguments: ["milk"] });
  expect("error" in notObject.response && notObject.response.error.code === -32602, "arguments that are not an object are refused");
  await unchanged("arguments that are not an object");

  const discover = await request("server/discover");
  expect(
    isDeepStrictEqual(discover.supportedVersions, [version]) && typeof discover.instructions === "string",
    "server/discover gives the version 2026-07-28 and the instructions",
  );
  const noMethod = await rpc("resources/list");
  expect(
    noMethod.http.status === 404 && "error" in noMethod.response && noMethod.response.error.code === -32601,
    "an unknown method answers 404 with -32601",
  );

  const body = (method: string, params: { [key: string]: Json }, metaVersion = version): string =>
    JSON.stringify({
      jsonrpc: "2.0",
      id: nextId++,
      method,
      params: { ...params, _meta: { ...meta, "io.modelcontextprotocol/protocolVersion": metaVersion } },
    });
  const errorCode = (http: Http): number | undefined => {
    const parsed = JSON.parse(http.body) as Response;
    return "error" in parsed ? parsed.error.code : undefined;
  };
  const addMilk = { name: "todo.add", arguments: { text: "milk" } };
  const noVersion = await post(body("tools/call", addMilk), { "Mcp-Method": "tools/call", "Mcp-Name": "todo.add" });
  expect(noVersion.status === 400 && errorCode(noVersion) === -32020, "a request with no MCP-Protocol-Version header: 400, -32020");
  const otherVersion = await post(body("tools/call", addMilk), {
    "MCP-Protocol-Version": "2025-11-25",
    "Mcp-Method": "tools/call",
    "Mcp-Name": "todo.add",
  });
  expect(otherVersion.status === 400 && errorCode(otherVersion) === -32020, "a version header that differs from _meta: 400, -32020");
  const oldVersion = await post(body("tools/call", addMilk, "2025-11-25"), {
    "MCP-Protocol-Version": "2025-11-25",
    "Mcp-Method": "tools/call",
    "Mcp-Name": "todo.add",
  });
  expect(oldVersion.status === 400 && errorCode(oldVersion) === -32022, "an unsupported version: 400, -32022");
  const otherMethod = await post(body("tools/call", addMilk), { "MCP-Protocol-Version": version, "Mcp-Method": "tools/list", "Mcp-Name": "todo.add" });
  expect(otherMethod.status === 400 && errorCode(otherMethod) === -32020, "an Mcp-Method header that differs from the method: 400, -32020");
  const otherName = await post(body("tools/call", addMilk), { "MCP-Protocol-Version": version, "Mcp-Method": "tools/call", "Mcp-Name": "todo.destroy" });
  expect(otherName.status === 400 && errorCode(otherName) === -32020, "an Mcp-Name header that differs from the tool: 400, -32020");
  const noName = await post(body("tools/call", addMilk), { "MCP-Protocol-Version": version, "Mcp-Method": "tools/call" });
  expect(noName.status === 400 && errorCode(noName) === -32020, "a tools/call with no Mcp-Name header: 400, -32020");
  await unchanged("a request with bad headers");
  const encodedName = await post(body("tools/call", { name: "read", arguments: {} }), {
    "MCP-Protocol-Version": version,
    "Mcp-Method": "tools/call",
    "Mcp-Name": `=?base64?${Buffer.from("read").toString("base64")}?=`,
  });
  expect(encodedName.status === 200 && errorCode(encodedName) === undefined, "an Mcp-Name in Base64 form is taken");
  const badJson = await post("{", standardHeaders("tools/list", {}));
  expect(badJson.status === 400 && errorCode(badJson) === -32700, "a body that is not JSON: 400, -32700");
  const batch = await post("[]", standardHeaders("tools/list", {}));
  expect(batch.status === 400 && errorCode(batch) === -32600, "a body that is not one message: 400, -32600");
  const notification = await post(JSON.stringify({ jsonrpc: "2.0", method: "notifications/cancelled", params: {} }), {});
  expect(notification.status === 202 && notification.body === "", "a notification: 202 with no body");
  const plain = await fetch(endpoint, { method: "POST", headers: { "content-type": "text/plain" }, body: "{}" });
  expect(plain.status === 415, "a body that is not application/json: 415");
  const get = await fetch(endpoint);
  expect(get.status === 405, "GET: 405");
  const foreign = await post(body("tools/call", addMilk), {
    ...standardHeaders("tools/call", addMilk),
    origin: "http://evil.example",
  });
  expect(foreign.status === 403, "a request from another origin: 403");
  const local = await post(body("tools/call", { name: "read", arguments: {} }), {
    ...standardHeaders("tools/call", { name: "read" }),
    origin: new URL(endpoint).origin,
  });
  expect(local.status === 200, "a request from the origin of the server: 200");
  await unchanged("the transport checks");
  await call("todo.destroy", { id: 4 });

  const uncalled = Object.keys(annotations).filter((name) => !called.has(name));
  expect(uncalled.length === 0, `every published tool was called (not called: ${uncalled.join(", ") || "none"})`);
}

main()
  .catch((error: unknown) => {
    console.log(`FAIL: ${error instanceof Error ? error.message : String(error)}`);
    failures++;
  })
  .finally(() => {
    console.log(failures === 0 ? "ALL PASSED" : `${failures} FAILED`);
    process.exitCode = failures === 0 ? 0 : 1;
  });
