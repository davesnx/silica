import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { createServer } from "node:net";
import { resolve } from "node:path";

const sessions = Number(process.argv[2] ?? "1000");
const issues = Number(process.argv[3] ?? "30");
const build = resolve(process.env["BUILD"] ?? "_build/default");

const freePort = (): Promise<number> =>
  new Promise((ok, fail) => {
    const s = createServer();
    s.once("error", fail);
    s.listen(0, "127.0.0.1", () => {
      const a = s.address();
      s.close(() => ok(typeof a === "object" && a ? a.port : 0));
    });
  });

const port = await freePort();
const base = `http://127.0.0.1:${port}`;
const server = spawn(
  resolve(build, "linear/server/main.exe"),
  ["--port", String(port), "--max-connections", "4096", "--issues", String(issues), "--static", resolve(build, "linear/client/dist"), "--metafile", resolve(build, "linear/client/meta.json")],
  { stdio: "ignore" },
);
const status = (field: string): string =>
  readFileSync(`/proc/${server.pid}/status`, "utf8").split("\n").find((l) => l.startsWith(field))?.split(/\s+/)[1] ?? "?";
const cpuSeconds = (): number => {
  const fields = readFileSync(`/proc/${server.pid}/stat`, "utf8").split(") ")[1]?.split(" ") ?? [];
  return (Number(fields[11]) + Number(fields[12])) / 100;
};
try {
  for (let i = 0; ; i++) {
    try {
      await fetch(base + "/_silica/health");
      break;
    } catch {
      if (i > 100) throw new Error("server did not start");
      await new Promise((r) => setTimeout(r, 100));
    }
  }
  const rssBefore = status("VmRSS:");
  type Client = { socket: WebSocket; states: number };
  const clients: Client[] = [];
  let lastArrival = 0;
  let expected = 0;
  const connectStarted = Date.now();
  for (let i = 0; i < sessions; i++) {
    const res = await fetch(base + "/");
    const cookie = (res.headers.get("set-cookie") ?? "").split(";")[0] ?? "";
    const html = await res.text();
    const csrf = /name="silica-csrf" content="([^"]+)"/.exec(html)?.[1] ?? "";
    const client: Client = {
      socket: new WebSocket(`ws://127.0.0.1:${port}/_silica/live`, {
        headers: { cookie, origin: base },
      } as unknown as string[]),
      states: 0,
    };
    await new Promise<void>((ok, fail) => {
      client.socket.onopen = () => {
        client.socket.send(JSON.stringify({ t: "hello", v: 2, csrf, sub: { topic: "issues", key: "all" } }));
      };
      client.socket.onmessage = (m) => {
        if (!String(m.data).startsWith('{"t":"state"')) return;
        client.states++;
        if (client.states === 1) ok();
        else if (client.states === expected) lastArrival = Date.now();
      };
      client.socket.onerror = () => fail(new Error(`socket ${i} failed`));
    });
    clients.push(client);
  }
  const connectSeconds = (Date.now() - connectStarted) / 1000;
  await new Promise((r) => setTimeout(r, 500));
  const rssConnected = status("VmRSS:");
  const threads = status("Threads:");
  const results: number[] = [];
  for (let round = 0; round < 5; round++) {
    expected = 2 + round;
    lastArrival = 0;
    const page = await fetch(base + "/");
    const cookie = (page.headers.get("set-cookie") ?? "").split(";")[0] ?? "";
    const csrf = /name="silica-csrf" content="([^"]+)"/.exec(await page.text())?.[1] ?? "";
    const cpuBefore = cpuSeconds();
    const sent = Date.now();
    const created = await fetch(base + "/api/issues/create", {
      method: "POST",
      headers: { cookie, origin: base, "content-type": "application/json", "x-silica-csrf": csrf },
      body: JSON.stringify({ title: `Fan-out ${round}`, description: "", status: ["Todo"], priority: ["Medium"] }),
    });
    if (created.status !== 200) throw new Error(`create: ${created.status} ${await created.text()}`);
    while (clients.some((c) => c.states < expected)) await new Promise((r) => setTimeout(r, 2));
    results.push(lastArrival - sent);
    console.error(`round ${round}: ${lastArrival - sent} ms, server CPU ${(cpuSeconds() - cpuBefore).toFixed(2)} s`);
  }
  results.sort((a, b) => a - b);
  console.log(
    JSON.stringify({
      sessions,
      issues,
      connectSeconds,
      rssBefore: Number(rssBefore) / 1024,
      rssConnected: Number(rssConnected) / 1024,
      threads: Number(threads),
      publishToLastClientMs: results,
      medianMs: results[Math.floor(results.length / 2)],
    }),
  );
  for (const c of clients) c.socket.close();
} finally {
  server.kill("SIGKILL");
}
