import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

const kinds: Record<string, { markers: string[]; what: string }> = {
  client: {
    markers: ["/_silica/dev", "data-silica-dev-overlay"],
    what: "code of the silica dev client, which silica dev serves on its own",
  },
  handoff: {
    markers: [
      "__silica_handoff",
      "__silica_dev",
      "silica-resumed",
      "its state type changed",
      "its view raised after the reload",
      "its shape has no fingerprint",
    ],
    what: "hot reload code (suspend, dispose, resume and Handoff), which only a development build injects",
  },
  inspector: {
    markers: ["/_silica/inspect", "__silica_inspect_tab"],
    what: "the inspector target, which only a development build injects",
  },
};

const [kind, ...apps] = process.argv.slice(2);
const chosen = kind === undefined ? undefined : kinds[kind];
if (!chosen || apps.length === 0 || apps.length % 2 !== 0)
  throw new Error("usage: no_dev_client.ts (client|handoff|inspector) (name dist)...");

const found: string[] = [];
for (let i = 0; i < apps.length; i += 2) {
  const name = apps[i] as string;
  const dist = apps[i + 1] as string;
  for (const file of readdirSync(dist).filter((file) => file.endsWith(".js"))) {
    const text = readFileSync(join(dist, file), "utf8");
    for (const marker of chosen.markers) if (text.includes(marker)) found.push(`${name}: ${file} holds ${marker}`);
  }
}
if (found.length > 0) {
  console.error(`no_dev_client: a bundle holds ${chosen.what}:\n  ` + found.join("\n  "));
  process.exit(1);
}
