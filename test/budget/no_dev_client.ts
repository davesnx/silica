import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

const markers = ["/_silica/dev", "data-silica-dev-overlay"];
const apps = process.argv.slice(2);
if (apps.length === 0 || apps.length % 2 !== 0) throw new Error("usage: no_dev_client.ts (name dist)...");

const found: string[] = [];
for (let i = 0; i < apps.length; i += 2) {
  const name = apps[i] as string;
  const dist = apps[i + 1] as string;
  for (const file of readdirSync(dist).filter((file) => file.endsWith(".js"))) {
    const text = readFileSync(join(dist, file), "utf8");
    for (const marker of markers) if (text.includes(marker)) found.push(`${name}: ${file} holds ${marker}`);
  }
}
if (found.length > 0) {
  console.error(
    "no_dev_client: a bundle holds code of the silica dev client, which silica dev serves on its own:\n  " +
      found.join("\n  "),
  );
  process.exit(1);
}
