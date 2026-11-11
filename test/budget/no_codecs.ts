import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

const [markersPath, ...apps] = process.argv.slice(2);
if (!markersPath || apps.length % 2 !== 0) throw new Error("usage: no_codecs.ts markers.json (name dist)...");
const markers = JSON.parse(readFileSync(markersPath, "utf8")) as Record<string, string[]>;

const failures: string[] = [];
for (let i = 0; i < apps.length; i += 2) {
  const name = apps[i] as string;
  const dist = apps[i + 1] as string;
  const wanted = markers[name];
  if (!wanted) {
    failures.push(`${name}: no markers`);
    continue;
  }
  for (const file of readdirSync(dist).filter((file) => file.endsWith(".js"))) {
    const text = readFileSync(join(dist, file), "utf8");
    for (const marker of wanted)
      if (text.includes(marker)) failures.push(`${name}: ${file} holds ${marker}, a string of a derived codec`);
  }
}
if (failures.length > 0) {
  console.error(
    "no_codecs: a client bundle holds machine codecs; only development entry points may call register_codecs:\n  " +
      failures.join("\n  "),
  );
  process.exit(1);
}
