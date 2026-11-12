import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

const [markersPath, name, dist] = process.argv.slice(2);
if (!markersPath || !name || !dist) throw new Error("usage: dev_bundle.ts codec_markers.json name dist");
const codecMarkers = (JSON.parse(readFileSync(markersPath, "utf8")) as Record<string, string[]>)[name] ?? [];
const text = readdirSync(dist)
  .filter((file) => file.endsWith(".js"))
  .map((file) => readFileSync(join(dist, file), "utf8"))
  .join("\n");
const missing = [
  ...codecMarkers.filter((marker) => !text.includes(marker)).map((marker) => `the recorder codec marker ${marker}`),
  ...["__silica_handoff", "silica-resumed"].filter((marker) => !text.includes(marker)).map((marker) => `the hot reload marker ${marker}`),
];
if (missing.length > 0) {
  console.error(`dev_bundle: the ${name} development bundle (${dist}) misses:\n  ` + missing.join("\n  "));
  process.exit(1);
}
