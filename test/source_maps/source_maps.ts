import { existsSync, readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";

type SourceMap = { version: number; sources: string[]; mappings: string };
type Segment = { column: number; source: number; line: number; sourceColumn: number };

const base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

const decodeVlq = (text: string): number[] => {
  const values: number[] = [];
  let value = 0;
  let shift = 0;
  for (const character of text) {
    const digit = base64.indexOf(character);
    if (digit < 0) throw new Error(`bad VLQ character ${character}`);
    value += (digit & 31) << shift;
    if (digit & 32) {
      shift += 5;
    } else {
      values.push(value & 1 ? -(value >>> 1) : value >>> 1);
      value = 0;
      shift = 0;
    }
  }
  return values;
};

const decodeMappings = (mappings: string): Segment[][] => {
  let source = 0;
  let line = 0;
  let sourceColumn = 0;
  return mappings.split(";").map((generatedLine) => {
    let column = 0;
    const segments: Segment[] = [];
    for (const field of generatedLine.split(",")) {
      if (field === "") continue;
      const values = decodeVlq(field);
      column += values[0] ?? 0;
      if (values.length < 4) continue;
      source += values[1] ?? 0;
      line += values[2] ?? 0;
      sourceColumn += values[3] ?? 0;
      segments.push({ column, source, line, sourceColumn });
    }
    return segments;
  });
};

const failures: string[] = [];
const literal = '"silica-root"';

for (const dist of process.argv.slice(2)) {
  const scripts = readdirSync(dist).filter((name) => name.endsWith(".js"));
  for (const name of scripts) {
    if (!existsSync(join(dist, name + ".map"))) failures.push(`${dist}/${name}: no source map`);
    else if (!readFileSync(join(dist, name), "utf8").trimEnd().endsWith(`//# sourceMappingURL=${name}.map`))
      failures.push(`${dist}/${name}: no sourceMappingURL comment`);
  }
  const entry = scripts.find((name) => name.startsWith("client_main-"));
  if (!entry || !existsSync(join(dist, entry + ".map"))) {
    failures.push(`${dist}: no client_main bundle with a source map`);
    continue;
  }
  const generated = readFileSync(join(dist, entry), "utf8").split("\n");
  const generatedLine = generated.findIndex((text) => text.includes(literal));
  if (generatedLine < 0) {
    failures.push(`${dist}/${entry}: no ${literal}`);
    continue;
  }
  const generatedColumn = (generated[generatedLine] as string).indexOf(literal);
  const map = JSON.parse(readFileSync(join(dist, entry + ".map"), "utf8")) as SourceMap;
  if (map.version !== 3) failures.push(`${dist}/${entry}.map: version ${map.version}, expected 3`);
  const segment = (decodeMappings(map.mappings)[generatedLine] ?? []).find(
    (candidate) => candidate.column === generatedColumn,
  );
  if (!segment) {
    failures.push(`${dist}/${entry}: no mapping at the mount_app argument ${literal}`);
    continue;
  }
  const source = map.sources[segment.source] ?? "";
  if (!source.endsWith("/client/client_main.mjs")) {
    failures.push(`${dist}/${entry}: mount_app maps to ${source}, expected the Melange client_main.mjs`);
    continue;
  }
  const original = readFileSync(resolve(dirname(join(dist, entry)), source), "utf8").split("\n");
  const text = (original[segment.line] ?? "").slice(segment.sourceColumn);
  if (!text.startsWith(literal))
    failures.push(`${dist}/${entry}: mount_app maps to ${source}:${segment.line + 1}:${segment.sourceColumn}, which is not ${literal}`);
  else console.log(`${dist}/${entry}: mount_app ${literal} -> ${source}:${segment.line + 1}:${segment.sourceColumn + 1}`);
}

if (failures.length > 0) {
  for (const failure of failures) console.error(`source maps: ${failure}`);
  process.exit(1);
}
