# Browser benchmarks

The driver `run.ts` measures, in Chromium through Playwright:

- the js-framework-benchmark keyed operations for `bench/keyed` (Silica) and
  `bench/compare/{vanilla,react,solid}`, at each CPU throttle rate;
- the JS heap after create 1,000 rows;
- a keystroke in `bench/filter` (Silica, 1,000 rows);
- bundle bytes (raw, gzip, brotli);
- page loads of the site server (`/`, the largest docs page and a lazy route):
  first contentful paint and the time until the root gets `data-silica-ready`;
- Live: a vote in one page of the example app to the new count in another.

It writes one JSON file and a Markdown table next to it. The method is in the
`meta.method` field of the JSON.

## Build

From the repository root:

```sh
dune build @all
(cd bench/compare && npm ci && npm run build)
(cd bench/browser && npm ci)
```

The comparison apps are not part of the dune build.

## Run

Playwright starts Chromium (`/usr/bin/chromium`, or `CHROME`), so run it outside
the sandbox:

```sh
TMPDIR=/tmp node --experimental-strip-types bench/browser/run.ts --out bench/browser/results/all.json
```

Only Silica, only some operations, at 4x only:

```sh
TMPDIR=/tmp node --experimental-strip-types bench/browser/run.ts \
  --apps silica --suites keyed,memory --ops create1k,update,swap --cpu 4 --out bench/browser/results/silica.json
```

`--help` lists the options: `--apps`, `--ops`, `--suites`
(`bundle,keyed,memory,filter,load,live`), `--cpu`, `--runs`, `--warmup`,
`--load-runs`, `--docs-page`, `--label`, `--out`.

Type check: `(cd bench/browser && npm run typecheck)`.
