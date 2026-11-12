# Mini Linear

The reference app of this repository: an issue list, an issue detail, a create
modal and live updates, over mocked data. See `CONTEXT.md` for the term.

## Build and run

```sh
npm --prefix linear/client install
dune build @all
_build/default/linear/server/main.exe --port 8080
```

The server reads the hashed assets from `_build/default/linear/client/dist`
through the esbuild metafile `meta.json`. Without `--origin`, only local
addresses may open a socket or send a call.

A visitor gets the next mock user (Ada, Ben, Cleo) when the session starts.
After a server restart the sessions are gone: the browser reloads the page once
and gets a new session.

## Traces in development

`linear/client_dev` is a second client for development: the same app, which
also registers the codecs of `Linear_codecs` (so a hot reload keeps the state
of the machines) and records a trace with `Silica.Trace.Recorder`, with data. Run it with

```sh
dune exec -- silica dev --server linear/server/main.exe --client linear/client_dev
```

Under `silica dev` the server has `SILICA_DEV_STATE`, so each page has
`<meta name="linear-trace">` and the endpoint `/api/trace` exists. When the
tab is hidden, the client posts its trace there, and the server writes it into
`linear/traces/` (`--traces DIR` or `SILICA_LINEAR_TRACES` changes the folder)
under a name it makes, `trace-<UTC time>-<n>.jsonl`. The endpoint refuses a
trace of more than 512 KiB or 2000 transitions, a file that is not a trace,
and every trace once the folder holds 200 trace files. Without development
mode there is no endpoint and no meta, and the client records nothing. Git
ignores `linear/traces/`: the traces hold what you typed.

Compare a trace with the specs and the code of mini Linear:

```sh
cd test/linear_check
../../_build/default/test/linear_check/main.exe conform ../../linear/traces/*.jsonl
```

`linear/client`, the bundle that the budgets measure and that a deploy
serves, has no recorder and no codecs.

## Tailwind

`linear/client/dune` holds the build rule. Tailwind (`@tailwindcss/cli`, from
`node_modules`) reads `input.css`, scans `linear/app/shared` for class names
(`@source`) and writes `app.css`. esbuild adds a hash to the file name and the
metafile gives the page its URL.

`source(none)` turns off the automatic scan of the working folder. In the dune
build folder that scan also reads earlier outputs (`out`, `dist`, `app.css`), so
the CSS would depend on what was built before.

Write each class as a complete string literal. The scan reads the source text,
so a name built at run time (`"bg-" ^ colour`) gets no CSS. For a condition use
`Class.join`, which drops empty strings:

```ocaml
Class.join [ "grow truncate"; (if closed then "line-through" else "") ]
```

Dark mode follows `prefers-color-scheme`. There is no toggle.
