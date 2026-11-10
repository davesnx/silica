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
