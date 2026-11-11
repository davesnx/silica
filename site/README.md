# Silica site

The landing page, the documentation and the component browser of Silica,
built with Silica and silica-ui. See `CONTEXT.md` for the terms.

## Build and run

```sh
npm --prefix site/client install
dune build @all
_build/default/site/server/main.exe --port 8080
```

Then open <http://127.0.0.1:8080/>. The server reads the hashed assets from
`_build/default/site/client/dist` through the esbuild metafile `meta.json`
(`--static`, `--metafile`). Without `--origin`, only local addresses may send a
call.

## Where things are

- `app/shared/`: routes (`site_routes.ml`), the shell (`site_app.mlx`), the
  landing page, the doc page component, the component browser, the search.
- `stories/`: one file per story. Its text is embedded by a dune rule
  (`Site_story_sources`), so the source on a page is the compiled file.
- `gen/`: build-time generators. `docs_gen` reads the `.mld` pages of `docs/`
  (the same files odoc builds) and fails on unknown markup or a dead link;
  `embed_gen` embeds file text; `facts_gen` reads the statechart goldens of
  `test/ui_check/spec/`; `landing_gen` runs the checker on the landing save
  machine and writes the spec that the landing diagram draws.
- `client/`: the Melange entry, Tailwind and esbuild rules, `themes.css`.
- `server/`: the page, the `/api/doc` endpoint and the endpoints of the live
  samples.

A doc page is the answer of `/api/doc`. The server writes the answer into the
first HTML, so a page reads with JavaScript off, and a link fetches the next
page when the pointer rests on it.

## Tests

`test/site/` (server render of every route, link check, component coverage,
token contrast), `test/e2e/site.test.ts` and `test/e2e/components.test.ts`
(Playwright), and the token lint in `test/ui/test_tokens.ml`.
