# Silica

### Every component is a statechart

**Silica** is a view framework for OCaml where all behaviour lives in statecharts. A component is a machine and a view: the machine says what can happen, the view says what each state looks like, in JSX. The same code runs on the server (native OCaml) and in the browser ([Melange](https://melange.re)), and a model checker walks every state your component can reach before a user does.

No hooks. No effects. No `isLoading && !isError && data != null`.

## Motivation

Most UI bugs aren't rendering bugs, they are behaviour bugs. The spinner that never stops. The request that answers after you left the page. The timer nobody cleared. The modal that is open and closed at once. We keep writing behaviour as booleans and effects scattered across a component, and then we are surprised when they disagree with each other.

Silica takes a strong position on this:

- **Behaviour is a machine, not a side effect of rendering.** The states of a machine are the constructors of a GADT, and each state carries its own data. `Saved` carries the time, `Failed` carries the error, `Saving` carries nothing. A state that can't exist can't be written.
- **A state owns its resources.** Timers, calls and subscriptions are declared on a state. They start when the machine enters it and stop when it leaves it. There is nothing to clean up, so there is nothing to forget to clean up.
- **Handlers are data.** `on_press=Save` is an event value, not a closure. So a view is plain data, and a program can read it: the server renders it, the browser diffs it, and the checker finds every event in it.
- **One language, both sides.** The machine, the view, the routes and the endpoints are written once in OCaml. The server renders the first HTML with the answers of its calls, and the browser adopts that HTML and doesn't call again. A Remote endpoint is one typed value shared by the client that calls it and the server that implements it.
- **If it's a machine, check it.** A statechart is small enough to explore in full. The checker runs every event in every reachable state, checks properties of each view (every pressable element has a name a screen reader can read, no dead ends, every running timer belongs to the current state) and gives the shortest list of events that breaks one. The shape of each machine exports to [Quint](https://quint-lang.org) and Graphviz.

The browser, the server and the model checker are three hosts of the same machines. That's the whole idea.

## Features

- **[Machines](docs/machines.mld)**: typed states, declared edges (`~to_`), timers (`~after`) and sources (`~invoke`) owned by a state
- **[Views in JSX](docs/components.mld)** with [mlx](https://github.com/ocaml-mlx/mlx), child components, props and output through function props
- **[Local, Remote and Live data](docs/data.mld)**: client data, typed endpoints, and server data pushed over a socket to topics and islands, plus [optimistic changes](docs/optimistic.mld)
- **[Server render and hydration](docs/hydration.mld)**: the first HTML holds the answers of the calls, so a page reads with JavaScript off
- **[A typed router](docs/router.mld)**: the routes are the states of a machine, one value reads and writes each URL; guards, preload, unsaved-changes blocks and lazy routes
- **[A model checker](docs/checker.mld)**: every event in every reachable state, built-in properties, your invariants, shortest traces, exports to Quint and Graphviz
- **[silica-ui](docs/ui.mld)**: headless components after [Base UI](https://base-ui.com) that follow the WAI-ARIA patterns: Dialog, Popover, Tooltip, Menu, Select, Combobox, Command menu, Checkbox, Switch, Tabs and Toast. Each one is checked by the checker in the tests
- **[Theming](docs/theming.mld)**: themes are record values (a theme without a token doesn't compile), four ship with the library, and the first paint has the right colours with no script
- **[A server](docs/server.mld)** with sessions, CSRF, hashed assets and Live sockets. It is independent of the HTTP library; the repository has one adapter, over `tiny_httpd`
- **[Deploy](docs/deploy.mld)** as a container (Docker, Fly.io) or as a static export (Cloudflare Pages, Vercel)
- **[Testing](docs/testing.mld)** on native OCaml with no browser: step a machine, check every state, or run the whole runtime on an in-memory platform with a manual clock

## Example

A button that saves a draft. It calls the server, shows the time of the save for two seconds, and goes back to idle. Four states, one Remote call, one timer:

```ocaml
open Silica

let save =
  Remote.declare ~path:"/api/save" ~input:Codec.string ~output:Codec.string

type event = Save | Answer of (string, Remote.error) result | Reset

module States = struct
  type _ t =
    | Idle : unit t
    | Saving : unit t
    | Saved : string t
    | Failed : Remote.error t
end

module M = Machine.Make (States)

let machine =
  M.make ~id:"save" ~init:(M.at Idle ())
    [
      M.state Idle "idle" ~to_:[ Saving ] (fun () -> function
        | Save ->
            M.goto Saving ()
        | Answer _ | Reset ->
            M.unhandled
        );
      M.state Saving "saving" ~to_:[ Saved; Failed ]
        ~invoke:[ Source.call save (fun () -> "draft") (fun r -> Answer r) ]
        (fun () -> function
          | Answer (Ok time) ->
              M.goto Saved time
          | Answer (Error error) ->
              M.goto Failed error
          | Save | Reset ->
              M.unhandled
          );
      M.state Saved "saved" ~to_:[ Idle ]
        ~after:[ (2000, Reset) ]
        (fun _ -> function Reset -> M.goto Idle () | _ -> M.unhandled);
      M.state Failed "failed" ~to_:[ Saving ] (fun _ -> function
        | Save ->
            M.goto Saving ()
        | Answer _ | Reset ->
            M.unhandled
        );
    ]

let view (snapshot : M.snapshot) : event Node.t =
  match snapshot with
  | M.At (Idle, ()) ->
      <button on_press=Save>"Save the draft"</button>
  | M.At (Saving, ()) ->
      <button disabled=true>"Saving"</button>
  | M.At (Saved, time) ->
      <p>(Node.text ("Saved at " ^ time))</p>
  | M.At (Failed, _) ->
      <button on_press=Save>"Not saved. Try again"</button>

let component = Component.make machine view
```

This is [`docs/examples/shared/landing_save.mlx`](docs/examples/shared/landing_save.mlx), the sample that runs on the landing page of the site.

Look at what isn't there. The call starts when the machine enters `Saving` and its answer is dropped if the machine left. The timer starts in `Saved` and dies with it. A double click can't send two requests, because `Saving` doesn't handle `Save`.

A machine is a value, so a test steps it on native OCaml, with no browser:

```ocaml
let snapshot, _ = Machine.start Landing_save.machine
let snapshot, _, _ = Machine.send Landing_save.machine snapshot Landing_save.Save

let html = Render.to_string (Landing_save.view snapshot)
(* <button disabled>Saving</button> *)
```

And the checker explores all of it. It has no server, so you give it a sample of what the server could answer; it also tries the failures by itself:

```ocaml
match
  Check.component
    ~samples:[ Check.answers Landing_save.save [ "10:42" ] ]
    Landing_save.component
with
| Check.Ok { states; transitions; _ } ->
    Printf.printf "%d states, %d transitions, no problem" states transitions
| _ -> print_string "problem"
(* 5 states, 7 transitions, no problem *)
```

The [tutorial](docs/tutorial.mld) builds a counter and then a component like this one, with the client file and the server.

## Getting started

Silica isn't on opam yet. Use it from a checkout: pin it, or put the checkout inside your dune workspace.

```sh
git clone https://github.com/davesnx/silica
opam pin add silica ./silica
opam pin add silica-ui ./silica
```

You need OCaml 5.1 or later, dune 3.20 or later, Melange 7 or later, [mlx](https://github.com/ocaml-mlx/mlx) (the opam packages `mlx` and `ocamlformat-mlx`), `jsonkit` and `jsonkit-melange`, and Node.js for esbuild and Tailwind in the client build.

Declare the mlx dialect in your `dune-project`, next to `(using melange 1.0)`:

```clojure
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (format
   (run ocamlformat-mlx %{input-file}))
  (preprocess
   (run mlx-pp %{input-file}))))
```

and add Silica to a library:

```clojure
(library
 (name app)
 (libraries silica silica-ui)
 (preprocess
  (pps silica.ppx)))
```

The Melange library is the same with `(modes melange)` and `(pps silica.ppx melange.ppx)`. [Install](docs/install.mld) has the full setup: the shared folder, the two libraries and the browser entry.

### Run the site

The landing page, the documentation and the component browser are one Silica app. It's the fastest way to see everything working:

```sh
npm --prefix site/client install
dune build @all
_build/default/site/server/main.exe --port 8080
```

Then open <http://127.0.0.1:8080>. The mini Linear clone runs the same way, with `linear` in place of `site`.

### Run the tests

```sh
dune test                                 # machines, checker, server, docs, bundle size budgets
dune build @test/browser/browser-test     # the DOM renderer and hydration, on jsdom
npm --prefix test/e2e test                # Playwright, in Chromium
```

The two browser suites need `npm install` in `test/browser` and `test/e2e` first.

## What's in this repository

| Folder | What it is |
|---|---|
| `lib/` | `silica`: machines, components, views, commands, routes, codecs, the host runtime |
| `ppx/` | `silica.ppx`: JSX elements to `Silica.Node.element` calls |
| `browser/` | `silica.browser`: the browser platform (Melange) |
| `server/` | `silica.server`: the server logic (native) |
| `memory/` | `silica.memory`: an in-memory platform for tests |
| `check/` | `silica.check`: the model checker |
| `ui/` | `silica-ui`: the headless components and the theme API |
| `docs/` | the documentation (odoc pages) and its compiled examples |
| `site/` | the landing page, the docs and the component browser, built with Silica |
| `linear/` | Mini Linear, the reference app: an issue tracker with live updates |
| `example/` | a smaller example app, and the `tiny_httpd` adapter |
| `bench/`, `test/` | the benchmarks and the tests |

[`CONTEXT.md`](CONTEXT.md) is the glossary. Silica uses each of its words for one thing only.

## Status

Silica is pre-1.0 and experimental. The version is 0.1.0, it isn't published on opam and the API will change. It's a serious attempt to answer one question: what does a view framework look like when behaviour is a statechart all the way down, and OCaml checks the rest?

If that question is interesting to you, read the [tutorial](docs/tutorial.mld), break something and [open an issue](https://github.com/davesnx/silica/issues/new).

## License

MIT © [David Sancho](https://github.com/davesnx)
