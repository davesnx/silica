# Silica

### Every component is a statechart

**Silica** is a view framework for OCaml where all behaviour lives in statecharts. A component is a machine and a view: the machine says what can happen, the view says what each state looks like, in JSX. The same code runs on the server (native OCaml) and in the browser ([Melange](https://melange.re)), and a model checker walks every state your component can reach before a user does.

No hooks. No effects. No `isLoading && !isError && data != null`.

## Motivation

Most UI bugs aren't rendering bugs, they are behaviour bugs. The spinner that never stops. The request that answers after you left the page. The timer nobody cleared. The modal that is open and closed at once. We keep writing behaviour as booleans and effects scattered across a component, and then we are surprised when they disagree with each other.

Silica takes a strong position on this:

- **Behaviour is a machine, not a side effect of rendering.** The states of a machine are the constructors of a plain OCaml variant, and each state carries its own data. `Saved` carries the time, `Failed` carries the error, `Saving` carries nothing. A state that can't exist can't be written.
- **A state owns its resources.** Timers, calls and subscriptions are what a state owns: one `owns` function gives them for each state. They start when the machine enters it and stop when it leaves it. There is nothing to clean up, so there is nothing to forget to clean up.
- **Handlers are data.** `on_press=Save` is an event value, not a closure. So a view is plain data, and a program can read it: the server renders it, the browser diffs it, and the checker finds every event in it.
- **One language, both sides.** The machine, the view, the routes and the endpoints are written once in OCaml. The server renders the first HTML with the answers of its calls, and the browser adopts that HTML and doesn't call again. A Remote endpoint is one typed value shared by the client that calls it and the server that implements it.
- **If it's a machine, check it.** A statechart is small enough to explore in full. The checker runs every event in every reachable state, checks properties of each view (every pressable element has a name a screen reader can read, no dead ends, every running timer belongs to the current state, no button whose event the machine ignores) and gives the shortest list of events that breaks one. The spec that a check finds exports to [Quint](https://quint-lang.org) and Graphviz.

The browser, the server and the model checker are three hosts of the same machines. That's the whole idea.

## Features

- **[Machines](docs/machines.mld)**: plain variant states, one update function that returns `Go`, `Stay`, `Reenter` or `Ignore`, and timers, calls and sources owned by a state (`~owns`), compared by key after each step
- **[Views in JSX](docs/components.mld)** with [mlx](https://github.com/ocaml-mlx/mlx), child components, props and output through function props
- **[Local, Remote and Live data](docs/data.mld)**: client data, typed endpoints, and server data pushed over a socket to topics and islands, plus [optimistic changes](docs/optimistic.mld)
- **[Server render and hydration](docs/hydration.mld)**: the first HTML holds the answers of the calls, so a page reads with JavaScript off
- **[A typed router](docs/router.mld)**: the routes are the states of a machine, one value reads and writes each URL; guards, preload, unsaved-changes blocks and lazy routes
- **[A model checker](docs/checker.mld)**: every event in every reachable state, built-in properties, your invariants, shortest traces, exports to Quint and Graphviz
- **[silica-ui](docs/ui.mld)**: headless components after [Base UI](https://base-ui.com) that follow the WAI-ARIA patterns: Dialog, Popover, Tooltip, Menu, Select, Combobox, Command menu, Checkbox, Switch, Tabs and Toast. Each one is checked by the checker in the tests
- **[Theming](docs/theming.mld)**: themes are record values (a theme without a token doesn't compile), four ship with the library, and the first paint has the right colours with no script
- **[A server](docs/server.mld)** with sessions, CSRF, hashed assets and Live sockets. It is independent of the HTTP library; the library `silica.adapter` is an adapter over `tiny_httpd`
- **[Deploy](docs/deploy.mld)** as a container (Docker, Fly.io) or as a static export (Cloudflare Pages, Vercel)
- **[Testing](docs/testing.mld)** on native OCaml with no browser: step a machine, check every state, or run the whole runtime on an in-memory platform with a manual clock

## Example

A button that saves a draft. It calls the server, shows the time of the save for two seconds, and goes back to idle. Four states, one Remote call, one timer. `[@@deriving state_shape]` and `[@@deriving event_shape]` name the states and the events for the checker:

```ocaml
open Silica

let save =
  Remote.declare ~path:"/api/save" ~input:Codec.string ~output:Codec.string
    ~rejected:Codec.string

type event = Save | Answer of (string, string) Remote.answer | Reset
[@@deriving event_shape]

type state = Idle | Saving | Saved of string | Failed [@@deriving state_shape]

let machine =
  Machine.make ~id:"save" ~init:Idle ~shape:state_shape ~events:event_shape
    ~owns:(function
      | Saving ->
          [ Remote.call save "draft" (fun answer -> Answer answer) ]
      | Saved _ ->
          [ Source.after 2000 Reset ]
      | Idle | Failed ->
          []
      )
    (fun state event ->
      match (state, event) with
      | (Idle | Failed), Save ->
          Go Saving
      | Saving, Answer (Ok time) ->
          Go (Saved time)
      | Saving, Answer (Rejected _ | Failed _) ->
          Go Failed
      | Saved _, Reset ->
          Go Idle
      | _ ->
          Ignore
    )

let view : state -> event Node.t = function
  | Idle ->
      <button on_press=Save>"Save the draft"</button>
  | Saving ->
      <button disabled=true>"Saving"</button>
  | Saved time ->
      <p>(Node.text ("Saved at " ^ time))</p>
  | Failed ->
      <button on_press=Save>"Not saved. Try again"</button>

let component = Component.make machine view
```

This is [`docs/examples/shared/landing_save.mlx`](docs/examples/shared/landing_save.mlx), the sample that runs on the landing page of the site.

Look at what isn't there. The call starts when the machine enters `Saving` and its answer is dropped if the machine left. The timer starts in `Saved` and dies with it. A double click can't send two requests, because `Saving` doesn't handle `Save`. The answer says what happened: `Ok` with the output, `Rejected` with the application error of the handler, or `Failed` when no answer came. This sample treats both errors the same; `Remote.error_to_string` turns a `Failed` into a sentence when a view needs one.

A machine is a value, so a test steps it on native OCaml, with no browser:

```ocaml
let state, _ = Machine.start Landing_save.machine ()
let state, _, _ = Machine.step Landing_save.machine () state Landing_save.Save

let html = Render.to_string (Landing_save.view state)
(* <button disabled>Saving</button> *)
```

And the checker explores all of it. It has no server, so you give it a sample of what the server could answer; it also tries the failures by itself:

```ocaml
let report =
  Check.run
    (Check.component
       ~samples:[ Check.answers Landing_save.save [ "10:42" ] ]
       Landing_save.component)

let () =
  if Check.passed report then
    Printf.printf "%d states, %d transitions, no problem" report.states
      report.transitions
  else Format.printf "%a" Check.pp_report report
(* 4 states, 6 transitions, no problem *)
```

The [tutorial](docs/tutorial.mld) builds a counter and then a component like this one, with the client file and the server.

## Getting started

Silica isn't on opam yet. Pin it from GitHub; this installs the libraries and the `silica` command. Then make an app:

```sh
opam pin add silica git+https://github.com/davesnx/silica#main
silica new myapp
```

From a checkout of this repository, run `dune exec -- silica new ../myapp` instead.

`silica new` writes a server, a client, a counter component with a Remote call, its tests and an `AGENTS.md`. Run the app:

```sh
cd myapp
npm --prefix client install
dune build
dune exec server/main.exe
```

Then open <http://127.0.0.1:8080>. `dune test` steps the machine, drives the page with `Silica_memory` and runs the model checker.

You need OCaml 5.1 or later, dune 3.20 or later, Melange 7 or later, [mlx](https://github.com/ocaml-mlx/mlx) (the opam packages `mlx` and `ocamlformat-mlx`), `jsonkit` and `jsonkit-melange`, and Node.js for esbuild and Tailwind in the client build.

### Set up a project by hand

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

## Status

Silica is pre-1.0 and experimental. The version is 0.1.0, it isn't published on opam and the API will change. It's a serious attempt to answer one question: what does a view framework look like when behaviour is a statechart all the way down, and OCaml checks the rest?

If that question is interesting to you, read the [tutorial](docs/tutorial.mld), break something and [open an issue](https://github.com/davesnx/silica/issues/new).

## License

MIT © [David Sancho](https://github.com/davesnx)
