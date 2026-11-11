# {{name}}

A Silica app. Every component is a machine (states and events) and a view.

## Layout

- `app/shared/`: code for the server and the browser. `counter.mlx` is a
  component; `api.ml` declares the Remote endpoints.
- `app/native/`, `app/melange/`: build `app/shared/` for each side. Do not put
  code here.
- `server/main.ml`: the HTTP server. Implement each endpoint of `api.ml` here.
- `client/client_main.ml`: the browser entry.
- `test/`: `counter_steps.ml` steps the machine, `counter_page.ml` drives the
  page by role and name with `Silica_memory`.
- `check/main.ml`: the list of checks that the model checker runs, one
  `Check_suite.entry` for each component. `check/spec/` holds the golden files
  of each machine: its states and edges (`.spec.txt`) and the text view of
  each state (`.views.txt`).

## Commands

Run every command from this directory.

- Install the bundler once: `npm --prefix client install`
- Build: `dune build`
- Run: `dune exec server/main.exe`, then open http://127.0.0.1:8080
- Test: `dune test`
- Check the machines: `silica check`, or `silica check --json` for a report
  that a program reads. Exit code 0: passed; 1: findings; 2: a check stopped at
  its state limit; 3: a setup error.
- Write the golden files again after an intended change of behaviour:
  `dune exec ./check/main.exe -- golden check/spec --write`
- Format: `dune fmt`

## Rules

- Put behaviour in the machine. A view only reads the state and names events.
- Give each new component a test in `test/`: step its machine, drive it with
  `Silica_memory`, and add a `Check_suite.entry` for it in `check/main.ml`.
- When the checker reports a finding, read its rule, hint, trace and view, and
  fix the machine or the view. Do not loosen the check.
- When `dune test` reports golden drift, read the changed states and edges. If
  the change is intended, write the golden files again; if not, fix the code.
- Before you finish, run `dune build`, `dune test` and `dune fmt`. All three
  must pass with no output from `dune fmt`.
