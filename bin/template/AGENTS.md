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
  page by role and name with `Silica_memory`, `check_counter.ml` runs the
  model checker.

## Commands

Run every command from this directory.

- Install the bundler once: `npm --prefix client install`
- Build: `dune build`
- Run: `dune exec server/main.exe`, then open http://127.0.0.1:8080
- Test: `dune test`
- Check the machines: `silica check`
- Format: `dune fmt`

## Rules

- Put behaviour in the machine. A view only reads the state and names events.
- Give each new component a test in `test/`: step its machine, drive it with
  `Silica_memory`, and add it to the checker.
- When the checker prints a trace, fix the machine or the view. Do not loosen
  the check.
- Before you finish, run `dune build`, `dune test` and `dune fmt`. All three
  must pass with no output from `dune fmt`.
