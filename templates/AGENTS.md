# {{name}}

A Silica app. Every component is a machine (states and events) and a view.
The checker runs every event in every state that a component can reach, so
you prove a change right with no browser.

Docs for agents, as Markdown, in the Silica repository
(https://github.com/davesnx/silica):

- `skills/silica-verify/references/verification.md`: the loop below with
  real output, every command, the report fields, a table of what to do for
  each rule, the property conditions, and `Silica_memory` tests.
- `skills/silica-verify/references/checker.md`: the checker in full.
- `skills/silica-verify/references/attributes.md`: the type of each
  attribute value in mlx that is not a string, such as ``type_=`number``.
- `skills/silica-verify/references/api/`: every public signature with its
  documentation, one file for each library (`silica.md`, `check.md`,
  `memory.md`, `server.md`). Look a name up there instead of guessing it.
- `docs/*.mld`: every page. In a checkout,
  `dune build site/server/llms-full.txt` writes them all as one Markdown
  file, `_build/default/site/server/llms-full.txt`, with an index in
  `llms.txt` beside it.

## Layout

- `app/shared/`: code for the server and the browser. `counter.mlx` is a
  component; `api.ml` declares the Remote endpoints.
- `app/native/`, `app/melange/`: build `app/shared/` for each side. Do not put
  code here.
- `server/main.ml`: the HTTP server. Implement each endpoint of `api.ml` here.
- `client/client_main.ml`: the browser entry.
- `test/`: `counter_steps.ml` steps the machine, `counter_page.ml` drives the
  page by role and name with `Silica_memory`.
- `check/checks.ml`: the checks, in the library `checks`: one value for each
  component, such as `Checks.counter`. Tests use them too: the test that
  `silica script` writes evaluates its rule with the check.
- `check/main.ml`: the suite of the model checker, one `Check_suite.entry`
  for each check of `Checks`. `check/spec/` holds the golden files of each machine:
  its states and edges (`.spec.txt`, and `.spec.json` for programs) and the
  text view of each state (`.views.txt`).

## Commands

Run every command from this directory.

- Build: `dune build`. Test: `dune test`. Format: `dune fmt` (it exits 1
  when it rewrote a file; then `dune build @fmt` must pass).
- Check the machines: `silica check`. It also compares the golden files in
  `check/spec`: a missing or changed one is a finding (`--no-golden` leaves
  them out). `silica check --json` gives a report that a program reads;
  `--traces DIR` writes a trace file for each violation. Exit code 0: passed;
  1: findings; 2: a check stopped at its state limit; 3: a setup error
  (stderr has the reason, such as a compile error).
- Draw the machines: `silica check --graphs graphs` also writes, for each
  machine, `graphs/<id>.svg` (the chart), `graphs/<id>.txt` (one fact per
  line, the form to read) and `graphs/<id>.md` (Mermaid).
- Run one entry: `dune exec ./check/main.exe -- --only ID`.
- List the rules with their hints: `dune exec ./check/main.exe -- rules`.
- Replay a trace: `silica replay ID FILE`. Make a test of it:
  `silica script ID FILE --output TEST_FILE`.
- Write the golden files again after an intended change of behaviour:
  `dune exec ./check/main.exe -- golden check/spec --write`
- Run in development: `npm --prefix client install` once, then `silica dev`
  and open http://127.0.0.1:8080. With no browser: `silica dev --json` in the
  background prints one JSON line for each event; the lines that do not start
  with `{` are the output of your server:
  - `{"t":"dev","url":"http://127.0.0.1:8080"}` first.
  - `{"t":"build","status":"started"}` for a rebuild (the first build reports
    only its end), then `"ok"` or `"failed"` with
    `errors`: each has `file`, `line`, `col`, `end_line`, `end_col`,
    `severity`, `message` and `excerpt` (a list of `[line, text]`).
  - `{"t":"server","status":"started"}` or `"restarted"`, with `pid` and
    `ms`; `"crashed"` with `code` and `output`.
  - `{"t":"reload","what":"code"}` (the page swaps in the new bundle and
    keeps the state of its machines), `"page"` or `"css"`.
  - `{"t":"hot-reload","status":"kept","kept":[...],"restarted":[...]}`: each
    `restarted` entry has `path`, `machine` and `reason`, such as
    `"its snapshot does not decode: ..."`. A machine keeps its state when
    `app/shared/codecs.ml` registers a state codec for it with
    `Machine.register_codecs`, and the old snapshot decodes with the new
    codec. `client/dev_codecs.ml` calls it in the development bundle only.

  After an edit, wait for `build` `ok` and `server` `restarted` (and
  `hot-reload` when a page is open), or for `failed`, and fix each error.
  Stop it with SIGINT or SIGTERM.
- Read the machines while `silica dev` runs, from another shell, with no
  browser: `silica inspect tail --json` prints one JSON line for each step of
  each machine of the open pages and of the Live islands of the server, with
  `target`, `machine`, `instance`, `from`, `to`, `change`, `event`, `cause`,
  `actions`, `owns` (timers and sources after the step), and `data` and
  `snapshot` when the machine registered its codecs. `--machine ID`,
  `--path PATH`, `--event NAME` and `--change kept` keep the output small;
  `--count N` stops after N lines. `silica inspect tree --json` gives each
  running instance with its state, data and resources.
  `silica inspect export -o run.jsonl` writes the steps of a page as a trace
  file for `silica inspect replay`, `silica replay` and `silica conform`.
  `silica inspect graph ID --specs check/spec` prints the spec of a machine
  with its current state, the edges the app took, and `drift` lines for the
  edges the spec does not have.

## The loop for each change

1. Write the requirement as a check first: a property in `~properties` of the
   check in `check/checks.ml` (`Check.always`, `Check.leads_to`,
   `Check.never_after`), and the `~samples`, `~texts` (text that the checker
   types into each field) and `~inputs` (other events that no element gives)
   that reach the new states. A new component gets its own
   `Check_suite.entry`. A rule about steps ("Reset always gives 0", "Increment
   adds the step") is a `Check.always` with `Check.event` and
   `Check.transition`: it is read on every step; see "A rule about steps" in
   the verification reference.
2. Put behaviour in the machine. A view only reads the state and names
   events. Disable an element when its event does nothing in a state:
   `disabled=(not (Machine.can machine () state Event))`. `Machine.make`
   takes `~name` (the name of each state in the spec, the checker and the
   traces), `?states` (every name that `name` gives, so that the checker
   reports the states that no run reached), `?event_name` (the name of each
   event on the edges; an event with no name is labelled by its cause, such
   as `press button "Save"`, `after 2000ms` or `ok /api/save`), `?during`
   (the timers, calls and sources of each state). Give a new machine a state
   codec in `app/shared/codecs.ml` (see `hot-reload` above). Write `name` and `event_name` as a `function` over
   every constructor, with no `_` case, so that a new constructor is a compile
   error, and add each new state name to `states`.
3. Run `silica check --json --traces traces > report.json`.
4. For each item of `entries[].violations` in the report, read `rule`,
   `hint`, `state.name`, `view` (the view as text at the failing step) and
   `trace` (each step has `event` and `cause.text`). Find the rule in the
   table of the verification reference.
5. Make a red test for each violation you fix. Each violation has a
   `trace_file` such as `traces/counter_ignored_event_saving.jsonl` and a
   `test_file` such as `test/counter_ignored_event_saving_replay.ml`; run
   `silica script ID TRACE_FILE --output TEST_FILE` (it writes the file only
   when the test can be written, and says which file it wrote) and add its
   name, such as `counter_ignored_event_saving_replay`, to the names in
   `test/dune` (then `dune fmt`, which wraps the long line). The test is already formatted when `ocamlformat`
   is installed; do not edit its last line, the assertion. Run `dune test`
   and see the test fail with the rule and hint. A test that compares a whole
   view, such as `test/counter_page.ml`, fails here too when the change
   changed that view, and prints the lines that differ: if the difference is
   intended, update it in step 8. When `silica script` exits 1 (a step that
   cannot run in memory, such as a timer that real time fires later), it
   writes no file; use `silica replay ID TRACE_FILE`: it replays the steps
   that the checker chose, with no codec, and exits 1 while a rule fails at the last step, 3
   when the trace cannot be replayed faithfully (recorded with names only, or
   from another version of the check).
6. Fix the machine or the view, as the hint says. Keep the check as strict as
   it was: no removed property, lower bound or allowance to hide a finding.
7. Run `silica check` again: the violations are gone. A change of behaviour
   is also `golden-drift`: read the changed states, edges and view lines. If
   the change is intended, write the golden files again with
   `dune exec ./check/main.exe -- golden check/spec --write`: it prints the
   same findings, then `golden: wrote N files into check/spec: ...`, a line
   that says the golden files now match, and `passed (exit 0)`. If not, fix
   the code. Then `silica check` exits 0.
8. `dune test`: the red test passes; keep it. Update the tests that expect
   the old behaviour, such as `test/counter_page.ml`, which compares the
   whole view.
9. Before you finish: `dune build`, `dune test` and `dune build @fmt` all
   pass with no output. `report.json` and `traces/` belong to one run, and
   `.gitignore` leaves them out; the red tests keep the steps. A new run of
   `silica check --traces traces` removes the trace files of a check that it
   no longer writes.
