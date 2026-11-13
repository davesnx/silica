---
name: silica-verify
description: Prove a change to a Silica app right with no browser, with the model checker, trace replay and golden files. Use when you add or change a Silica component, machine or view; when `silica check` or `dune test` reports a finding or golden drift; or when asked to verify, check or test a Silica app.
---

# Verify a Silica change

Work from the root of the app (the directory with `check/checks.ml`). The loop
is red, then green: a check fails on the bug, then passes on the fix.

1. Write the requirement as a check before the change: a property in
   `~properties` of the check in `check/checks.ml`, and the `~samples`,
   `~texts` (text typed into each field) and `~inputs` (other events) that
   reach the new states. A new component gets its own `Check_suite.entry`. A
   rule about steps is a `Check.always` with `Check.event` or
   `Check.transition`; it is read on every step (see "A rule about steps" in
   the verification reference). Done when `silica check` runs your property.
2. Change the machine and the view. Behaviour goes in the machine; the view
   disables each element whose event the state does not handle.
3. Run `silica check --json --traces traces > report.json` and branch on the
   exit code: 0 go to step 7; 1 findings (violations, or golden files that
   are missing or changed); 2 bound the search (`~within`, and
   `~max_history` for an app); 3 read stderr (a compile error or a bad
   argument).
4. For each item of `entries[].violations`, read `rule`, `hint`,
   `state.name`, `view` and each step of `trace` (`event`, `cause.text`).
   Look up the rule in the table in
   [references/verification.md](references/verification.md) (section "The
   rules"). Also read `unreached`, `vacuous`, `unchecked`, `golden` and
   `errors`. `unlisted` names each machine that gives no `~states` to
   `Machine.make`: `unreached` cannot cover it, so give it `~states`. An
   `event` is the `~event_name` of the machine, or, when it has none, the
   label of the cause, such as `press button "Save"` or `after 2000ms`.
5. Make it red, one test for each violation you fix: each violation has a
   `trace_file` (`traces/<id>_<rule>_<state>.jsonl`) and a `test_file`
   (`null`, with the reason in `test_error`, when no script test can run it);
   `silica script ID TRACE_FILE --output TEST_FILE` (no file when it exits
   1), add the name of the test file to the names in `test/dune`,
   `dune test`. The test is formatted already
   (when `ocamlformat` is installed), and it fails with the same rule and
   hint; do not edit its last line. A test that compares a whole view fails
   too when the change changed that view, and prints the lines that differ;
   update it in step 7 if the difference is intended. If `silica script`
   exits 1, run `silica replay ID TRACE_FILE` and read the step that fails:
   it replays the steps the checker chose, with no codec, and exits 3 when a
   trace cannot be replayed faithfully, never 0.
6. Fix the code as the hint says, until the violations are gone and the red
   test passes. The property, the bounds and the allowances stay as they
   were; an allowance is right only where the table says so.
7. `silica check` reports a change of behaviour as `golden-drift`: read each
   changed state, edge and view line. Intended:
   `dune exec ./check/main.exe -- golden check/spec --write` (it prints the
   findings, then the files it wrote and `passed (exit 0)`), and update the
   tests that expect the old behaviour (a test that compares a whole view
   fails on any change of it). Not intended: fix the code. Then
   `silica check` exits 0.
8. Done when `dune build`, `dune test` and `dune build @fmt` pass with no
   output. `report.json` and `traces/` are left out by `.gitignore`; do not
   commit them.

References, read when a step needs them:

- [references/verification.md](references/verification.md): the loop with
  real output, every command and exit code, the fields of the report, the
  table of rules (meaning, fix, when to allow), how to write a property, and
  `Silica_memory` tests.
- [references/attributes.md](references/attributes.md): every attribute in
  mlx whose value is not a string (a `bool`, an `int`, or variants such as
  ``type_=`number``), generated from the ppx.
- [references/api/](references/api/): every public signature with its
  documentation, one file for each library, generated from the interfaces:
  [silica](references/api/silica.md), [silica.check](references/api/check.md)
  (`Check`, `Check_suite`, `Check_expect`),
  [silica.memory](references/api/memory.md),
  [silica.server](references/api/server.md),
  [silica.json](references/api/json.md) (the JSON values that
  `Codec.make` reads and writes) and [silica-ui](references/api/ui.md)
  (`Silica_ui.Dialog`, `Silica_ui.Tabs` and the other components). Look a name up here, such as
  `Silica_memory.forward` or `Check.app`, instead of guessing it.
- [references/checker.md](references/checker.md): the checker in full:
  each rule, properties and temporal properties, samples, children, routed
  apps, the full JSON shape, replay and script.
