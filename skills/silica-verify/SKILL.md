---
name: silica-verify
description: Prove a change to a Silica app right with no browser, with the model checker, trace replay and golden files. Use when you add or change a Silica component, machine or view; when `silica check` or `dune test` reports a finding or golden drift; or when asked to verify, check or test a Silica app.
---

# Verify a Silica change

Work from the root of the app (the directory with `check/main.ml`). The loop
is red, then green: a check fails on the bug, then passes on the fix.

1. Write the requirement as a check before the change: a property in
   `~properties` of the entry in `check/main.ml`, and the `~samples` and
   `~inputs` that reach the new states. A new component gets its own
   `Check_suite.entry`. A rule about steps needs `Check.never_after`, not
   `Check.always` (see "Write a requirement as a property" in the
   verification reference). Done when `silica check` runs your property.
2. Change the machine and the view. Behaviour goes in the machine; the view
   disables each element whose event the state does not handle.
3. Run `silica check --json --traces traces > report.json` and branch on the
   exit code: 0 go to step 7; 1 findings; 2 bound the search (`~within`); 3
   read stderr (a compile error or a bad argument).
4. For each item of `entries[].violations`, read `rule`, `hint`,
   `state.name`, `view` and each step of `trace` (`event`, `cause.text`).
   Look up the rule in the table in
   [references/verification.md](references/verification.md) (section "The
   rules"). Also read `unreached`, `unchecked` and `errors`.
5. Make it red, one test for each violation you fix: for the `trace_file`
   `traces/ID-N.jsonl`, `silica script ID traces/ID-N.jsonl >
   test/ID_N_replay.ml`, add `ID_N_replay` to the names in `test/dune`,
   `dune fmt` twice (the first run prints a diff and exits 1), `dune test`.
   It fails with the same rule and hint; keep what its last line asserts. A
   test that compares a whole view fails too when the change changed that
   view; update it in step 7 if the difference is intended. If `silica script` exits 1, run
   `silica replay ID FILE` and read the step that fails.
6. Fix the code as the hint says, until `silica check` exits 0 and the red
   test passes. The property, the bounds and the allowances stay as they were;
   an allowance is right only where the table says so.
7. Run `dune test`. On `golden-drift`, read each changed state, edge and view
   line. Intended: `dune exec ./check/main.exe -- golden check/spec --write`
   (it prints the findings, then `golden: wrote the changes into check/spec`)
   and update the tests that expect the old behaviour (a test that compares a
   whole view fails on any change of it). Not intended: fix the code.
8. Done when `dune build`, `dune test` and `dune build @fmt` pass with no
   output. Delete `report.json` and `traces/`; do not commit them.

References, read when a step needs them:

- [references/verification.md](references/verification.md): the loop with
  real output, every command and exit code, the fields of the report, the
  table of rules (meaning, fix, when to allow), how to write a property, and
  `Silica_memory` tests.
- [references/attributes.md](references/attributes.md): every attribute in
  mlx whose value is not a string (a `bool`, an `int`, or variants such as
  ``type_=`number``), generated from the ppx.
- [references/checker.md](references/checker.md): the checker in full:
  each rule, properties and temporal properties, samples, children, routed
  apps, the full JSON shape, replay and script.
