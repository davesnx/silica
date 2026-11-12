The exit code alone says the outcome: 0 passed.

  $ FIXTURE=pass ./fixtures.exe
  door: 3 states, 4 transitions
  passed (exit 0)
  $ echo $?
  0

1: a finding. The JSON of a seeded bug: the Save button stays in the saved
state, and the update ignores it there.

  $ FIXTURE=bug ./fixtures.exe --json
  {
    "format": "silica-check",
    "version": 1,
    "command": "check",
    "exit_code": 1,
    "outcome": "findings",
    "passed": false,
    "entries": [
      {
        "id": "save",
        "machine": "save",
        "machines": [
          {
            "machine": "save",
            "states": 3
          }
        ],
        "passed": false,
        "complete": true,
        "truncated": false,
        "states": 3,
        "transitions": 5,
        "violations": [
          {
            "rule": "ignored-event",
            "message": "In state saved, the view offers press on button \"Save\" at div > button gives the event save, and the update returns Ignore.",
            "hint": "Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of saved.",
            "state": {
              "name": "saved",
              "data": [
                "Saved"
              ]
            },
            "element": "div > button",
            "view": "text \"Saved\"\nbutton \"Save\" [press]\n",
            "kind": "path",
            "trace": [
              {
                "index": 1,
                "instance": "0",
                "machine": "save",
                "from": "editing",
                "event": "save",
                "data": [
                  "Save"
                ],
                "cause": {
                  "type": "view",
                  "handler": "press",
                  "element": "form > button[2]",
                  "role": "button",
                  "name": "Save",
                  "text": "the view offers press on button \"Save\" at form > button[2]"
                },
                "target": "saved"
              },
              {
                "index": 2,
                "instance": "0",
                "machine": "save",
                "from": "saved",
                "event": "save",
                "data": [
                  "Save"
                ],
                "cause": {
                  "type": "view",
                  "handler": "press",
                  "element": "div > button",
                  "role": "button",
                  "name": "Save",
                  "text": "the view offers press on button \"Save\" at div > button"
                },
                "target": "saved"
              }
            ],
            "loop": null,
            "trace_file": null,
            "test_file": null
          }
        ],
        "unreached": [],
        "state_limit": null,
        "allowed": [],
        "not_explored": []
      }
    ],
    "unchecked": [],
    "elsewhere": [],
    "golden": null,
    "errors": []
  }
  [1]

2: incomplete, the search stopped at max_states.

  $ FIXTURE=limit ./fixtures.exe
  counter: 5 states, 5 transitions
  state-limit: The search stopped at 5 states, so the check is incomplete.
    hint: Bound the search with ~within, or raise ~max_states.
  incomplete (exit 2)
  [2]

3: a setup error.

  $ FIXTURE=broken ./fixtures.exe --json
  {
    "format": "silica-check",
    "version": 1,
    "command": "check",
    "exit_code": 3,
    "outcome": "setup error",
    "passed": false,
    "entries": [],
    "unchecked": [],
    "elsewhere": [],
    "golden": null,
    "errors": [
      {
        "id": null,
        "message": "Check.app needs at least one route in ~routes"
      }
    ]
  }
  [3]

A finding beats an incomplete check, and a setup error beats a finding.

  $ FIXTURE=limit-and-bug ./fixtures.exe > /dev/null
  [1]
  $ FIXTURE=broken-and-bug ./fixtures.exe > /dev/null
  [3]

A component that a checked view renders needs an entry.

  $ FIXTURE=unchecked ./fixtures.exe
  shell: 1 states, 1 transitions
  not explored: component door
  unchecked-component: A checked view renders the component door, and no entry of the suite checks it.
    hint: Add Check_suite.entry for door, or Check_suite.elsewhere with the reason when another suite checks it.
  findings (exit 1)
  [1]
  $ FIXTURE=unchecked-covered ./fixtures.exe
  shell: 1 states, 1 transitions
  not explored: component door
  door: 3 states, 4 transitions
  passed (exit 0)

--only runs some entries, does not judge coverage, and an unknown id is a
setup error.

  $ FIXTURE=unchecked ./fixtures.exe --only shell
  shell: 1 states, 1 transitions
  not explored: component door
  passed (exit 0)
  $ FIXTURE=limit-and-bug ./fixtures.exe --only counter > /dev/null
  [2]
An entry can make more entries from its report. --only runs the parent to
make them, and reports the selected one.

  $ FIXTURE=dependents ./fixtures.exe
  shell: 1 states, 1 transitions
  not explored: component door
  door: 3 states, 4 transitions
  passed (exit 0)
  $ FIXTURE=dependents ./fixtures.exe --only door
  door: 3 states, 4 transitions
  passed (exit 0)
  $ FIXTURE=bug ./fixtures.exe --only nothing
  error: no entry checks the machine nothing
  setup error (exit 3)
  [3]

Bad arguments print the usage and exit 3.

  $ FIXTURE=pass ./fixtures.exe --verbose
  check: unknown argument --verbose
  Usage:
    main.exe [check] [--json] [--only ID]... [--traces DIR]
    main.exe golden DIR [--write] [--json]
    main.exe replay ID TRACE_FILE [--json]
    main.exe script ID TRACE_FILE [--component EXPR]
    main.exe rules [--json]
    main.exe conform [--spec DIR] [--only ID]... [--json] TRACE_FILE...
  Exit codes: 0 passed, 1 findings, 2 incomplete, 3 setup error.
  [3]

The rules of the suite are in the rule table.

  $ FIXTURE=pass ./fixtures.exe rules | grep -A 2 -E '^(unchecked-component|golden-drift|state-limit)'
  state-limit (search)
    The search stopped at {states} states, so the check is incomplete.
    hint: Bound the search with ~within, or raise ~max_states.
  unchecked-component (suite)
    A checked view renders the component {machine}, and no entry of the suite checks it.
    hint: Add Check_suite.entry for {machine}, or Check_suite.elsewhere with the reason when another suite checks it.
  golden-drift (suite)
    The golden file {file} differs from what the check found.
    hint: Read the changes. When they are intended, write the files again with {command}, and commit them.

Golden files: missing files are a finding until they are written.

  $ FIXTURE=pass ./fixtures.exe golden spec
  door: 3 states, 4 transitions
  golden-drift: The golden file spec/door.spec.txt differs from what the check found.
    the file is not there yet
  golden-drift: The golden file spec/door.qnt differs from what the check found.
    the file is not there yet
  golden-drift: The golden file spec/door.dot differs from what the check found.
    the file is not there yet
  golden-drift: The golden file spec/door.views.txt differs from what the check found.
    the file is not there yet
    hint: Read the changes. When they are intended, write the files again with ./fixtures.exe golden spec --write, and commit them.
  findings (exit 1)
  [1]
  $ FIXTURE=pass ./fixtures.exe golden spec --write > /dev/null
  $ ls spec
  door.dot
  door.qnt
  door.spec.txt
  door.views.txt
  $ cat spec/door.spec.txt spec/door.views.txt
  machine door
  initial closed
  state closed
  state locked
  state opened
  edge closed -> locked on lock
  edge closed -> opened on open
  edge locked -> closed on unlock
  edge opened -> closed on close
  state closed
  path
    (start)
  view
    button "Open" [press]
    button "Lock" [press]
  
  state opened
  path
    closed -> opened on open, by the view offers press on button "Open" at div > button[1]
  view
    button "Close" [press]
  
  state locked
  path
    closed -> locked on lock, by the view offers press on button "Lock" at div > button[2]
  view
    button "Unlock" [press]
  $ FIXTURE=pass ./fixtures.exe golden spec

One edge changes: unlock now goes to the opened state. The diff names that
edge, removed and added, and nothing else.

  $ FIXTURE=changed ./fixtures.exe golden spec
  door: 3 states, 4 transitions
  golden-drift: The golden file spec/door.spec.txt differs from what the check found.
    removed edge locked -> closed on unlock
    added edge locked -> opened on unlock
  golden-drift: The golden file spec/door.qnt differs from what the check found.
    follows the changes of door.spec.txt
  golden-drift: The golden file spec/door.dot differs from what the check found.
    follows the changes of door.spec.txt
    hint: Read the changes. When they are intended, write the files again with ./fixtures.exe golden spec --write, and commit them.
  findings (exit 1)
  [1]
  $ FIXTURE=changed ./fixtures.exe golden spec --json | grep -A 4 '"lines"'
          "lines": [
            "removed edge locked -> closed on unlock",
            "added edge locked -> opened on unlock"
          ]
        },
  --
          "lines": [
            "follows the changes of door.spec.txt"
          ]
        },
        {
  --
          "lines": [
            "follows the changes of door.spec.txt"
          ]
        }
      ]
  $ FIXTURE=changed ./fixtures.exe golden spec --write | tail -3
  golden: wrote 3 files into spec: door.spec.txt (changed), door.qnt (changed), door.dot (changed)
  golden: the golden files now match what the check found, so the changes above are written, not findings; commit them with the change of code.
  passed (exit 0)
  $ FIXTURE=changed ./fixtures.exe golden spec

A file that no entry makes is stale.

  $ touch spec/gone.qnt
  $ FIXTURE=changed ./fixtures.exe golden spec
  door: 3 states, 4 transitions
  golden-drift: The golden file spec/gone.qnt differs from what the check found.
    no entry makes this file
    hint: Read the changes. When they are intended, write the files again with ./fixtures.exe golden spec --write, and commit them.
  findings (exit 1)
  [1]
  $ FIXTURE=changed ./fixtures.exe golden spec --write > /dev/null
  $ ls spec
  door.dot
  door.qnt
  door.spec.txt
  door.views.txt

With --traces, each violation is also a trace file, and the JSON names it.

  $ FIXTURE=bug ./fixtures.exe --json --traces traces | grep trace_file
            "trace_file": "traces/save_ignored_event_saved.jsonl",
  $ cat traces/save_ignored_event_saved.jsonl
  {"root":"save","build":"check","origin":{"type":"checker","rule":"ignored-event","choices":[1,0]},"dropped":0}
  {"seq":1,"instance":"save","number":1,"entry":0,"machine":"save","from":"","to":"editing","change":"started","event":"","snapshot":["Editing",0],"cause":{"type":"start"}}
  {"seq":2,"instance":"save","number":1,"entry":0,"machine":"save","from":"editing","to":"saved","change":"entered","event":"save","data":["Save"],"snapshot":["Saved"],"cause":{"type":"element","handler":"press","role":"button","name":"Save","element":"form > button[2]"}}
  {"seq":3,"instance":"save","number":1,"entry":0,"machine":"save","from":"saved","to":"saved","change":"unhandled","event":"save","data":["Save"],"snapshot":["Saved"],"cause":{"type":"element","handler":"press","role":"button","name":"Save","element":"div > button"}}

replay runs the trace in the check again: the last step breaks the rule, so
the exit code is 1.

  $ FIXTURE=bug ./fixtures.exe replay save traces/save_ignored_event_saved.jsonl
  replay save: traces/save_ignored_event_saved.jsonl
  start: editing
  1. editing -> saved: save, by the view offers press on button "Save" at form > button[2]
  2. saved -> saved: save, by the view offers press on button "Save" at div > button
    fails ignored-event: In state saved, the view offers press on button "Save" at div > button gives the event save, and the update returns Ignore.
      hint: Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of saved.
  state: saved
  view:
    text "Saved"
    button "Save" [press]
  the last step breaks a rule (exit 1)
  [1]
  $ FIXTURE=bug ./fixtures.exe replay save traces/save_ignored_event_saved.jsonl --json | grep -E '"(exit_code|not_enabled|rule)"'
    "exit_code": 1,
            "rule": "ignored-event",
    "not_enabled": null,

After a fix, the same trace no longer breaks a rule: exit 0.

  $ FIXTURE=fixed ./fixtures.exe replay save traces/save_ignored_event_saved.jsonl | tail -1
  no rule fails (exit 0)

A step that the check no longer offers is a mismatch: exit 1.

  $ FIXTURE=disabled ./fixtures.exe replay save traces/save_ignored_event_saved.jsonl | tail -4
      hint: Add a button, a timer or an input that leaves saved, or mark saved final with ~final when the machine is done there.
  step 2 of the trace is not enabled here. The state offers:
    nothing
  the trace does not replay (exit 1)

script prints a Silica_memory test of the counterexample.

  $ FIXTURE=bug ./fixtures.exe script save traces/save_ignored_event_saved.jsonl --component Fixtures.save | grep -c Memory.press
  script: the test of the violation ignored-event of save, from traces/save_ignored_event_saved.jsonl. Save it as test/save_ignored_event_saved_replay.ml, and add save_ignored_event_saved_replay to the names of the tests in test/dune.
  2

Two violations of one check get two trace files, named by the machine, the
rule and the state, and the text names the test file of each:

  $ FIXTURE=twice ./fixtures.exe --traces traces | grep trace
  trace of violation 1: traces/twice_ignored_event_first.jsonl; its test: silica script twice traces/twice_ignored_event_first.jsonl > test/twice_ignored_event_first_replay.ml
  trace of violation 2: traces/twice_ignored_event_second.jsonl; its test: silica script twice traces/twice_ignored_event_second.jsonl > test/twice_ignored_event_second_replay.ml
  $ FIXTURE=twice ./fixtures.exe --json --traces traces | grep -E '"(trace|test)_file"'
            "trace_file": "traces/twice_ignored_event_first.jsonl",
            "test_file": "test/twice_ignored_event_first_replay.ml"
            "trace_file": "traces/twice_ignored_event_second.jsonl",
            "test_file": "test/twice_ignored_event_second_replay.ml"
  $ FIXTURE=twice ./fixtures.exe script twice traces/twice_ignored_event_second.jsonl > /dev/null
  script: the test of the violation ignored-event of twice, from traces/twice_ignored_event_second.jsonl. Save it as test/twice_ignored_event_second_replay.ml, and add twice_ignored_event_second_replay to the names of the tests in test/dune.

conform takes a trace that the checker wrote, and says that it is a
counterexample, not a recorded run: replay says whether it still breaks the
rule.

  $ FIXTURE=bug ./fixtures.exe golden savespec --write > /dev/null
  [1]
  $ FIXTURE=bug ./fixtures.exe conform --spec savespec traces/save_ignored_event_saved.jsonl
  conform traces/save_ignored_event_saved.jsonl: root save, build check, 3 transitions
    note: this trace is a counterexample of the checker for ignored-event, not a recorded run: conform compares its steps with the specs and the code, and says nothing about the rule; run replay to see whether it still breaks ignored-event
    by names: transitions checked against the golden specs: 3
    by data: steps run again in the code: 3; snapshots checked by the properties: 3
  every trace conforms (exit 0)

A new run removes the trace files of a check that ran and that it no longer
writes, so a stale trace cannot be replayed by mistake. It keeps the files of
other checks and files that are not traces:

  $ touch traces/twice_ignored_event_gone.jsonl traces/door_dead_end_closed.jsonl traces/notes.txt
  $ FIXTURE=twice ./fixtures.exe --traces traces > /dev/null
  [1]
  $ ls traces | grep -E '^(twice|door|notes)'
  door_dead_end_closed.jsonl
  notes.txt
  twice_ignored_event_first.jsonl
  twice_ignored_event_second.jsonl

A trace that cannot be replayed faithfully is a setup error, never a pass,
such as one recorded with names only (no event data and no choices of the
checker).

  $ sed -e 's/"origin":{[^}]*}/"origin":{"type":"runtime","platform":"browser"}/' -e 's/,"data":\[[^]]*\]//' traces/save_ignored_event_saved.jsonl > names.jsonl
  $ FIXTURE=bug ./fixtures.exe replay save names.jsonl
  replay: transition 2 (save) has no event data and no choice of the checker, so it cannot be replayed faithfully: the trace was recorded with names only, or the machine has no event codec (see Silica.Machine.register_codecs). To compare a recorded run with the specs and the code, run conform
  [3]

A trace that does not read, or an unknown machine, is a setup error.

  $ echo '{' > bad.jsonl
  $ FIXTURE=bug ./fixtures.exe replay save bad.jsonl
  replay: bad.jsonl: line 1: Line 1, bytes 0-1:
  Unexpected end of input
  [3]
  $ FIXTURE=bug ./fixtures.exe script door traces/save_ignored_event_saved.jsonl
  script: no entry checks the machine door
  [3]
