Five traces of the example app, from visits in Silica_memory: add a todo,
vote, open two items; one visit also adds a second todo and types a draft on
the edit page. The suite of the example app compares each trace with its
golden specs (by names) and with its code (by data).

  $ ./fixtures.exe traces
  $ conform () {
  >   ../example_check/main.exe conform --spec ../example_check/spec "$@" > out
  >   code=$?
  >   grep -v "not checked here" out
  >   return $code
  > }

The visit as it was recorded, with data, conforms:

  $ conform traces/conforms.jsonl
  conform traces/conforms.jsonl: root router, build dev, 20 transitions
    by names: transitions checked against the golden specs: 20
    by data: steps run again in the code: 9; snapshots checked by the properties: 8
  every trace conforms (exit 0)

The same visit with names only, as a recorder keeps it by default, with one
step that no spec has: the item loads from missing.

  $ conform traces/missing-edge.jsonl
  conform traces/missing-edge.jsonl: root router, build dev, 20 transitions
    by names: transitions checked against the golden specs: 20
    by data: steps run again in the code: 4; snapshots checked by the properties: 0
  deviation 1 (unknown-step): transition 17, machine item, instance router/:item
    step: missing -> loaded on "settled ok"
    difference: no spec of the machine has this step. The specs allow from missing: -> loading on props.
    hint: If the trace is from this build: run the golden check of the specs (dune test, or main.exe golden DIR). When it reports this step as added, the spec is out of date: review the golden diff and write it with --write. When it does not, the app has behaviour the check does not explore: add samples, inputs or props that reach it. If the trace is from another build (this one is dev), check it with the specs of that build.
  a trace does not conform (exit 1)
  [1]

A joint check compares a parent with its children against its joint spec:

  $ conform --only router+item traces/missing-edge.jsonl
  conform traces/missing-edge.jsonl: root router, build dev, 20 transitions
    by names: transitions checked against the golden specs: 7
    by data: steps run again in the code: 0; snapshots checked by the properties: 0
  deviation 1 (unknown-step): transition 17, machine item, instance router/:item
    step: missing -> loaded on "settled ok"
    difference: no spec of the machine has this step. The specs allow from missing: -> loading on props.
    hint: If the trace is from this build: run the golden check of the specs (dune test, or main.exe golden DIR). When it reports this step as added, the spec is out of date: review the golden diff and write it with --write. When it does not, the app has behaviour the check does not explore: add samples, inputs or props that reach it. If the trace is from another build (this one is dev), check it with the specs of that build.
  a trace does not conform (exit 1)
  [1]

A trace from an older build that did not trim a todo: the names conform, the
data does not.

  $ conform traces/old-build.jsonl
  conform traces/old-build.jsonl: root router, build 2026-10-01, 20 transitions
    by names: transitions checked against the golden specs: 20
    by data: steps run again in the code: 9; snapshots checked by the properties: 8
  deviation 1 (different-step): transition 8, machine todo, instance router/:home/2:todo
    step: stay todo on add
    difference: the trace has todo -> todo (kept), snapshot {"todos":[{"id":1,"text":"  Purple elephant memo  ","completed":false}],"draft":"","next_id":2}; the code gives todo -> todo (kept), snapshot {"todos":[{"id":1,"text":"Purple elephant memo","completed":false}],"draft":"","next_id":2}
    hint: The trace is from another build (2026-10-01), or the update changed since it was recorded: check the trace with the code and specs of that build. If it is this build, the update depends on something outside the snapshot and the event: make it a function of them.
  a trace does not conform (exit 1)
  [1]

A todo list resumed at a snapshot that breaks a property: two todos have the
same id. The next step deletes both.

  $ conform traces/breaks-property.jsonl
  conform traces/breaks-property.jsonl: root router, build dev, 2 transitions
    by names: transitions checked against the golden specs: 2
    by data: steps run again in the code: 1; snapshots checked by the properties: 2
  deviation 1 (property-fails): transition 1, machine todo, instance router/:home/2:todo
    step: resume in todo
    difference: The property "todo ids unique" does not hold in state todo.
    hint: The app reached a snapshot that breaks "todo ids unique". When silica check does not report it too, the check does not reach this snapshot: add samples, inputs or props that reach it. Then fix it. Change the update or the view so that "todo ids unique" holds in state todo, or correct the property.
    view:
      heading "Todos" level=2
      text "Press a todo to toggle it, double press to delete it."
      form [submit]
        textbox "New todo" value="" [input]
        button "Add"
      list
        listitem
          button "[ ] first" [press, double_press]
        listitem
          button "[ ] second" [press, double_press]
  a trace does not conform (exit 1)
  [1]

A visit that also adds a second todo and types a draft on the edit page, a
step of the page that the joint check of the router and the edit page explores
with its inputs. It
conforms:

  $ conform traces/edits.jsonl
  conform traces/edits.jsonl: root router, build dev, 27 transitions
    by names: transitions checked against the golden specs: 27
    by data: steps run again in the code: 12; snapshots checked by the properties: 10
  every trace conforms (exit 0)

replay takes counterexamples of the checker, and recorded runs whose steps
all have data. This recorded run has a step with no data, so replay refuses
it with exit 3 and points to conform:

  $ ../example_check/main.exe replay router traces/conforms.jsonl
  replay: transition 11 (url changed) has no event data and no choice of the checker, so it cannot be replayed faithfully: the trace was recorded with names only, or the machine has no event codec (see Silica.Machine.register_codecs). To compare a recorded run with the specs and the code, run conform
  [3]

The JSON form, for a program:

  $ ../example_check/main.exe conform --spec ../example_check/spec --json traces/old-build.jsonl | grep -E '"(exit_code|outcome|kind|seq|step)"'
    "exit_code": 1,
    "outcome": "deviations",
            "kind": "different-step",
            "seq": 8,
            "step": "stay todo on add",

A trace that does not read is a setup error:

  $ echo '{"root":' > traces/cut.jsonl
  $ ../example_check/main.exe conform --spec ../example_check/spec traces/cut.jsonl
  conform: traces/cut.jsonl: line 1: Line 1, bytes 7-8:
  Unexpected end of input
  [3]

So is a spec that does not read:

  $ mkdir -p unreadable/x.spec.txt
  $ ../example_check/main.exe conform --spec unreadable traces/conforms.jsonl
  conform: unreadable/x.spec.txt: Is a directory
  [3]
  $ ../example_check/main.exe conform --spec ../example_check/spec
  conform: give at least one trace file
  Usage:
    main.exe [check] [--json] [--only ID]... [--traces DIR] [--golden DIR] [--specs DIR]
    main.exe golden DIR [--write] [--json]
    main.exe replay ID TRACE_FILE [--json]
    main.exe script ID TRACE_FILE [--output FILE] [--component EXPR] [--check EXPR]
    main.exe rules [--json]
    main.exe conform [--spec DIR] [--only ID]... [--json] TRACE_FILE...
  Exit codes: 0 passed, 1 findings, 2 incomplete, 3 setup error.
  [3]
