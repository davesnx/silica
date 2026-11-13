Five traces of mini Linear, from visits in Silica_memory: open an issue, type
a comment, send it; one visit creates an issue first. The suite of mini Linear
compares each trace with its golden specs (by names) and with its code (by
data).

  $ ./fixtures.exe traces
  $ conform () {
  >   ../linear_check/main.exe conform --spec ../linear_check/spec "$@" > out
  >   code=$?
  >   grep -v "not checked here" out
  >   return $code
  > }

The visit as it was recorded, with data, conforms:

  $ conform traces/conforms.jsonl
  conform traces/conforms.jsonl: root router, build dev, 39 transitions
    by names: transitions checked against the golden specs: 20
    by data: steps run again in the code: 9; snapshots checked by the properties: 9
  every trace conforms (exit 0)

The same visit with names only, as a recorder keeps it by default, with one
step that no spec has: the composer sends from empty.

  $ conform traces/missing-edge.jsonl
  conform traces/missing-edge.jsonl: root router, build dev, 39 transitions
    by names: transitions checked against the golden specs: 20
    by data: steps run again in the code: 1; snapshots checked by the properties: 0
  deviation 1 (unknown-step): transition 38, machine comment-composer, instance router/1.0:issue-detail/1.6:comments/1.1:comment-composer
    step: empty -> sending on send
    difference: no spec of the machine has this step. The specs allow from empty: -> drafting on typed, stay on clear, stay on props, stay on typed.
    hint: If the trace is from this build: run the golden check of the specs (dune test, or main.exe golden DIR). When it reports this step as added, the spec is out of date: review the golden diff and write it with --write. When it does not, the app has behaviour the check does not explore: add samples, inputs or props that reach it. If the trace is from another build (this one is dev), check it with the specs of that build.
  a trace does not conform (exit 1)
  [1]

A joint check compares a parent with its children against its joint spec:

  $ conform --only comments+comment+comment-composer traces/missing-edge.jsonl
  conform traces/missing-edge.jsonl: root router, build dev, 39 transitions
    by names: transitions checked against the golden specs: 7
    by data: steps run again in the code: 0; snapshots checked by the properties: 0
  deviation 1 (unknown-step): transition 38, machine comment-composer, instance router/1.0:issue-detail/1.6:comments/1.1:comment-composer
    step: empty -> sending on send
    difference: no spec of the machine has this step. The specs allow from empty: -> drafting on typed, stay on clear.
    hint: If the trace is from this build: run the golden check of the specs (dune test, or main.exe golden DIR). When it reports this step as added, the spec is out of date: review the golden diff and write it with --write. When it does not, the app has behaviour the check does not explore: add samples, inputs or props that reach it. If the trace is from another build (this one is dev), check it with the specs of that build.
  a trace does not conform (exit 1)
  [1]

A trace from an older build that did not trim a comment: the names conform,
the data does not.

  $ conform traces/old-build.jsonl
  conform traces/old-build.jsonl: root router, build 2026-10-01, 39 transitions
    by names: transitions checked against the golden specs: 20
    by data: steps run again in the code: 9; snapshots checked by the properties: 9
  deviation 1 (different-step): transition 38, machine comment-composer, instance router/1.0:issue-detail/1.6:comments/1.1:comment-composer
    step: drafting -> sending on send
    difference: the trace has drafting -> sending (entered), snapshot ["Sending","  Purple elephant memo  "]; the code gives drafting -> sending (entered), snapshot ["Sending","Purple elephant memo"]
    hint: The trace is from another build (2026-10-01), or the update changed since it was recorded: check the trace with the code and specs of that build. If it is this build, the update depends on something outside the snapshot and the event: make it a function of them.
  a trace does not conform (exit 1)
  [1]

A composer resumed at a snapshot that breaks a property: it sends a blank
comment.

  $ conform traces/breaks-property.jsonl
  conform traces/breaks-property.jsonl: root router, build dev, 2 transitions
    by names: transitions checked against the golden specs: 2
    by data: steps run again in the code: 1; snapshots checked by the properties: 2
  deviation 1 (property-fails): transition 1, machine comment-composer, instance router/1.0:issue-detail/1.6:comments/1.1:comment-composer
    step: resume in sending
    difference: The property "only a comment with text is ever sent" does not hold in state sending.
    hint: The app reached a snapshot that breaks "only a comment with text is ever sent". When silica check does not report it too, the check does not reach this snapshot: add samples, inputs or props that reach it. Then fix it. Change the update or the view so that "only a comment with text is ever sent" holds in state sending, or correct the property.
    view:
      textbox "Comment" value="   " disabled [input, key_down]
      button "Sending..." disabled [press]
  a trace does not conform (exit 1)
  [1]

A visit that also creates an issue: it types the title in the create
dialog, a step of the dialog that the joint checks of the create page and its
dialog explore with the texts of the dialog. It conforms:

  $ conform traces/creates.jsonl
  conform traces/creates.jsonl: root router, build dev, 52 transitions
    by names: transitions checked against the golden specs: 33
    by data: steps run again in the code: 9; snapshots checked by the properties: 9
  every trace conforms (exit 0)

replay takes counterexamples of the checker, and recorded runs whose steps
all have data. This recorded run has a step with no data, so replay refuses
it with exit 3 and points to conform:

  $ ../linear_check/main.exe replay router traces/conforms.jsonl
  replay: transition 17 (url changed) has no event data and no choice of the checker, so it cannot be replayed faithfully: the trace was recorded with names only, or the machine has no event codec (see Silica.Machine.register_codecs). To compare a recorded run with the specs and the code, run conform
  [3]

The JSON form, for a program:

  $ ../linear_check/main.exe conform --spec ../linear_check/spec --json traces/old-build.jsonl | grep -E '"(exit_code|outcome|kind|seq|step)"'
    "exit_code": 1,
    "outcome": "deviations",
            "kind": "different-step",
            "seq": 38,
            "step": "drafting -> sending on send",

A trace that does not read is a setup error:

  $ echo '{"root":' > traces/cut.jsonl
  $ ../linear_check/main.exe conform --spec ../linear_check/spec traces/cut.jsonl
  conform: traces/cut.jsonl: line 1: Line 1, bytes 7-8:
  Unexpected end of input
  [3]
  $ ../linear_check/main.exe conform --spec ../linear_check/spec
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
