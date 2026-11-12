Make a new app.

  $ silica new counter-app
  created .gitignore
  created .ocamlformat
  created AGENTS.md
  created app/melange/dune
  created app/native/dune
  created app/shared/api.ml
  created app/shared/codecs.ml
  created app/shared/counter.mlx
  created check/checks.ml
  created check/dune
  created check/main.ml
  created check/spec/counter.dot
  created check/spec/counter.qnt
  created check/spec/counter.spec.json
  created check/spec/counter.spec.txt
  created check/spec/counter.views.txt
  created client/client_main.ml
  created client/dev_codecs.ml
  created client/dune
  created client/package.json
  created dune-project
  created server/dune
  created server/main.ml
  created test/counter_page.ml
  created test/counter_steps.ml
  created test/dune
  created test/expect.ml
  
  Next:
    cd counter-app
    npm --prefix client install
    silica dev

The directory name is the project name.

  $ grep counter-app counter-app/dune-project counter-app/client/package.json
  counter-app/dune-project:(name counter-app)
  counter-app/dune-project: (name counter-app)
  counter-app/client/package.json:  "name": "counter-app-client",

A second run changes nothing.

  $ silica new counter-app
  counter-app already holds the template; nothing changed.
  
  Next:
    cd counter-app
    npm --prefix client install
    silica dev

A changed file stops the command, and it writes nothing.

  $ echo "(* mine *)" >> counter-app/server/main.ml
  $ rm counter-app/AGENTS.md
  $ silica new counter-app
  silica new: these files differ from the template:
    server/main.ml
  Nothing was written. Move these files away, or choose another directory.
  [1]
  $ test -e counter-app/AGENTS.md
  [1]
  $ silica new counter-app > /dev/null 2>&1
  [1]

Restore the file; the next run writes the missing one.

  $ silica new restored > /dev/null
  $ cp restored/server/main.ml counter-app/server/main.ml
  $ silica new counter-app | head -1
  created AGENTS.md

A name that is not a valid project name stops the command.

  $ silica new 1app
  silica new: "1app" is not a valid project name. Use letters, digits, '_' and '-', and start with a letter.
  [1]

The app builds its server and its client bundle, and its tests pass.

  $ ln -s "$DUNE_SOURCEROOT/example/client/node_modules" counter-app/client/node_modules
  $ cd counter-app
  $ dune build server/main.exe client/dist 2>&1
  $ ls _build/default/client/dist | sed 's/-[A-Z0-9]*\./-HASH./'
  client_main-HASH.js
  client_main-HASH.js.map
  $ dune test 2>&1
  $ silica check 2>&1
  counter: 16 states, 45 transitions
  passed (exit 0)
  $ silica check --json | grep -E '"(exit_code|outcome)"'
    "exit_code": 0,
    "outcome": "passed",

A check that does not build is a setup error. With --json the report still
comes on standard output, with the compile error in errors.

  $ cp check/checks.ml checks.ml.saved
  $ echo 'let broken =' >> check/checks.ml
  $ silica check --json 2> /dev/null | grep -o -E '"(exit_code|outcome)":[^,]*|Syntax error'
  "exit_code":3
  "outcome":"setup error"
  Syntax error
  $ mv checks.ml.saved check/checks.ml

silica check also compares the golden files: a missing one is a finding, with
the command that writes it. --no-golden leaves them out.

  $ mv check/spec/counter.dot counter.dot.saved
  $ silica check 2>&1 | grep -E '^(golden-drift|  hint|findings)'
  golden-drift: The golden file check/spec/counter.dot differs from what the check found.
    hint: Read the changes. When they are intended, write the files again with dune exec ./check/main.exe -- golden check/spec --write, and commit them.
  findings (exit 1)
  $ silica check --no-golden 2>&1 | tail -1
  passed (exit 0)
  $ mv counter.dot.saved check/spec/counter.dot

With a directory, the command also writes the graphs of each machine for
review: its spec, a drawing, the text form and a Mermaid block. Standard
output keeps the report alone.

  $ silica check --graphs graphs
  counter: 16 states, 45 transitions
  passed (exit 0)
  silica check: wrote the graphs of 1 machine into graphs
  $ ls graphs
  counter.md
  counter.spec.json
  counter.svg
  counter.txt
  $ cmp graphs/counter.txt check/spec/counter.spec.txt
  $ cmp graphs/counter.spec.json check/spec/counter.spec.json
  $ grep -o 'class="chart-node[^"]*" data-state="[a-z]*"' graphs/counter.svg
  class="chart-node is-initial" data-state="counting"
  class="chart-node" data-state="saved"
  class="chart-node" data-state="saving"
  $ head -5 graphs/counter.md
  # counter
  
  ```mermaid
  stateDiagram-v2
    direction LR
  $ silica check --json --graphs graphs 2> /dev/null | grep -E '"(exit_code|outcome)"'
    "exit_code": 0,
    "outcome": "passed",
  $ ls graphs | wc -l
  4

A change of behaviour fails the tests: the golden spec names the changed edge.

  $ sed 's/edge counting -> saving on save/edge counting -> saved on save/' check/spec/counter.spec.txt > changed.txt
  $ mv changed.txt check/spec/counter.spec.txt
  $ dune test 2>&1 | grep -E '^ *(golden-drift|removed|added)'
  golden-drift: The golden file spec/counter.spec.txt differs from what the check found.
    removed edge counting -> saved on save
    added edge counting -> saving on save
  $ dune exec ./check/main.exe -- golden check/spec --write > /dev/null
  $ dune test 2>&1
  $ dune build @fmt 2>&1

A seeded bug: the Save button stays enabled while the save runs, and the
update ignores it there. The checker writes the violation as a trace, and the
script of that trace is a test that fails for the same reason.

  $ sed 's/<button on_press=Save disabled=(disabled Save)>/<button on_press=Save>/' app/shared/counter.mlx > changed.mlx
  $ cp app/shared/counter.mlx counter.mlx.orig
  $ mv changed.mlx app/shared/counter.mlx
  $ silica check --traces traces > /dev/null 2>&1
  [1]
  $ ls traces
  counter_ignored_event_saved.jsonl
  counter_ignored_event_saving.jsonl
  $ silica replay counter traces/counter_ignored_event_saving.jsonl 2>&1 | tail -1
  the last step breaks a rule (exit 1)
  $ silica script counter traces/counter_ignored_event_saving.jsonl --output test/counter_ignored_event_saving_replay.ml
  script: wrote test/counter_ignored_event_saving_replay.ml, the test of the violation ignored-event of counter, from traces/counter_ignored_event_saving.jsonl. Add counter_ignored_event_saving_replay to the names of the tests in test/dune.
  $ sed 's/(names counter_steps counter_page)/(names counter_steps counter_page counter_ignored_event_saving_replay)/' test/dune > test/dune.new
  $ mv test/dune.new test/dune
  $ cat test/counter_ignored_event_saving_replay.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "ignored-event" ~state:"saving"
      ~message:
        "In state saving, the view offers press on button \"Save\" at main > \
         button[3] gives the event save, and the update returns Ignore."
      ~hint:
        "Handle save in state saving, or do not offer it there: disable the \
         element with Machine.can, or leave its timer or call out of during of \
         saving."
  
  let () =
    let page = Memory.mount Counter.component in
    Memory.press (Memory.by_role page Accessibility.Button ~name:"Save");
    Check_expect.handles broken ~name:"Save" page Accessibility.Button
      Memory.press
  $ dune build @fmt 2>&1
  $ dune test 2>&1 | grep -E '^(ignored-event|hint)'
  ignored-event: In state saving, the view offers press on button "Save" at main > button[3] gives the event save, and the update returns Ignore.
  hint: Handle save in state saving, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of during of saving.

The fix disables the button again; the test passes and stays as a regression
test.

  $ mv counter.mlx.orig app/shared/counter.mlx
  $ dune test 2>&1
  $ silica check 2>&1 | tail -1
  passed (exit 0)

The traces conform to the specs and to the code: the steps are the same. A
trace with a step that no spec has does not conform:

  $ silica conform traces/counter_ignored_event_saved.jsonl traces/counter_ignored_event_saving.jsonl 2>&1 | tail -1
  every trace conforms (exit 0)
  $ sed 's/"from":"counting","to":"saving"/"from":"counting","to":"saved"/' traces/counter_ignored_event_saving.jsonl > traces/changed.jsonl
  $ silica conform traces/changed.jsonl 2>&1 | grep -E '^(deviation|  step|  difference)|exit'
  deviation 1 (unknown-step): transition 2, machine counter, instance counter
    step: counting -> saved on save
    difference: no spec of the machine has this step. The specs allow from counting: stay on decrement, stay on increment, -> saving on save.
  a trace does not conform (exit 1)

The page test compares the whole text view. When a view changes, its failure
shows the lines that differ.

  $ sed 's/^      "Saved"$/      "All saved"/' app/shared/counter.mlx > changed.mlx
  $ cp app/shared/counter.mlx counter.mlx.orig
  $ mv changed.mlx app/shared/counter.mlx
  $ dune test 2>&1 | grep -A 12 'view: the text differs'
  view: the text differs (- expected, + got):
    main
      heading "Counter" level=1
      status
        text "2"
      button "Decrement" [press]
      button "Increment" [press]
      button "Save" disabled [press]
  -   text "Saved"
  +   text "All saved"
  $ mv counter.mlx.orig app/shared/counter.mlx

The reports and traces that the commands write stay out of git.

  $ grep -x -e traces -e report.json .gitignore
  traces
  report.json
