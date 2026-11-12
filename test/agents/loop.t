The verification loop of docs/verification.mld, run end to end. The task: let
the user save again after a save. Start from a new app.

  $ silica new app > /dev/null
  $ cd app

Write the requirements as checks first. Save is never offered while a save
runs, and every save ends.

  $ cat > check/main.ml <<'OCAML'
  > open Silica
  > 
  > let save_offered view =
  >   List.exists
  >     (fun element ->
  >       Accessibility.role element = Some Accessibility.Button
  >       && Accessibility.name element = "Save"
  >       && not (Accessibility.disabled element)
  >     )
  >     (Accessibility.elements view)
  > 
  > let () =
  >   Check_suite.main
  >     [
  >       Check_suite.entry
  >         (Check.component
  >            ~within:(fun state -> Counter.count state <= 3)
  >            ~samples:[ Check.answers Api.save ~rejected:[ "too many" ] [ () ] ]
  >            ~properties:
  >              [
  >                Check.always ~name:"no-save-while-saving"
  >                  ~hint:
  >                    "Disable Save in {state}: a second save waits for the \
  >                     answer of the first."
  >                  Check.(not_ (state_named "saving" &&& view save_offered));
  >                Check.leads_to ~name:"save-ends"
  >                  ~from:(Check.state_named "saving")
  >                  ~to_:Check.(state_named "saved" ||| state_named "save_failed")
  >                  ();
  >              ]
  >            Counter.component
  >         );
  >     ]
  > OCAML
  $ dune build @fmt 2>&1
  $ silica check 2>&1
  counter: 16 states, 45 transitions
  passed (exit 0)

A first try with a bug: the view offers Save in every state, and the update
still ignores Save in saving and in saved.

  $ cp app/shared/counter.mlx counter.mlx.orig
  $ sed 's/<button on_press=Save disabled=(disabled Save)>/<button on_press=Save>/' counter.mlx.orig > app/shared/counter.mlx

Run the suite with JSON and trace files. Exit code 1: findings.

  $ silica check --json --traces traces > report.json 2> build.log
  [1]
  $ grep -E '"(exit_code|outcome)"' report.json
    "exit_code": 1,
    "outcome": "findings",

Each violation has its rule, its state, a hint and a trace file.

  $ grep -E '^          "(rule|hint|element|trace_file)"' report.json
            "rule": "no-save-while-saving",
            "hint": "Disable Save in saving: a second save waits for the answer of the first.",
            "element": null,
            "trace_file": "traces/counter-1.jsonl"
            "rule": "ignored-event",
            "hint": "Handle save in state saving, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of saving.",
            "element": "main > button[3]",
            "trace_file": "traces/counter-2.jsonl"
            "rule": "ignored-event",
            "hint": "Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of saved.",
            "element": "main > button[3]",
            "trace_file": "traces/counter-3.jsonl"
  $ grep -E '^ *"(name|event|text)"' report.json | head -12
              "name": "saving",
                "event": "save",
                  "name": "Save",
                  "text": "the view offers press on button \"Save\" at main > button[3]"
              "name": "saving",
                "event": "save",
                  "name": "Save",
                  "text": "the view offers press on button \"Save\" at main > button[3]"
                "event": "save",
                  "name": "Save",
                  "text": "the view offers press on button \"Save\" at main > button[3]"
              "name": "saved",

Replay the third violation in the checker: each step, and the rule that fails.

  $ silica replay counter traces/counter-3.jsonl
  replay counter: traces/counter-3.jsonl
  start: counting
  1. counting -> saving: save, by the view offers press on button "Save" at main > button[3]
    fails no-save-while-saving: The property "no-save-while-saving" does not hold in state saving.
      hint: Disable Save in saving: a second save waits for the answer of the first.
  2. saving -> saved: answered ok, by the answer of the call counter/saving//api/save 0 to /api/save
  3. saved -> saved: save, by the view offers press on button "Save" at main > button[3]
    fails ignored-event: In state saved, the view offers press on button "Save" at main > button[3] gives the event save, and the update returns Ignore.
      hint: Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of saved.
  state: saved
  view:
    main
      heading "Counter" level=1
      status
        text "0"
      button "Decrement" disabled [press]
      button "Increment" [press]
      button "Save" [press]
      text "Saved"
  the last step breaks a rule (exit 1)
  [1]

Make it a test that fails for the same reason: one test for each violation
that you fix, named after its trace file.

  $ silica script counter traces/counter-3.jsonl > test/counter_3_replay.ml
  $ cat test/counter_3_replay.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "ignored-event" ~state:"saved"
      ~message:"In state saved, the view offers press on button \"Save\" at main > \
                button[3] gives the event save, and the update returns Ignore."
      ~hint:"Handle save in state saved, or do not offer it there: disable the \
             element with Machine.can, or leave its timer or call out of owns \
             of saved."
  
  let () =
    let page = Memory.mount Counter.component in
    Memory.press (Memory.by_role page Accessibility.Button ~name:"Save");
    Memory.reply (Memory.pending_request page "/api/save") "null";
    Check_expect.handles broken ~name:"Save" page Accessibility.Button Memory.press
  $ sed 's/(names counter_steps counter_page)/(names counter_steps counter_page counter_3_replay)/' test/dune > test/dune.new
  $ mv test/dune.new test/dune
The generated test is not formatted yet. dune fmt prints a diff, writes the
file and exits 1; a second run prints nothing and exits 0.

  $ dune fmt 2> fmt.log
  [1]
  $ grep -E '^(File|Promoting)' fmt.log
  File "test/counter_3_replay.ml", line 1, characters 0-0:
  Promoting _build/default/test/counter_3_replay.ml.corrected to
  $ dune fmt

The red test fails with the rule and hint. The page test fails too: it
compares the whole view, and the first try changed the view of saved.

  $ dune test 2>&1 | grep -E '^(ignored-event|hint|view:)'
  view: expected "main\n  heading \"Counter\" level=1\n  status\n    text \"2\"\n  button \"Decrement\" [press]\n  button \"Increment\" [press]\n  button \"Save\" disabled [press]\n  text \"Saved\"\n", got "main\n  heading \"Counter\" level=1\n  status\n    text \"2\"\n  button \"Decrement\" [press]\n  button \"Increment\" [press]\n  button \"Save\" [press]\n  text \"Saved\"\n"
  ignored-event: In state saved, the view offers press on button "Save" at main > button[3] gives the event save, and the update returns Ignore.
  hint: Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of saved.

The fix: the update handles Save in saved, and the view disables Save where
the update does not handle it.

  $ sed 's/| (Counting count | Save_failed count), Save ->/| (Counting count | Saved count | Save_failed count), Save ->/' counter.mlx.orig > app/shared/counter.mlx
  $ silica check 2>&1
  counter: 16 states, 49 transitions
  passed (exit 0)
  $ silica check --json 2> /dev/null | grep '"exit_code"'
    "exit_code": 0,

The behaviour changed, so the golden files differ: the suite names the new
edge and the changed view. The page test still expects the old view.

  $ dune test 2>&1 | grep -E '^ *(golden-drift|added|removed|[+-] )|^view:'
  golden-drift: The golden file spec/counter.spec.txt differs from what the check found.
    added edge saved -> saving on save
  golden-drift: The golden file spec/counter.qnt differs from what the check found.
  golden-drift: The golden file spec/counter.dot differs from what the check found.
  golden-drift: The golden file spec/counter.views.txt differs from what the check found.
    +     button "Save" [press]
    -     button "Save" disabled [press]
  view: expected "main\n  heading \"Counter\" level=1\n  status\n    text \"2\"\n  button \"Decrement\" [press]\n  button \"Increment\" [press]\n  button \"Save\" disabled [press]\n  text \"Saved\"\n", got "main\n  heading \"Counter\" level=1\n  status\n    text \"2\"\n  button \"Decrement\" [press]\n  button \"Increment\" [press]\n  button \"Save\" [press]\n  text \"Saved\"\n"

The change is intended: write the golden files again and update the page
test. Now everything passes.

  $ dune exec ./check/main.exe -- golden check/spec --write
  counter: 16 states, 49 transitions
  golden-drift: The golden file check/spec/counter.spec.txt differs from what the check found.
    added edge saved -> saving on save
  golden-drift: The golden file check/spec/counter.qnt differs from what the check found.
    follows the changes of counter.spec.txt
  golden-drift: The golden file check/spec/counter.dot differs from what the check found.
    follows the changes of counter.spec.txt
  golden-drift: The golden file check/spec/counter.views.txt differs from what the check found.
    +     button "Save" [press]
    -     button "Save" disabled [press]
  golden: wrote the changes into check/spec
  passed (exit 0)
  $ sed 's/  button "Save" disabled \[press\]/  button "Save" [press]/' test/counter_page.ml > counter_page.ml.new
  $ mv counter_page.ml.new test/counter_page.ml
  $ dune test 2>&1
  $ dune build @fmt 2>&1
  $ dune exec ./check/main.exe -- golden check/spec
