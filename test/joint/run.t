A script reproduces a counterexample of a joint check in Silica_memory. The
step is a press inside the view of a row, a child of the board:

  $ cat bumped_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "a bumped row finishes" ~state:"board"
      ~message:"The property \"a bumped row finishes\" does not hold: after its \
                from condition held, the run reaches state board, where no timer \
                and no call runs, so the app waits for the user, who may do \
                nothing; its to_ condition did not hold on the way."
      ~hint:"Make state board own a timer or a call that leads to the to_ \
             condition (for example a retry with a limit, or a failure state \
             that the to_ condition accepts), or correct the property."
  
  let broken_view = {|main
    text "label 0 last 1 finished"
    button "add" [press]
    button "remove-first" [press]
    button "relabel" [press]
    button "toggle-job" [press]
    list
      listitem
        text "row 1 label 0 seen 0 clicks 1"
        button "bump 1" [press]
    text "idle"
    button "start job" [press]
  |}
  
  let () =
    let page = Memory.mount Board.component in
    Memory.press (Memory.by_role page Accessibility.Button ~name:"bump 1");
    Check_expect.does_not_wait broken page broken_view

The script fails while the bug is there:

  $ ./bumped_script.exe
  a bumped row finishes: The property "a bumped row finishes" does not hold: after its from condition held, the run reaches state board, where no timer and no call runs, so the app waits for the user, who may do nothing; its to_ condition did not hold on the way.
  hint: Make state board own a timer or a call that leads to the to_ condition (for example a retry with a limit, or a failure state that the to_ condition accepts), or correct the property.
  The page still waits for the user, with no request and no timer, in the view:
  main
    text "label 0 last 1 finished"
    button "add" [press]
    button "remove-first" [press]
    button "relabel" [press]
    button "toggle-job" [press]
    list
      listitem
        text "row 1 label 0 seen 0 clicks 1"
        button "bump 1" [press]
    text "idle"
    button "start job" [press]
  
  [1]
