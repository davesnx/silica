A script reproduces a counterexample of the checker in Silica_memory. The
Save button stays in the saved state, and the update ignores it there:

  $ cat save_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "ignored-event" ~state:"saved"
      ~message:"In state saved, the view offers press on button \"Save\" at div > \
                button gives the event save, and the update returns Ignore."
      ~hint:"Handle save in state saved, or do not offer it there: disable the \
             element with Machine.can, or leave its timer or call out of owns \
             of saved."
  
  let () =
    let page = Memory.mount Save.component in
    Memory.press (Memory.by_role page Accessibility.Button ~name:"Save");
    Check_expect.handles broken ~name:"Save" page Accessibility.Button Memory.press

The script fails while the bug is there, with the rule and its hint:

  $ ./save_script.exe
  ignored-event: In state saved, the view offers press on button "Save" at div > button gives the event save, and the update returns Ignore.
  hint: Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of saved.
  The update still ignores the input.
  [1]

It passes on the fixed component, which handles the second press:

  $ ./save_fixed_script.exe

It also passes when the fix disables the button in the saved state: a press
cannot reach a disabled button.

  $ ./save_disabled_script.exe

A rule of the view: the script checks the rule on the view of Silica_memory.

  $ ./nameless_script.exe
  accessible-name: In state editing, the button at button has no accessible name, so a screen reader cannot say what it does.
  hint: Give the button at button visible text, an aria-label, or an aria-labelledby that points to an element with text.
  The view still breaks it:
  In state editing, the button at button has no accessible name, so a screen reader cannot say what it does.
  [1]
  $ ./nameless_fixed_script.exe

An exception in the update reaches on_error:

  $ ./crash_script.exe
  raised: The update raised Division_by_zero in state ready.
  hint: Fix the exception in the update; the trace reproduces it. Keep an expected failure in the state, not in an exception.
  The step raised Division_by_zero.
  [1]

An answer of a call, then a press that the update ignores:

  $ cat upload_retry_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "ignored-event" ~state:"failed"
      ~message:"In state failed, the view offers press on button \"Retry\" at div \
                > button gives the event retry, and the update returns Ignore."
      ~hint:"Handle retry in state failed, or do not offer it there: disable \
             the element with Machine.can, or leave its timer or call out of \
             owns of failed."
  
  let () =
    let page = Memory.mount Upload.component in
    Memory.press (Memory.by_role page Accessibility.Button ~name:"Save");
    Memory.reply ~status:422 (Memory.pending_request page "/api/save") {|"too many"|};
    Check_expect.handles broken ~name:"Retry" page Accessibility.Button Memory.press
  $ ./upload_retry_script.exe
  ignored-event: In state failed, the view offers press on button "Retry" at div > button gives the event retry, and the update returns Ignore.
  hint: Handle retry in state failed, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of failed.
  The update still ignores the input.
  [1]

A property: the view must not be the view that broke it.

  $ ./upload_error_script.exe
  short-error: The property "short-error" does not hold in state failed.
  hint: Keep the error under five letters.
  It still fails in state failed: The property "short-error" does not hold in state failed.
  [1]

A timer that fires first in real time: the script moves the clock to it.

  $ ./clock_script.exe
  ignored-event: In state ready, the view offers press on button "Again" at div > button gives the event again, and the update returns Ignore.
  hint: Handle again in state ready, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of ready.
  The update still ignores the input.
  [1]

A component with props mounts with the props of the start, decoded by its
props codec:

  $ grep -A 13 'let props' labelled_script.ml
  [1]
  $ ./labelled_script.exe
  accessible-name: In state shown, the button at button has no accessible name, so a screen reader cannot say what it does.
  hint: Give the button at button visible text, an aria-label, or an aria-labelledby that points to an element with text.
  The view still breaks it:
  In state shown, the button at button has no accessible name, so a screen reader cannot say what it does.
  [1]

A leads_to property with a Waits counterexample: the sync fails, and the app
waits for the user with no retry. The script fails while the page waits, with
no request and no timer, in the view that broke the property.

  $ cat sync_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "sync leads to synced" ~state:"offline"
      ~message:"The property \"sync leads to synced\" does not hold: after its \
                from condition held, the run reaches state offline, where no timer \
                and no call runs, so the app waits for the user, who may do \
                nothing; its to_ condition did not hold on the way."
      ~hint:"Make state offline own a timer or a call that leads to the to_ \
             condition (for example a retry with a limit, or a failure state \
             that the to_ condition accepts), or correct the property."
  
  let () =
    let page = Memory.mount Sync.component in
    Check_expect.holds broken Sync.check page (fun () ->
        Memory.press (Memory.by_role page Accessibility.Button ~name:"Sync");
        Memory.reply ~status:500 (Memory.pending_request page "/api/sync") ""
    )
  $ ./sync_script.exe
  sync leads to synced: The property "sync leads to synced" does not hold: after its from condition held, the run reaches state offline, where no timer and no call runs, so the app waits for the user, who may do nothing; its to_ condition did not hold on the way.
  hint: Make state offline own a timer or a call that leads to the to_ condition (for example a retry with a limit, or a failure state that the to_ condition accepts), or correct the property.
  It still fails in state offline: The property "sync leads to synced" does not hold: after its from condition held, the run reaches state offline, where no timer and no call runs, so the app waits for the user, who may do nothing; its to_ condition did not hold on the way.
  [1]

A Loop counterexample: the answer is lost and the call starts again forever.
The script runs the stem and the cycle once, and fails while the page is back
at the view where the cycle starts.

  $ cat lost_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "save leads to saved or failed" ~state:"saving"
      ~message:"The property \"save leads to saved or failed\" does not hold: \
                after its from condition held, the run can repeat saving -> saving \
                forever; each timer and call that runs all along is delivered \
                (call lost/saving//api/save null), so this run is fair, and its \
                to_ condition never holds."
      ~hint:"Make a step of the cycle leave it toward the to_ condition: count \
             the repeats and stop after a limit, or handle the answer or timer \
             that repeats (call lost/saving//api/save null) by going on instead \
             of starting it again, or correct the property."
  
  let () =
    let page = Memory.mount Lost.component in
    Check_expect.holds broken ~cycle_from:1 Lost.check page (fun () ->
        Memory.press (Memory.by_role page Accessibility.Button ~name:"Save");
        Memory.reply (Memory.pending_request page "/api/save") "1"
    )
  $ ./lost_script.exe
  save leads to saved or failed: The property "save leads to saved or failed" does not hold: after its from condition held, the run can repeat saving -> saving forever; each timer and call that runs all along is delivered (call lost/saving//api/save null), so this run is fair, and its to_ condition never holds.
  hint: Make a step of the cycle leave it toward the to_ condition: count the repeats and stop after a limit, or handle the answer or timer that repeats (call lost/saving//api/save null) by going on instead of starting it again, or correct the property.
  It still fails in state saving: The property "save leads to saved or failed" does not hold: after its from condition held, the run can repeat saving -> saving forever; each timer and call that runs all along is delivered (call lost/saving//api/save null), so this run is fair, and its to_ condition never holds.
  [1]

A routed app: a link press, then the back button of the browser.

  $ cat pages_back_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "home stays behind" ~state:"home"
      ~message:"The property \"home stays behind\" does not hold: state home \
                matches its never condition, and this step or an earlier one of \
                the run matched its after condition."
      ~hint:"Change the steps so that no run reaches state home with what the \
             never condition forbids once the after condition held, or correct \
             the property."
  
  let () =
    let page = Memory.mount_app ~url:"/" Pages.app in
    Check_expect.holds broken Pages.check page (fun () ->
        Memory.press (Memory.by_role page Accessibility.Link ~name:"About");
        Memory.back page
    )
  $ ./pages_back_script.exe
  home stays behind: The property "home stays behind" does not hold: state home matches its never condition, and this step or an earlier one of the run matched its after condition.
  hint: Change the steps so that no run reaches state home with what the never condition forbids once the after condition held, or correct the property.
  It still fails in state home: The property "home stays behind" does not hold: state home matches its never condition, and this step or an earlier one of the run matched its after condition.
  [1]
  $ ./pages_link_script.exe
  img-alt: In state about, the image at main > img has no alt text.
  hint: Give the image at main > img an alt that says what it shows, or alt="" when it is decoration.
  The view still breaks it:
  In state about, the image at main > img has no alt text.
  [1]

A key handler: focus the field, then the key.

  $ cat search_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "ignored-event" ~state:"done"
      ~message:"In state done, the view offers key_down Enter on textbox \"Query\" \
                at div > input gives the event run, and the update returns Ignore."
      ~hint:"Handle run in state done, or do not offer it there: disable the \
             element with Machine.can, or leave its timer or call out of owns \
             of done."
  
  let () =
    let page = Memory.mount Search.component in
    Memory.focus (Memory.by_role page Accessibility.Textbox ~name:"Query");
    Memory.key page (Key.make "Enter");
    Check_expect.handles broken ~name:"Query" page Accessibility.Textbox
      (fun element ->
        Memory.focus element;
        Memory.key page (Key.make "Enter")
      )
  $ ./search_script.exe
  ignored-event: In state done, the view offers key_down Enter on textbox "Query" at div > input gives the event run, and the update returns Ignore.
  hint: Handle run in state done, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of owns of done.
  The update still ignores the input.
  [1]

Typing: the checker types each text of ~texts into each field, and the script
types it into the field by role and name.

  $ cat typing_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "the step is positive" ~state:"counting"
      ~message:"The property \"the step is positive\" does not hold in state \
                counting."
      ~hint:"Change the update or the view so that \"the step is positive\" \
             holds in state counting, or correct the property."
  
  let () =
    let page = Memory.mount Stepper.component in
    Check_expect.holds broken Stepper.typing page (fun () ->
        Memory.input (Memory.by_role page Accessibility.Spinbutton ~name:"Step size") "0"
    )
  $ ./typing_script.exe
  the step is positive: The property "the step is positive" does not hold in state counting.
  hint: Change the update or the view so that "the step is positive" holds in state counting, or correct the property.
  It still fails in state counting: The property "the step is positive" does not hold in state counting.
  [1]

The last line evaluates the property itself, in the check, along the run of
the page. A fix that only changes the view does not pass it:

  $ ./upload_restyled_script.exe
  short-error: The property "short-error" does not hold in state failed.
  hint: Keep the error under five letters.
  It still fails in state failed: The property "short-error" does not hold in state failed.
  [1]

A fix that keeps the error short passes it:

  $ ./upload_fixed_script.exe

A loop: the New badge pauses while the removal is confirmed, so Remove and the
timeout of the confirmation can repeat forever and the badge never gets old.
The script goes once around the cycle and asks the check whether the run can
repeat it:

  $ cat badge_script.ml
  open Silica
  module Memory = Silica_memory
  
  let broken =
    Check_expect.rule "a new badge gets old" ~state:"badge"
      ~message:"The property \"a new badge gets old\" does not hold: after its \
                from condition held, the run can repeat badge -> badge -> badge \
                forever; each timer and call that runs all along is delivered \
                (nothing), so this run is fair, and its to_ condition never holds."
      ~hint:"Make a step of the cycle leave it toward the to_ condition: count \
             the repeats and stop after a limit, or handle the answer or timer \
             that repeats (nothing) by going on instead of starting it again, \
             or correct the property."
  
  let () =
    let page = Memory.mount Badge.component in
    Check_expect.holds broken ~cycle_from:0 Badge.check page (fun () ->
        Memory.press (Memory.by_role page Accessibility.Button ~name:"Remove");
        Memory.press (Memory.by_role page Accessibility.Button ~name:"Cancel")
    )
  $ ./badge_script.exe
  a new badge gets old: The property "a new badge gets old" does not hold: after its from condition held, the run can repeat badge -> badge -> badge forever; each timer and call that runs all along is delivered (nothing), so this run is fair, and its to_ condition never holds.
  hint: Make a step of the cycle leave it toward the to_ condition: count the repeats and stop after a limit, or handle the answer or timer that repeats (nothing) by going on instead of starting it again, or correct the property.
  It still fails in state badge: The property "a new badge gets old" does not hold: after its from condition held, the run can repeat badge -> badge -> badge forever; each timer and call that runs all along is delivered (nothing), so this run is fair, and its to_ condition never holds.
  [1]

After the fix the badge timer runs during the confirmation too: the same view
comes back, but the cycle no longer repeats, so the test passes:

  $ ./badge_fixed_script.exe

Props come from the check, by the position of the start that the trace
records, so no props codec is needed: int props and record props.

  $ grep -A 3 'let page' numbered_script.ml
    let page =
      Memory.mount_with_props
        ~props:(Check.nth_props Rows.numbered_check 1)
        Rows.numbered
  $ ./numbered_script.exe
  accessible-name: In state shown, the button at button has no accessible name, so a screen reader cannot say what it does.
  hint: Give the button at button visible text, an aria-label, or an aria-labelledby that points to an element with text.
  The view still breaks it:
  In state shown, the button at button has no accessible name, so a screen reader cannot say what it does.
  [1]
  $ ./titled_script.exe
  accessible-name: In state shown, the button at div > button has no accessible name, so a screen reader cannot say what it does.
  hint: Give the button at div > button visible text, an aria-label, or an aria-labelledby that points to an element with text.
  The view still breaks it:
  In state shown, the button at div > button has no accessible name, so a screen reader cannot say what it does.
  [1]

A timer that real time fires second has no script; the command says why.

  $ ./gen_script.exe clock accessible-name
  step 2 fires the timer clock/waiting/after 2000ms (2000 ms) before the timer clock/waiting/after 300ms, which is due 1700 ms earlier in real time. The checker lets running timers fire in any order; Silica_memory keeps real time, so it cannot replay this order
  [1]
