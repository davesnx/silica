# Verify a change with no browser

This page is for a coding agent, or a person, who changes a Silica app and must show that the change is right. You need no browser and no screenshots. The checker runs every event in every state that each component can reach, and gives each finding as data: a rule, a hint, the steps that lead to it, the state and the view as text. Each section below is one step of the loop, with the command to run and the output to look for.

The examples come from `test/agents/loop.t`, which runs this loop on a new app in every `dune test`. The task in that test: let the user save again after a save.

## Before you start

Where to read these docs: in a checkout of Silica they are `docs/*.mld` (this page is `docs/verification.mld`), and the same pages as Markdown are in `skills/silica-verify/references/`. `dune build site/server/llms-full.txt` writes every page as one Markdown file in `_build/default/site/server/`. The Silica site serves them at `/llms.txt`, `/llms-full.txt` and `/docs/<page>.md`.

Run every command from the root of the app: the directory with `dune-project` and `check/checks.ml`. `silica new DIR` makes such an app (see [A new app](../../../docs/install.mld)). The parts that the loop uses:

- `check/checks.ml`: the checks, in a library: one value for each component, such as `Checks.counter`. Tests use them too: a test that `Check.script` writes evaluates its rule with the check.
- `check/main.ml`: the suite. One `Check_suite.entry` for each check of `Checks`.
- `check/spec/`: the golden files of each machine. `<id>.spec.txt` lists its states and edges, `<id>.views.txt` the view of each state as text.
- `test/`: tests that drive the component with `Silica_memory`.

The commands:

| Command | What it does | Exit codes |
|---|---|---|
| `silica check` | Builds `check/main.exe`, runs every entry and compares the golden files in `check/spec`: a missing or changed golden file is a finding. Add `--json` for a report that a program reads, `--traces DIR` for a trace file of each violation, `--no-golden` to leave the golden files out. | 0 passed, 1 findings, 2 incomplete, 3 setup error |
| `silica replay ID FILE` | Runs a trace file again in the check of `ID` and prints each step with the rules that fail there. Add `--json` for JSON. | 0 no rule fails at the last step, 1 a rule fails or a step is no longer offered, 3 the trace does not read |
| `silica script ID FILE --output TEST_FILE` | Writes a test for `Silica_memory` that does the same steps and fails while the bug is there; without `--output` it prints the test. | 0 written, 1 a step cannot run in memory (the reason is on stderr, and no file is written) |
| `dune exec ./check/main.exe -- --only ID` | Runs only the entry `ID`. `silica check` has no `--only`. | as `silica check` |
| `dune exec ./check/main.exe -- golden check/spec` | Compares the golden files with what the checks find, as `silica check` and `dune test` do. Add `--write` to accept a change. | 0 no change, 1 drift |
| `dune exec ./check/main.exe -- rules` | Lists every built-in rule with its message and hint. Add `--json` for JSON. | 0 |
| `silica dev --json` | Builds on each save and prints one JSON line for each build, error, restart, reload and hot reload, with the instances kept and restarted (see [Development: silica dev](../../../docs/install.mld)); stdout has the JSON lines only, and the output of the server goes to stderr. | runs until stopped |

## The loop

1. **Write the requirement as a check.** Before you change the component, add what must hold to its check in `check/checks.ml`: a property (see [Write a requirement as a property](verification.md#write-a-requirement-as-a-property)), and the `samples`, `texts` (text that the checker types into each field of the view) and `inputs` (other events that no element gives) that reach the new states. A new component gets its own `Check_suite.entry`; the suite reports a rendered component with no entry as `unchecked-component`.

2. **Change the machine and the view.** See [Write the machine](verification.md#write-the-machine).

3. **Run the suite.** `silica check --json --traces traces > report.json`. Read the exit code first. 0: go to step 7, but on the first run of a new property, prove that it can fail first: break the machine once in a way the requirement forbids, see the suite exit 1, make the red tests of step 5 from those traces, then undo the break. 1: read the findings. 2: the search stopped at its state limit; bound it (see `state-limit` in [the rules](verification.md#the-rules)). 3: a setup error; read stderr, which has the compiler errors when `check/main.exe` does not build.

4. **Read each finding.** In `report.json`, for each entry, each item of `violations`: `rule`, `message`, `hint`, `state.name`, `view` and `trace`. Each step of `trace` has `event` and `cause.text`, which says what made the step. Find the rule in [the table of rules](verification.md#the-rules). Also read `unreached`, `vacuous` and the top-level `unchecked` and `errors`.

5. **Make a red test.** Make one test for each violation that you fix. Each violation has a `trace_file`, such as `traces/counter_ignored_event_saving.jsonl` (the machine, the rule and the state), and a `test_file`, such as `test/counter_ignored_event_saving_replay.ml`: `null` when `silica script` cannot write the test, with the reason in `test_error`. Run `silica script ID TRACE_FILE --output TEST_FILE` (it writes the file only when the test can be written, and says which file it wrote), add its name to the `names` in `test/dune`, then run `dune test`. The test must fail with the same rule and hint. When `silica script` exits 1, it prints why on standard error and writes no file; use `silica replay ID TRACE_FILE` instead.

   - The printed test is formatted with the `.ocamlformat` of the app when `ocamlformat` is installed, so `dune build @fmt` passes on it. Do not edit its last line, the assertion.
   - A test that compares a whole view, such as the `Expect.string "view"` in `test/counter_page.ml`, fails in this `dune test` too when your change changed that view; it prints the lines that differ, `-` expected and `+` got. When the difference is the change you want, update the expected text in step 7; when it is not, it is one more bug to fix.
   - A step that types into a field is a step of the script: see [Type into a field](verification.md#type-into-a-field).
   - The last line evaluates the rule itself: for a property or most built-in rules it is `Check_expect.holds broken Checks.NAME page (fun () -> steps)`, which replays the run of the page in the check and fails while the rule fails on the states and views the steps reached. So the test passes exactly when the fix makes the rule hold on that run.

6. **Fix the machine or the view.** Follow the hint. Do not remove the property, lower a bound or add an allowance to make a finding go away; an allowance is right only for the cases in the table.

7. **Run the suite again.** `silica check`: the violations are gone. It compares the golden files too, so a new machine with no golden files yet, or a change of behaviour, is a `golden-drift` finding. When it reports `golden-drift`, read the changed states and edges. If the change is the one you wanted, run `dune exec ./check/main.exe -- golden check/spec --write`: it prints the same `golden-drift` findings, then the files it wrote, a line that says the golden files now match, and `passed (exit 0)`. It lists only the files that changed. If not, fix the code. `<id>.views.txt` holds each distinct view of a state, up to 16 for each state name, so a view with an error message is in it, and the drift names each added, removed and changed view. Then `silica check` exits 0. Run `dune test`: the red test passes; keep it. Update each test that expects the old behaviour, such as a test that compares a whole view.

8. **Finish.** `dune build`, `dune test` and `dune build @fmt` all pass with no output. `report.json` and `traces/` are the output of one run, and the `.gitignore` of `silica new` leaves them out; the red tests you kept hold the steps. A new `silica check --traces traces` removes the trace files of a check that it no longer writes.

## Write the machine

- Put behaviour in the machine. A view only reads the state and names events.
- Disable each element whose event the state does not handle: `disabled=(not (Machine.can machine state Event))`. The checker offers no event of a disabled element, and `Silica_memory` refuses to press it.
- `Go snapshot` to a state with the same name changes only the data: the timers and calls of the state keep running. A resource starts again only when its key changes, such as the input of a call. The state name is part of that key: a step to a state with another name stops every resource of the old state and starts the resources of the new one, even when both own the same `Host.after 3000 Expire`. To keep a timer running across a change, such as a row that asks for confirmation while it shows "New", keep the change as data inside one state, or give the phases sub-state names with a dot, such as `list.live` and `list.offline`: the part before the dot owns the resources, so they keep running, and the spec and the golden views still show each phase (see [Connection phases](../../../docs/data.mld)). `Reenter snapshot` leaves and enters the state, so all its timers and calls start again. A debounce uses `Reenter`, so that each key starts the timer again:

From `docs/examples/shared/doc_debounce.mlx`:

```ocaml
let typed : string -> (state, event) Machine.next = function
  | "" ->
      Go Empty
  | text ->
      Reenter (Typing text)
```

See [Machines and states](../../../docs/machines.mld) for the update and `during`.

## The loop on an example

The task: let the user save again after a save. First the requirements. Save is never offered while a save runs, and every save ends:

From `test/agents/loop.t`:

```sh
  >     ~properties:
  >       [
  >         Check.always ~name:"no-save-while-saving"
  >           ~hint:
  >             "Disable Save in {state}: a second save waits for the answer of \
  >              the first."
  >           Check.(not_ (state_named "saving" &&& view save_offered));
  >         Check.leads_to ~name:"save-ends"
  >           ~from:(Check.state_named "saving")
  >           ~to_:Check.(state_named "saved" ||| state_named "save_failed")
  >           ();
  >       ]
```

A first try enables the Save button in every state but does not handle `Save` in the update. The suite exits 1 and writes one trace file for each violation:

From `test/agents/loop.t`:

```sh
  $ silica check --json --traces traces > report.json 2> build.log
  [1]
  $ grep -E '"(exit_code|outcome)"' report.json
    "exit_code": 1,
    "outcome": "findings",
```

Three violations: the property in saving, and `ignored-event` in saving and in saved. Each has a hint that says what to change:

From `test/agents/loop.t`:

```sh
            "rule": "no-save-while-saving",
            "hint": "Disable Save in saving: a second save waits for the answer of the first.",
            "element": null,
            "trace_file": "traces/counter_no_save_while_saving_saving.jsonl",
            "rule": "ignored-event",
            "hint": "Handle save in state saving, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of during of saving.",
            "element": "main > button[3]",
            "trace_file": "traces/counter_ignored_event_saving.jsonl",
            "rule": "ignored-event",
            "hint": "Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of during of saved.",
            "element": "main > button[3]",
            "trace_file": "traces/counter_ignored_event_saved.jsonl",
```

`silica replay` shows each step of a trace and the rules that fail at each step:

From `test/agents/loop.t`:

```sh
  $ silica replay counter traces/counter_ignored_event_saved.jsonl
  replay counter: traces/counter_ignored_event_saved.jsonl
  start: counting
  1. counting -> saving: save, by the view offers press on button "Save" at main > button[3]
    fails no-save-while-saving: The property "no-save-while-saving" does not hold in state saving.
      hint: Disable Save in saving: a second save waits for the answer of the first.
  2. saving -> saved: answered ok, by the answer of the call counter/saving//api/save 0 to /api/save
  3. saved -> saved: save, by the view offers press on button "Save" at main > button[3]
    fails ignored-event: In state saved, the view offers press on button "Save" at main > button[3] gives the event save, and the update returns Ignore.
      hint: Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of during of saved.
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
```

`silica script` turns the same trace into a test. It finds each element by its role and name, answers the call, and ends with one assertion that fails while the update ignores Save in saved:

From `test/agents/loop.t`:

```sh
  let () =
    let page = Memory.mount Counter.component in
    Memory.press (Memory.by_role page Accessibility.Button ~name:"Save");
    Memory.reply (Memory.pending_request page "/api/save") "null";
    Check_expect.handles broken ~name:"Save" page Accessibility.Button
      Memory.press
```

From `test/agents/loop.t`:

```sh
  ignored-event: In state saved, the view offers press on button "Save" at main > button[3] gives the event save, and the update returns Ignore.
  hint: Handle save in state saved, or do not offer it there: disable the element with Machine.can, or leave its timer or call out of during of saved.
```

The fix handles `Save` in `Saved` in the update, and keeps `disabled=(disabled Save)` in the view, so Save is off while the save runs. The checks pass (`silica check --no-golden` exits 0). The behaviour changed on purpose, so the golden files differ, and `silica check` and `dune test` name the new edge and the changed lines of the view:

From `test/agents/loop.t`:

```sh
  golden-drift: The golden file spec/counter.spec.txt differs from what the check found.
    added edge saved -> saving on save
```

From `test/agents/loop.t`:

```sh
  golden-drift: The golden file check/spec/counter.views.txt differs from what the check found.
    changed the view of state saved
        +     button "Save" [press]
        -     button "Save" disabled [press]
```

The change is the intended one, so write the golden files again. The command prints the findings it accepts, then what it did:

From `test/agents/loop.t`:

```sh
  $ dune exec ./check/main.exe -- golden check/spec --write
  counter: 16 states, 49 transitions
  golden-drift: The golden file check/spec/counter.spec.txt differs from what the check found.
    added edge saved -> saving on save
```

From `test/agents/loop.t`:

```sh
  golden: wrote 5 files into check/spec: counter.spec.txt (changed), counter.spec.json (changed), counter.qnt (changed), counter.dot (changed), counter.views.txt (changed)
  golden: the golden files now match what the check found, so the changes above are written, not findings; commit them with the change of code.
  passed (exit 0)
```

Then update the page test that expected the old view, and run `dune test` again: it prints nothing.

## Read the report

The fields that the loop uses. The full shape is in [The JSON report](checker.md#the-json-report).

- `exit_code`: 0, 1, 2 or 3, as the exit code of the command.
- `entries`: one for each entry of the suite, with `id`, `passed` and `complete`.
- `entries[].violations`: `rule`, `message`, `hint`, `state.name`, `element` (for a rule of the view), `view` (the view as text at the last step), `kind` (`path`, `waits` or `loop`), `trace` (the steps), `loop` (the cycle, for `kind = "loop"`), and `trace_file` (with `--traces`).
- Each step of `trace`: `from`, `event`, `target`, and `cause` with its `type` and a `text` in words.
- `entries[].unreached`: states that no step reached, each with its hint.
- `entries[].vacuous`: properties that passed without being tested, because their `from`, their `after` or the left side of their implication never held. It is a warning: the exit code stays 0. The text output prints it as `vacuous: the property "NAME": ...`. Reach that state with `samples`, `texts` or `inputs`, or fix the condition.
- `entries[].allowed`: each allowance, its reason, and how many violations it turned off.
- `unchecked`: components that a checked view renders and that no entry checks.
- `golden`: the `golden-drift` findings, which are not in `entries[].violations`: `golden.changes`, each with `file`, `change` (`missing`, `changed` or `stale`), `rule`, `message`, `hint` and `lines` (the diff). A new machine has `missing` files, so the exit code is 1 with no violation. `null` with `--no-golden`.
- `errors`: setup errors, such as an `--only` id that no entry has.

With `jq`, one line for each violation:

```
jq -r '.entries[].violations[] | [.rule, .state.name, .trace_file, .hint] | @tsv' report.json
```

The text view lists each element that has a role, with its name, its state (such as `disabled`) and its handlers in brackets. A step is offered only by an element that is not disabled. A `label` around a field shows twice: once as the text of the label, and once as the name of the field, such as `text "Step size"` and then `spinbutton "Step size"`.

The counts of states and transitions, such as `counter: 16 states, 45 transitions`, change when the code changes, also with no finding. Read them as a size, not as a result.

## The rules

`dune exec ./check/main.exe -- rules` prints the message and hint of each rule. A property has the name you gave it as its rule. To allow a rule, add `Check.allow`` ~rule ?state ?element ~reason ()` to `allow` of the check; the report lists it with its reason. Use an allowance only in the cases of the last column.

| Rule | What it means | Usual fix | When an allowance is right |
|---|---|---|---|
| `ignored-event` | The view offers an element, or a timer or call gives an event, and the update returns `Ignore` for it in this state. | Handle the event in that state, or disable the element with `Machine.can`, or move the timer or call out of `during` of that state. | Never for an element of the view. |
| `dead-end` | A state that is not final where nothing can happen. | Add a button, timer or input that leaves the state, or mark the state final with `~final`. | Never; mark a real end state final. |
| `unreached-state` | No step reached a state of the machine. | Add `samples`, `inputs` or `props` that reach it, or widen `within`. | The state is reachable only from data the check cannot give, or is kept on purpose; give the reason. |
| `raised` | The update, the view, a handler or a property raised an exception. | Fix the code; keep an expected failure in the state, not in an exception. | Never. |
| `state-limit` | The search stopped at `max_states`; exit code 2. | Bound the state with `~within`, or raise `~max_states`. | Never; bound the search. |
| `unchecked-component` | A checked view renders a component that no entry checks. | Add a `Check_suite.entry` for it, or run it as a child with `Check.child`. | Use `Check_suite.elsewhere ~machine ~reason` when another suite checks it. |
| `golden-drift` | A golden file in `check/spec/` differs from what the checks found. | Read the changed states, edges and view lines. If intended, run `golden check/spec --write`; if not, fix the code. | Never; write the files again. |
| `focus-target` | A focus or scroll action names a ref that the next view does not render. | Render the element with that ref in the target state, or remove the action. | Never. |
| `unique-refs` | One ref is on two elements of a view. | Make one `Ref.make` for each element. | Never. |
| `boundary-reset` | The fallback of a boundary has no element with the reset attribute. | Put `~reset` on a button of the fallback. | Never. |
| `timer-owner` | A timer runs that the state does not own. | This is a Silica bug: report it with the trace. | Never. |
| `call-owner` | A call or subscription runs that the state does not own. | This is a Silica bug: report it with the trace. | Never. |
| `live-uses-client-resource` | A Live component runs a resource that only a browser has. | Move the resource into a client component. | Never. |
| `live-uses-client-action` | A Live component runs an action that needs an answer of the browser. | Run the action in a client component. | Never. |
| `route-round-trip` | A route prints as a URL that does not parse back to the same route. | Make the parse and the print of the route agree. | Never. |
| `link-parses` | A link goes to a URL that does not print back the same, or goes to the not-found route. | Make the link with `Router.link`. | A link that leaves the app on purpose. |
| `accessible-name` | A button, link, other control, or element with a press handler has no name. | Give it visible text, an `aria-label` or an `aria-labelledby`. | The name comes from CSS that the checker cannot see, such as an icon font, with a reason. |
| `field-label` | A field has no label. | A `<label for>`, a `<label>` around it, an `aria-label` or an `aria-labelledby`. | Never. |
| `img-alt` | An `img` has no `alt`. | Give `alt` text, or `alt=""` for decoration. | Never. |
| `unique-ids` | One id is on two elements. | Make the id unique, for example with the key or a prop. | Never. |
| `aria-reference` | An `aria-labelledby`, `aria-describedby`, `aria-activedescendant` or `for` names an id that is not in the view. | Render the element with that id, or remove the reference. | The target is in the parent view, which a check of the child alone cannot see. |
| `valid-role` | A `role` is not an ARIA role, or lacks the attribute it needs. | Use an ARIA role and its attribute, such as `aria-checked` for a checkbox. | Never. |
| `required-children` | A listbox, tablist or menu holds items of the wrong role. | Give each item the right role; move other content out. | Never. |
| `nested-interactive` | A control holds another control. | Move the inner control out. | Never. |
| `keyboard-access` | An element with a press handler cannot get keyboard focus. | Use a `<button>` or an `<a href>`, or give a role and `tabindex="0"` and handle Enter and Space. | A key handler on an element above handles the keyboard. |
| `dialog-name` | A dialog has no name. | An `aria-labelledby` to its title, or an `aria-label`. | Never. |
| `dialog-focus` | A modal dialog shows, and the state has no focus trap on it. | Put `Host.focus_trap` on the dialog in `during`, or use `Silica_ui.Dialog`. | A native `<dialog>` opened with `showModal`, which traps focus itself. |
| `landmark-main` | A view has more than one visible main landmark, or a main inside another. | Keep one `main` (or `role="main"`) for the page; make the others a section, a region or a div. A component check sees its own view; a joint check sees the whole page. | Never. |
| `heading-order` | A heading skips a level after the heading before it. | Use the next level, or add the missing levels. | Never. |
| `name-spacing` | Two inline parts of a name join with no space, such as `ENG-2Second`. | Put a `" "` text node between them, or an `aria-label`. | Never; CSS spacing does not count. |
| a property name | A property of the check does not hold. | Fix the code, or the property when it is wrong. Read `kind`: `waits` means the app waits for the user and the goal never comes; `loop` means a cycle that never reaches it. | Never; change the property instead. |

## Write a requirement as a property

A requirement such as "X is not offered in state S", "after X, eventually Y" or "after X, never Y" is a property in `properties` of the check. Write it before the change, so that the first wrong try fails. See [Properties](checker.md#properties) and [Temporal properties](checker.md#temporal-properties) for the full API.

- `Check.always`` ~name ?hint condition`: holds at every point of every run: in every state, and on every step when the condition reads the step (see [A rule about steps](verification.md#a-rule-about-steps)).
- `Check.leads_to`` ~name ~from ~to_ ()`: every run that meets `from` meets `to_` later.
- `Check.never_after`` ~name ~after ~never ()`: after `after`, `never` never holds.

A condition is a test of one point of a run: the state, its view, and the step that reached it. The conditions, for a machine with states `'s` and events `'e`:

| Condition | Type | Holds when |
|---|---|---|
| `Check.state`` test` | `('s -> bool) -> ('s, 'e) condition` | the state passes `test` |
| `Check.state_named`` name` | `string -> ('s, 'e) condition` | the state has this name, such as `"saving"` |
| `Check.view`` test` | `('e Silica.Node.t -> bool) -> ('s, 'e) condition` | the view of the state passes `test` |
| `Check.shows`` text` | `string -> ('s, 'e) condition` | `text` is a part of the text that a person sees in the view, in any element, with or without a role |
| `Check.holds`` test` | `('s -> 'e Silica.Node.t -> bool) -> ('s, 'e) condition` | the state and its view pass `test` together |
| `Check.event`` test` | `('e -> bool) -> ('s, 'e) condition` | the event of the step that reached the state passes `test` |
| `Check.action`` test` | `('e Silica.Action.host -> bool) -> ('s, 'e) condition` | the step that reached the state ran an action that passes `test` |
| `Check.entered`` name` | `string -> ('s, 'e) condition` | the step came from a state with another name into the state `name` |
| `Check.transition`` test` | `('s -> 'e -> 's -> bool) -> ('s, 'e) condition` | the step has an event, and `test before event after` holds with the states before and after it |
| `a &&& b`, `a ||| b`, `not_ a` | `('s, 'e) condition -> ...` | both hold, one holds, `a` does not hold |

One example of each, on the lamp of [Components and JSX](../../../docs/components.mld) (states `Off` and `On`, event `Toggle`):

```ocaml
# let examples : (Doc_lamp.state, Doc_lamp.event) Check.condition list =
    Check.
      [
        state (fun state -> state = Doc_lamp.On);
        state_named "on";
        view (fun view -> Silica.View_text.to_string view <> "");
        shows "The lamp is on.";
        holds (fun state view ->
            state = Doc_lamp.Off
            || Silica.View_text.to_string view <> "");
        event (fun event -> event = Doc_lamp.Toggle);
        action (function
          | Silica.Action.Run (Silica.Action.Focus _) -> true
          | _ -> false);
        entered "on";
        transition (fun before _ after -> before <> after);
        state_named "on" &&& event (fun event -> event = Doc_lamp.Toggle);
        state_named "on" ||| state_named "off";
        not_ (state_named "off");
      ];;
val examples : (Doc_lamp.state, Doc_lamp.event) Check.condition list =
  [<abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
   <abstr>; <abstr>; <abstr>; <abstr>]
# Check.passed
    (Check.run
       (Check.component
          ~properties:
            [
              Check.always ~name:"a view in every state"
                (Check.view (fun view -> Silica.View_text.to_string view <> ""));
              Check.never_after ~name:"after on, the lamp is on or off"
                ~after:(Check.state_named "on")
                ~never:(Check.not_ Check.(state_named "on" ||| state_named "off"))
                ();
            ]
          Doc_lamp.component));;
- : bool = true
```

### A rule about steps

`Check.event`, `Check.action`, `Check.entered` and `Check.transition` read the step into a point. In `Check.always`, a condition that reads the step is read on every start and on every step that the search explored, not only on the first step into each state, so a rule about steps cannot pass on a machine that breaks it. "A Reset step leaves the count at 0", from the test that breaks it with a Reset that keeps the count:

From `test/check/test_temporal.ml`:

```ocaml
        Check.always ~name:"a reset step leaves the count at 0"
          Check.(
            not_ (event (( = ) Reset)) ||| state (fun (Count count) -> count = 0)
          );
```

`Check.transition` reads the state before the step and the state after it. "Increment adds one":

From `test/check/test_temporal.ml`:

```ocaml
           Check.always ~name:"increment adds one"
             Check.(
               not_ (event (( = ) Increment))
               ||| transition (fun (Count before) _ (Count after) ->
                   after = before + 1
               )
             );
```

A start has no step, so `Check.event`, `Check.entered` and `Check.transition` are false at a start, and a property built only from them fails there; the message says so. The `not_ (event ...)` part makes the property hold at a start and on the other steps: write a rule about steps as `not_ (event test) ||| ...`. A property of a `Check.child` reads only states and views; one that reads the step raises `Check.Invalid_check`.

### A rule about an element

To ask about an element, read the view with `Silica.Accessibility`: the example finds an enabled button by its role and name.

From `test/agents/loop.t`:

```sh
  > let save_offered view =
  >   List.exists
  >     (fun element ->
  >       Accessibility.role element = Some Accessibility.Button
  >       && Accessibility.name element = "Save"
  >       && not (Accessibility.disabled element)
  >     )
  >     (Accessibility.elements view)
```

For text with no role, such as a message in a `<p>`, use `Check.shows`` "text"`. Inside `Check.holds`, where the text depends on the state, use `Check.view_shows`` view text`: `Check.holds (fun state view -> Check.view_shows view state.reason)`.

Give each property a `hint` that says what to change; `{state}` in it is the name of the failing state.

## Check a parent with its children

To check a parent with a child component, list the child in `children` of the entry: `Check.component ~children:[ Check.child Row.component ] ...`. The child runs inside the check of the parent; each instance gets the props of its node in the view of the parent, so `Check.child` takes no props. See [Children](checker.md#children) for the bounds.

- The entry id becomes the parent and the child joined by `"+"`, and so do the golden files: `counter.spec.txt` becomes `counter+saved-row.counter.spec.txt` and `counter+saved-row.saved-row.spec.txt`. `golden --write` writes the new names and removes the old files.
- The traces of the checker record which props of the check each start and each props step used, so replay and the script need no props codec: the script mounts with `Check.nth_props` and gives new props with `Check_expect.set_props`.
- At most `max_instances` instances of a child run at once (default 2). A further instance shows its start view and takes no step, and the report lists it in `not_explored` with how many were shown and how to raise `~max_instances`. The `~within` of the parent does not bound how many children run.
- Check the child alone too, with its own entry and `~props`, for the properties that read its steps.

## Write a test with Silica_memory

A test in `test/` mounts the component in `Silica_memory`, the same runtime with no browser, and acts as a user:

- `Memory.mount component` starts the page.
- `Memory.by_role page Accessibility.Button ~name:"Save"` finds an element as a screen reader names it. A failed query lists the elements of that role.
- `Memory.press`, `Memory.input element text`, `Memory.submit`: act. A press on a disabled or hidden element raises `Silica_memory.Not_reachable` with a message, as a user cannot do it. To test that a button is off: `match Memory.press button with () -> failwith "pressed" | exception Memory.Not_reachable _ -> ()`.
- `Memory.pending page`, or `Memory.pending_request page "/api/rename"` for the oldest call to one path, gives the calls that wait for an answer. `Memory.reply request body` answers with status 200 and the output as JSON, such as `"null"` for `()`. `Memory.reply ~status:422 request {|"Enter a name."|}` gives the answer `Rejected "Enter a name."`: the server answers an application error with 422. `Memory.network_error request "down"` gives `Failed (Network "down")`.
- `Memory.advance page ms`: move the clock, so timers fire.
- `Memory.view_text page`: the view as text, the same text as the report. A test that compares it whole fails on any change of the view. It shows the view of each child component in full, while the check of the parent alone shows a child as one line, `component counter`. A link shows as `link "History" href="/history"`, with no `[press]`: following it is a navigation, not an event of the machine.
- The role of each element is in [Roles](verification.md#roles): a number field is `Accessibility.Spinbutton`, an `<output>` is `Accessibility.Status`.
- `Memory.trace page`: the transitions so far.

See [The memory platform](../../../docs/testing.mld).

### Roles

The roles of `Silica.Accessibility.role`, and the elements that have each one with no `role` attribute. An explicit `role` attribute wins; a `role` of `` `none `` or `` `presentation `` gives no role. The text view and the queries of `Silica_memory` print the role in lower case, such as `spinbutton`. An element that has no role, such as ``<input type_=`password>`` or a `<div>`, is found by its accessible name with `Memory.by_name`, such as `Memory.by_name page "Password"` for a password field in a label; the text view prints its tag in place of a role.

| Role | Elements with this role |
|---|---|
| `Alert` | only ``role=`alert`` |
| `Alertdialog` | only ``role=`alertdialog`` |
| `Banner` | `<header>` with no `article`, `aside`, `main`, `nav` or `section` above it |
| `Button` | `<button>`; `<input>` of type `` `button ``, `` `submit ``, `` `reset `` or `` `image `` |
| `Cell` | `<td>` |
| `Checkbox` | ``<input type_=`checkbox>`` |
| `Columnheader` | `<th>` |
| `Combobox` | `<select>` (one line); a text or search `<input>` with `list` |
| `Complementary` | `<aside>` |
| `Contentinfo` | `<footer>` with no `article`, `aside`, `main`, `nav` or `section` above it |
| `Dialog` | `<dialog>` |
| `Form` | `<form>` |
| `Generic` | only ``role=`generic``; a `<div>` or `<span>` has no role |
| `Grid` | only ``role=`grid`` |
| `Gridcell` | only ``role=`gridcell`` |
| `Group` | `<optgroup>`, `<fieldset>`, `<details>` |
| `Heading` | `<h1>` to `<h6>` |
| `Img` | `<img>`, but no role with `alt=""` |
| `Link` | `<a>` and `<area>` with `href` |
| `List` | `<ul>`, `<ol>`, `<menu>` |
| `Listbox` | `<select>` with `multiple` or `size` above 1 |
| `Listitem` | `<li>` |
| `Main` | `<main>` |
| `Menu` | only ``role=`menu`` |
| `Menubar` | only ``role=`menubar`` |
| `Menuitem` | only ``role=`menuitem`` |
| `Menuitemcheckbox` | only ``role=`menuitemcheckbox`` |
| `Menuitemradio` | only ``role=`menuitemradio`` |
| `Navigation` | `<nav>` |
| `Option` | `<option>` |
| `Progressbar` | `<progress>` |
| `Radio` | ``<input type_=`radio>`` |
| `Radiogroup` | only ``role=`radiogroup`` |
| `Region` | `<section>` with a name |
| `Row` | `<tr>` |
| `Rowheader` | ``<th scope=`row>`` |
| `Search` | `<search>` |
| `Searchbox` | ``<input type_=`search>`` |
| `Separator` | `<hr>` |
| `Slider` | ``<input type_=`range>`` |
| `Spinbutton` | ``<input type_=`number>`` |
| `Status` | `<output>` |
| `Switch` | only ``role=`switch`` |
| `Tab` | only ``role=`tab`` |
| `Table` | `<table>` |
| `Tablist` | only ``role=`tablist`` |
| `Tabpanel` | only ``role=`tabpanel`` |
| `Textbox` | `<textarea>`; `<input>` with no type or of type `` `text ``, `` `email ``, `` `tel `` or `` `url `` |
| `Toolbar` | only ``role=`toolbar`` |
| `Tooltip` | only ``role=`tooltip`` |
| `Tree` | only ``role=`tree`` |
| `Treeitem` | only ``role=`treeitem`` |

## Type into a field

`~texts` gives the text that the checker types into each field of the view, through its `on_input` handler. The cause of such a step is the field by role and name, and the text, so `silica script` types it with `Memory.input`. A step size field that breaks "the step is positive" when the text is `"0"`:

From `test/replay/run.t`:

```ocaml
    let page = Memory.mount Stepper.component in
    Check_expect.holds broken Stepper.typing page (fun () ->
        Memory.input (Memory.by_role page Accessibility.Spinbutton ~name:"Step size") "0"
    )
```

Keep `~inputs` for events that no element gives. `silica script` still exits 1 on a step from `~inputs`, and says to give the text with `~texts` when the event is text typed into a field.

## Replay proves only what it can replay

A trace that the checker writes holds, for each step, which move the check took among the moves it offered there. `silica replay` takes the same moves, so a trace replays exactly with no codec, such as in the app that `silica new` writes. When the check changed, a step is the one move that fits its event name, data and cause; with no such move the step is not enabled (exit 1).

A trace that cannot be replayed faithfully is a setup error, exit 3, never a pass: a trace recorded with names only, or one where several moves fit a step:

From `test/check_suite/run.t`:

```sh
  replay: transition 2 (save) has no event data and no choice of the checker, so it cannot be replayed faithfully: the trace was recorded with names only, or the machine has no event codec (see Silica.Machine.register_codecs). To compare a recorded run with the specs and the code, run conform
  [3]
```

## More commands

- `silica check --graphs DIR` writes the graph of each checked machine; see [Check a project](checker.md#check-a-project).
- `silica conform TRACE_FILE...` compares a trace recorded in a running app with the specs and the code; see [Conform a trace](checker.md#conform-a-trace).
- `silica inspect` reads the machines of a running app; see [Inspector](../../../docs/devtools.mld).
