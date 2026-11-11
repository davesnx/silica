# Spike: machines over a plain variant

The states are a plain OCaml variant with the data inside each constructor.
There is no `State` GADT, no `Machine.Make`, and no `At (State, data)` in views.
The new `Machine.make` sits next to `Machine.Make`. Old code does not change.

## The API

```ocaml
(* lib/machine.mli *)
type ('s, 'e) next =
  | Ignore                          (* not handled: the snapshot stays *)
  | Stay of 'e Cmd.t list           (* handled: keep the snapshot, run the commands *)
  | Go of 's                        (* same name: only the data changes *)
  | Go_with of 's * 'e Cmd.t list   (* Go, and run the commands *)
  | Reenter of 's                   (* Go, but exit and enter also on the same name *)

val make :
  id:string ->
  init:'s ->
  name:('s -> string) ->
  ?owns:('s -> (unit, 'e) Source.t list) ->
  ?entry:('s -> 'e Cmd.t list) ->
  ?exit:('s -> 'e Cmd.t list) ->
  ?final:('s -> bool) ->
  ?start:'e Cmd.t list ->
  ('s -> 'e -> ('s, 'e) next) ->
  ('s, 'e) t

(* lib/source.mli *)
val after : int -> 'e -> ('p, 'e) Source.t   (* a timer, owned like a source *)
val delay : ('p, 'e) Source.t -> int option

(* check/check.mli: one new field *)
| Ok of { ...; spec : Silica.Spec.t }          (* the spec that the run found *)
```

`Machine.make` builds the same record as `Machine.Make`, so hosts, the checker,
`Instance` and `map_snapshot` work with no change. The ids of timers and
sources are the same as before (`<id>/<name>/after<n>`,
`<id>/<name>/invoke<n>`), so the command logs of old and new are equal.

## Decisions

1. **The update returns `next`, not `state option`.** Two of the three ports
   need commands on a step (`Go_with`, `Stay`) and one needs `Reenter`. With
   `state option`, commands need a second function that repeats the match. The
   constructors are as short as `Some`/`None`: `Go Saving`, `Ignore`. Inside
   `Machine.make` they need no prefix (the type is known). In a helper outside
   it, annotate the result, `: (state, event) Machine.next`, or write
   `Machine.Go`.
2. **The update is the last positional argument, not `~update:`.** A function
   whose last argument is optional or labelled gives warning 16 (an optional
   argument that cannot be erased), and the build treats it as an error.
3. **Entry and exit.** The machine enters a state when `name` changes. `Go` to
   the same name keeps timers and sources running; `Reenter` restarts them.
   `entry` and `exit` are functions of the whole state, like `owns`:
   `~entry:(function Loading _ -> [ Cmd.Set_title "Issues" ] | _ -> [])`.
   Commands of a step go in `Go_with` and `Stay`; output to the parent stays
   in `Component.node ~output`, which now matches on plain states.
4. **Timers are sources.** `Source.after 2000 Reset` goes in `owns` with the
   calls and held sources. One function lists all that a state owns, which
   reads like the old row of the table. No separate `~after`.
5. **`final` stays** as `?final:(state -> bool)`. **`always` goes away**: a
   case such as `| _, Clear -> Go Empty` before the last `| _ -> Ignore` does
   the same, and a final state still handles nothing.
6. **Sources take `unit` data.** `owns` gets the state, so the data is in
   scope: `Source.call save (fun () -> "draft")`, `Popup.scoped (fun () ->
   props.id)`. For one name, `owns` must give the same list of sources in the
   same order (the ids are positions). The checker's `timer-owner` and
   `call-owner` invariants catch a list that changes.
7. **The spec comes from the checker.** `Machine.spec` of a new machine lists
   the initial state only and no edge. `Check.Ok` now carries `spec`: the
   states the run reached (initial first, then by name), each with the timers
   and sources of the first snapshot in it, and the edges it took. The ports
   export Quint and Graphviz from it (`spike/test/spec/`).
8. **State names are a hand-written function.** A deriver that gives the
   lower-case constructor name would remove 7 to 9 lines from each machine
   (the formatter puts each case on two lines). It is worth doing later; this
   spike has no ppx.

## What changes for the spec and the checker

- `Undeclared_target` and `Missing_case` cannot happen: nothing is declared.
  The compiler takes over part of `Missing_case`: `name`, `owns` and `view`
  are exhaustive matches, so a new state is reported in three places. The
  update ends with `| _ -> Ignore`, so the compiler does not report a new
  event or state there.
- `Unreached` never fires for a new machine: there are no declared edges to
  miss. Spec drift shows only as a change of the exported `.qnt` and `.dot`
  files, which the golden diff rules catch.
- `Spec.lint` on `Machine.spec` finds nothing. On `report.spec`, `Unreachable`
  cannot fire (each state in it was reached); `Dead_end` still works:
  `issue-list.ready: non-final dead end` is found as before.
- The exported files differ from the old ones in two ways:
  - The old spec added a self edge to each state with a timer, a source, an
    entry or an exit, because `reenter` could run them again
    (`open -> open`, `loading -> loading`, `saved -> saved`). The explored
    spec has a self edge only when a run took one. The Quint model is
    tighter.
  - States are in the order initial, then by name, not declaration order. A
    plain variant does not give its constructors in order.
- The explored spec holds only what the check reached. A state or edge
  outside `~within`, or one that needs a sample the check does not give, is
  not in it. The spec now depends on the check's inputs.
- `Check.Ok` has one more field. Code that builds it, or matches every field
  without `; _`, must change. No code in the repository does.

## The ports

Each port is a new file in `spike/`; the originals do not change.
`spike/test/test_spike.ml` runs each pair through `Check.component` with the
same inputs and compares states, transitions, taken edges and explored spec.
It drives both through the same steps in `Sim` and compares the page HTML, the
running timers and calls, the held sources and the command log after each
step. It also compares the server render (`Render.to_string`). All are equal.

| Component | Lines before | Lines after | Machine part before | after | Check: states, transitions |
|---|---|---|---|---|---|
| Landing save | 59 | 53 | 40 | 36 | 5, 7 (same) |
| Dialog | 266 | 253 | 85 | 71 | 11, 90 (same) |
| Issue list | 432 | 438 | 202 | 209 | 46854, 897302 (same) |

The machine part runs from the state type to the line before `view` or
`el`. Both versions are formatted by `ocamlformat`.

### Landing save

Before:

```ocaml
module States = struct
  type _ t =
    | Idle : unit t
    | Saving : unit t
    | Saved : string t
    | Failed : Remote.error t
end

module M = Machine.Make (States)

let machine =
  M.make ~id:"save" ~init:(M.at Idle ())
    [
      M.state Idle "idle" ~to_:[ Saving ] (fun () -> function
        | Save -> M.goto Saving ()
        | Answer _ | Reset -> M.unhandled);
      M.state Saving "saving" ~to_:[ Saved; Failed ]
        ~invoke:[ Source.call save (fun () -> "draft") (fun r -> Answer r) ]
        (fun () -> function
          | Answer (Ok time) -> M.goto Saved time
          | Answer (Error error) -> M.goto Failed error
          | Save | Reset -> M.unhandled);
      M.state Saved "saved" ~to_:[ Idle ] ~after:[ (2000, Reset) ]
        (fun _ -> function Reset -> M.goto Idle () | _ -> M.unhandled);
      M.state Failed "failed" ~to_:[ Saving ] (fun _ -> function
        | Save -> M.goto Saving ()
        | Answer _ | Reset -> M.unhandled);
    ]

let view (snapshot : M.snapshot) : event Node.t =
  match snapshot with
  | M.At (Idle, ()) -> <button on_press=Save>"Save the draft"</button>
  | M.At (Saved, time) -> <p>(Node.text ("Saved at " ^ time))</p>
  ...
```

After:

```ocaml
type state = Idle | Saving | Saved of string | Failed of Remote.error

let state_name = function
  | Idle -> "idle"
  | Saving -> "saving"
  | Saved _ -> "saved"
  | Failed _ -> "failed"

let machine =
  Machine.make ~id:"save" ~init:Idle ~name:state_name
    ~owns:(function
      | Saving -> [ Source.call save (fun () -> "draft") (fun result -> Answer result) ]
      | Saved _ -> [ Source.after 2000 Reset ]
      | Idle | Failed _ -> [])
    (fun state event ->
      match (state, event) with
      | (Idle | Failed _), Save -> Go Saving
      | Saving, Answer (Ok time) -> Go (Saved time)
      | Saving, Answer (Error error) -> Go (Failed error)
      | Saved _, Reset -> Go Idle
      | _ -> Ignore)

let view : state -> event Node.t = function
  | Idle -> <button on_press=Save>"Save the draft"</button>
  | Saved time -> <p>(Node.text ("Saved at " ^ time))</p>
  ...
```

(The snippets put each case on one line to fit side by side; the files are
formatted.)

### Dialog

Before:

```ocaml
module State = struct
  type _ t = Idle : unit t | Closed : props t | Open : props t
end

module M = Machine.Make (State)

type snapshot = M.snapshot = At : 'data State.t * 'data -> snapshot

let sources =
  List.map (Popup.scoped (fun (props : props) -> props.id)) [ ... ]

let close (props : props) =
  let cmds = match props.trigger with
    | Some _ -> [ Cmd.Focus (parts props).trigger ] | None -> [] in
  M.goto Closed props ~cmds

let machine =
  M.make ~id:Content.id ~init:(M.at Idle ())
    [
      M.state Idle "idle" ~to_:[ Closed; Open ] (fun () -> function
        | Props props when opened_by None props -> M.goto Open props
        | Props props -> M.goto Closed props
        | _ -> M.unhandled);
      M.state Closed "closed" ~to_:[ Open ] (fun previous -> function
        | Props props when opened_by (Some previous) props -> M.goto Open props
        | Props props -> M.goto Closed props
        | Open_request _ -> M.goto Open previous
        | Close_request _ | Content _ -> M.unhandled);
      M.state Open "open" ~to_:[ Closed ] ~invoke:sources (fun previous -> function
        | Props props when closed_by previous props -> close props
        | Props props -> M.goto Open props
        | Close_request reason when dismisses previous reason -> close previous
        | Close_request _ -> M.unhandled
        | Content event when Content.closes event -> close previous
        | Content _ -> M.stay []
        | Open_request _ -> M.unhandled);
    ]
```

After:

```ocaml
type state = Idle | Closed of props | Open of props

let state_name = function Idle -> "idle" | Closed _ -> "closed" | Open _ -> "open"

let sources (props : props) =
  List.map (Popup.scoped (fun () -> props.id)) [ ... ]

let close (props : props) : (state, event) Machine.next =
  match props.trigger with
  | Some _ -> Go_with (Closed props, [ Cmd.Focus (parts props).trigger ])
  | None -> Go (Closed props)

let machine =
  Machine.make ~id:Content.id ~init:Idle ~name:state_name
    ~owns:(function Open props -> sources props | Idle | Closed _ -> [])
    (fun state event ->
      match (state, event) with
      | Idle, Props props when opened_by None props -> Go (Open props)
      | Closed previous, Props props when opened_by (Some previous) props -> Go (Open props)
      | (Idle | Closed _), Props props -> Go (Closed props)
      | Closed previous, Open_request _ -> Go (Open previous)
      | Open previous, Props props when closed_by previous props -> close props
      | Open _, Props props -> Go (Open props)
      | Open previous, Close_request reason when dismisses previous reason -> close previous
      | Open previous, Content event when Content.closes event -> close previous
      | Open _, Content _ -> Stay []
      | _ -> Ignore)
```

The view and the parent output also get shorter:
`| Open_request reason, At (Open, _) ->` becomes `| Open_request reason, Open _ ->`.

### Issue list

Before:

```ocaml
module States = struct
  type _ t =
    | Loading : Linear_view.t t
    | Ready : ready t
    | Failed : (Linear_view.t * Remote.error) t
end

module M = Machine.Make (States)

let ready_event (r : ready) = function
  | Props view -> M.goto Ready { r with view }
  | Reconnect -> M.reenter Ready r
  | Filter_changed filter -> M.stay [ show { r.view with filter } ]
  ...

let machine =
  M.make ~id:"issue-list" ~init:(M.at Loading Linear_view.default)
    [
      M.state Loading "loading" ~to_:[ Ready; Failed ]
        ~entry:(fun _ -> [ Cmd.Set_title "Issues" ])
        ~invoke:[ Source.call issues_endpoint (fun _ -> ()) (fun r -> Loaded r) ]
        (fun view -> function
          | Props view -> M.goto Loading view
          | Loaded (Ok issues) -> M.goto Ready { view; data = ...; ... }
          | Loaded (Error error) -> M.goto Failed (view, error)
          | _ -> M.unhandled);
      M.state Ready "ready"
        ~invoke:[ Source.live issues_topic (fun _ -> issues_key) (fun update -> Updated update);
                  Source.keys ~prevent:true list_keys ]
        ready_event;
      M.state Failed "failed" ~to_:[ Loading ] (fun (view, error) -> function
        | Retry -> M.goto Loading view
        | Props view -> M.goto Failed (view, error)
        | _ -> M.unhandled);
    ]
```

After:

```ocaml
type state =
  | Loading of Linear_view.t
  | Ready of ready
  | Failed of Linear_view.t * Remote.error

let state_name = function Loading _ -> "loading" | Ready _ -> "ready" | Failed _ -> "failed"

let ready_event (r : ready) : event -> (state, event) Machine.next = function
  | Props view -> Go (Ready { r with view })
  | Reconnect -> Reenter (Ready r)
  | Filter_changed filter -> Stay [ show { r.view with filter } ]
  ...

let machine =
  Machine.make ~id:"issue-list" ~init:(Loading Linear_view.default) ~name:state_name
    ~entry:(function Loading _ -> [ Cmd.Set_title "Issues" ] | _ -> [])
    ~owns:(function
      | Loading _ -> [ Source.call issues_endpoint (fun () -> ()) (fun r -> Loaded r) ]
      | Ready _ -> [ Source.live issues_topic (fun () -> issues_key) (fun update -> Updated update);
                     Source.keys ~prevent:true list_keys ]
      | Failed _ -> [])
    (fun state event ->
      match (state, event) with
      | Loading _, Props view -> Go (Loading view)
      | Loading view, Loaded (Ok issues) -> Go (Ready { view; data = ...; ... })
      | Loading view, Loaded (Error error) -> Go (Failed (view, error))
      | Ready r, event -> ready_event r event
      | Failed (view, _), Retry -> Go (Loading view)
      | Failed (_, error), Props view -> Go (Failed (view, error))
      | _ -> Ignore)
```

The port reuses the event type of the original
(`type event = Linear_list.event = ...`) so that the checks can share the
event labels; the text of the type is the same.

## Better

- No GADT, no functor, no `module M`, no `At (State, data)`, no `M.` prefix.
  Views and parent output match plain constructors.
- One `match (state, event)` shows every transition of the machine in one
  place, and the compiler checks the patterns.
- No `~to_` lists to keep in step with the handlers, and no runtime
  `Undeclared_target`.
- `owns` puts timers, calls and held sources of a state in one list.
- The landing example and the Dialog are 6 and 13 lines shorter; their machine
  parts are 4 and 14 lines shorter.

## Worse

- `state_name` is boilerplate: 7 to 9 formatted lines per machine. A deriver
  would remove it.
- `update` ends with `| _ -> Ignore`, so the compiler does not report an
  event or state that a new constructor leaves unhandled. The old table
  forced one case per state (as a runtime `Missing_case`, not at compile
  time).
- `Go (Ready { ... })` wraps the data once more; the formatter adds 2 lines to
  each multi-line record. The issue list is 6 lines longer, mostly for this
  and `state_name`.
- Helpers outside `Machine.make` need a `(state, event) Machine.next`
  annotation to write `Go` without a prefix.
- `entry`, `exit` and `owns` match on the state, so a state with nothing to
  own still needs a case (`| Idle | Failed _ -> []`).
- The spec needs a checker run. Without a check, there are no edges for
  lint, Quint or Graphviz. `Unreached` cannot report spec drift.
- `owns` must give the same list for one name. The type does not enforce it.

## Open questions for you

1. `next` constructors (`Go`, `Go_with`, `Stay`, `Reenter`, `Ignore`), or
   `state option` plus a separate way to give commands? Names are open too
   (`Go_with` could be `Go_and_run`, `Ignore` could be `Unhandled`).
2. Should `Reenter` take commands too (`Reenter_with`)? None of the three
   ports needs it.
3. Is the update as the last positional argument acceptable, or do you want
   `~update:` with a trailing `()`?
4. `Source.t` keeps its data parameter, which is always `unit` here. Should
   the final design drop it, so that a call is
   `Source.call save "draft" (fun result -> Answer result)`?
5. Is a spec that comes only from a checker run acceptable? The alternative is
   a list of sample states (`~states:[ Idle; Saving; Saved ""; ... ]`) to list
   states and their sources without running, which brings back a
   declaration.
6. Should the exported state order be initial-then-by-name (this spike) or
   the order in which the check found them?
7. Write the `state_name` deriver now, or wait until the API is settled?
8. Keep `final` as a function, or drop it? Only `on_final` and the checker's
   dead-end invariant read it, and none of the three ports uses it.
