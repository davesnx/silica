open Silica

type event =
  | Input of string
  | Clear
  | Fire
  | Got of (string list, string) Remote.answer
  | Retry

type state =
  | Idle
  | Typing of string
  | Searching of string
  | Shown of string * string list
  | Failed of string * Remote.error

let name = function
  | Idle ->
      "idle"
  | Typing _ ->
      "typing"
  | Searching _ ->
      "searching"
  | Shown _ ->
      "shown"
  | Failed _ ->
      "failed"

let states = [ "idle"; "typing"; "searching"; "shown"; "failed" ]

let endpoint =
  Remote.declare ~path:"/api/search" ~input:Codec.string
    ~output:(Codec.list Codec.string) ~rejected:Codec.string

let note text = Action.Focus (Ref.make text)

let describe : type e. e Action.host -> string = function
  | Action.Start_timer { id; ms; _ } ->
      Printf.sprintf "start %s %dms" id ms
  | Action.Start_call { id; endpoint; _ } ->
      Printf.sprintf "call %s %s" id (Remote.path endpoint)
  | Action.Stop id ->
      "stop " ^ id
  | Action.Run (Action.Focus r) ->
      "note " ^ Ref.name r
  | _ ->
      "other"

let described : type e. e Action.host list -> string list =
 fun actions -> List.map describe actions

let strings = Alcotest.(list string)

let search =
  Machine.make ~id:"search" ~init:Idle ~name ~states
    ~during:(function
      | Typing _ ->
          [ Source.after 300 Fire ]
      | Searching query ->
          [ Remote.call endpoint query (fun answer -> Got answer) ]
      | Idle | Shown _ | Failed _ ->
          []
      )
    (fun state event ->
      match (state, event) with
      | _, Input query ->
          Machine.Reenter (Typing query)
      | _, Clear ->
          Machine.Go Idle
      | Typing query, Fire ->
          Machine.Go (Searching query)
      | Searching query, Got (Remote.Ok hits) ->
          Machine.Go (Shown (query, hits))
      | Searching query, Got (Remote.Rejected reason) ->
          Machine.Go (Failed (query, Remote.Decode reason))
      | Searching query, Got (Remote.Failed error) ->
          Machine.Go (Failed (query, error))
      | Failed (query, _), Retry ->
          Machine.Go (Searching query)
      | _ ->
          Machine.Ignore
    )

let typing_id = "search/typing/after 300ms"
let searching_id query = "search/searching//api/search \"" ^ query ^ "\""

let send snapshot event =
  let snapshot, actions, how = Machine.step search snapshot event in
  (snapshot, described actions, how)

let start () = fst (Machine.start search)

let test_start () =
  let snapshot, actions = Machine.start search in
  Alcotest.(check string) "init" "idle" (name snapshot);
  Alcotest.check strings "no entry" [] (described actions);
  Alcotest.(check string) "id" "search" (Machine.id search);
  Alcotest.(check bool) "not final" false (Machine.is_final search snapshot);
  Alcotest.(check (option strings))
    "states" (Some states) (Machine.states search)

let test_timer_starts_on_entry () =
  let snapshot, actions, how = send (start ()) (Input "ab") in
  Alcotest.(check string) "state" "typing" (name snapshot);
  Alcotest.(check bool) "entered" true (how = Machine.Entered);
  Alcotest.check strings "start" [ "start " ^ typing_id ^ " 300ms" ] actions

let test_timer_stops_on_exit () =
  let snapshot, _, _ = send (start ()) (Input "ab") in
  let snapshot, actions, _ = send snapshot Fire in
  Alcotest.(check string) "state" "searching" (name snapshot);
  Alcotest.check strings "stop then call"
    [ "stop " ^ typing_id; "call " ^ searching_id "ab" ^ " /api/search" ]
    actions;
  let typing, _, _ = send (start ()) (Input "x") in
  let _, actions, _ = send typing Clear in
  Alcotest.check strings "clear stops the timer" [ "stop " ^ typing_id ] actions

let test_reenter_restarts () =
  let snapshot, _, _ = send (start ()) (Input "a") in
  let snapshot, actions, how = send snapshot (Input "ab") in
  Alcotest.(check string) "state" "typing" (name snapshot);
  Alcotest.(check bool) "entered" true (how = Machine.Entered);
  Alcotest.check strings "debounce"
    [ "stop " ^ typing_id; "start " ^ typing_id ^ " 300ms" ]
    actions;
  Alcotest.(check bool) "new data" true (snapshot = Typing "ab")

let test_source_stops_on_exit () =
  let snapshot, _, _ = send (start ()) (Input "q") in
  let snapshot, _, _ = send snapshot Fire in
  let snapshot, actions, _ = send snapshot (Got (Remote.Ok [ "hit" ])) in
  Alcotest.(check string) "state" "shown" (name snapshot);
  Alcotest.check strings "stop" [ "stop " ^ searching_id "q" ] actions;
  let snapshot, _, _ = send (start ()) (Input "q") in
  let snapshot, _, _ = send snapshot Fire in
  let _, actions, _ = send snapshot Clear in
  Alcotest.check strings "leaving early stops the call"
    [ "stop " ^ searching_id "q" ]
    actions

let test_late_result_is_unhandled () =
  let snapshot, _, _ = send (start ()) (Input "q") in
  let late, actions, how = send snapshot (Got (Remote.Ok [ "old" ])) in
  Alcotest.(check bool) "unhandled" true (how = Machine.Unhandled);
  Alcotest.check strings "no actions" [] actions;
  Alcotest.(check bool) "same snapshot" true (late == snapshot)

let test_start_input_uses_data () =
  let snapshot, _, _ = send (start ()) (Input "cats") in
  let _, actions, _ = Machine.step search snapshot Fire in
  match actions with
  | [ _; Action.Start_call { input; endpoint; _ } ] ->
      Alcotest.(check string)
        "input" "\"cats\""
        (Codec.to_string (Remote.input endpoint) input)
  | _ ->
      Alcotest.fail "expected stop and call"

let resource_ids machine snapshot =
  List.map
    (function
      | Machine.Timer { id; _ }
      | Machine.Call { id; _ }
      | Machine.Subscription { id; _ }
      | Machine.Held { id; _ } ->
          id
      )
    (Machine.active machine snapshot)

let tracker () =
  let running = ref [] in
  let apply actions =
    List.iter
      (function
        | Action.Start_timer { id; _ }
        | Action.Start_call { id; _ }
        | Action.Start_source { id; _ }
        | Action.Start_live { id; _ } ->
            if List.mem id !running then Alcotest.fail ("started twice: " ^ id);
            running := List.sort compare (id :: !running)
        | Action.Stop id ->
            if not (List.mem id !running) then
              Alcotest.fail ("stopped while not running: " ^ id);
            running := List.filter (( <> ) id) !running
        | Action.Run _ ->
            ()
        )
      actions
  in
  (running, apply)

let test_active_follows_actions () =
  let running, apply = tracker () in
  let snapshot, actions = Machine.start search in
  apply actions;
  let snapshot = ref snapshot in
  List.iter
    (fun event ->
      let next, actions, _ = Machine.step search !snapshot event in
      snapshot := next;
      apply actions;
      Alcotest.check strings
        (name next ^ " resources")
        (List.sort compare (resource_ids search next))
        !running
    )
    [
      Input "a";
      Input "ab";
      Fire;
      Input "abc";
      Clear;
      Input "z";
      Fire;
      Got (Remote.Failed (Remote.Http 500));
      Retry;
      Got (Remote.Ok []);
      Clear;
    ]

type pair = A | B

let pair_name = function A -> "a" | B -> "b"

let test_entry_exit_order () =
  let machine =
    Machine.make ~id:"pair" ~init:A ~name:pair_name
      ~entry:(function A -> [ note "enter a" ] | B -> [ note "enter b" ])
      ~exit:(function A -> [ note "exit a" ] | B -> [])
      ~during:(function
        | A -> [ Source.after 5 () ] | B -> [ Source.after 7 () ]
        )
      (fun state () ->
        match state with
        | A ->
            Machine.Go_with (B, [ note "move" ])
        | B ->
            Machine.Ignore
      )
  in
  let describe actions =
    List.map
      (function
        | Action.Start_timer { id; _ } ->
            "start " ^ id
        | Action.Stop id ->
            "stop " ^ id
        | Action.Run (Action.Focus r) ->
            Ref.name r
        | _ ->
            "other"
        )
      actions
  in
  let snapshot, actions = Machine.start machine in
  Alcotest.check strings "start"
    [ "enter a"; "start pair/a/after 5ms" ]
    (describe actions);
  let _, actions, _ = Machine.step machine snapshot () in
  Alcotest.check strings "transition"
    [
      "exit a";
      "stop pair/a/after 5ms";
      "move";
      "enter b";
      "start pair/b/after 7ms";
    ]
    (describe actions);
  Alcotest.check strings "exit"
    [ "exit a"; "stop pair/a/after 5ms" ]
    (describe (Machine.exit machine snapshot))

type counter = Count of int

let test_self_goto_keeps_resources () =
  let machine =
    Machine.make ~id:"counter" ~init:(Count 0)
      ~name:(function Count _ -> "count")
      ~entry:(fun _ -> [ note "enter" ])
      ~exit:(fun _ -> [ note "exit" ])
      ~during:(fun _ -> [ Source.after 10 `Tick ])
      (fun (Count n) -> function
        | `Tick ->
            Machine.Go (Count (n + 1))
        | `Again ->
            Machine.Reenter (Count (n + 1))
        | `Note ->
            Machine.Stay [ note "stay" ]
        )
  in
  let snapshot, _ = Machine.start machine in
  let ticked, actions, how = Machine.step machine snapshot `Tick in
  Alcotest.(check int) "go: no actions" 0 (List.length actions);
  Alcotest.(check bool) "go: kept" true (how = Machine.Kept);
  Alcotest.(check bool) "go: new data" true (ticked = Count 1);
  let _, actions, how = Machine.step machine snapshot `Again in
  Alcotest.(check int)
    "reenter: exit, stop, enter, start" 4 (List.length actions);
  Alcotest.(check bool) "reenter: entered" true (how = Machine.Entered);
  let stayed, actions, how = Machine.step machine snapshot `Note in
  Alcotest.check strings "stay: its actions only" [ "note stay" ]
    (described actions);
  Alcotest.(check bool) "stay: kept" true (how = Machine.Kept);
  Alcotest.(check bool) "stay: same data" true (stayed = snapshot)

type list_state = Watching of string list

let list_endpoint =
  Remote.declare ~path:"/api/item" ~input:Codec.string ~output:Codec.int
    ~rejected:Codec.string

let watcher =
  Machine.make ~id:"watch"
    ~init:(Watching [ "a"; "b" ])
    ~name:(function Watching _ -> "watching")
    ~during:(fun (Watching items) ->
      List.map
        (fun item -> Remote.call list_endpoint item (fun _ -> `Answer))
        items
    )
    (fun _ -> function
      | `Set items ->
          Machine.Go (Watching items)
      | `Restart items ->
          Machine.Reenter (Watching items)
      | `Answer ->
          Machine.Stay []
      )

let call_id item = "watch/watching//api/item \"" ^ item ^ "\""

let test_keyed_sources () =
  let running, apply = tracker () in
  let snapshot, actions = Machine.start watcher in
  apply actions;
  Alcotest.check strings "both calls run" [ call_id "a"; call_id "b" ] !running;
  let snapshot, actions, how =
    Machine.step watcher snapshot (`Set [ "a"; "c" ])
  in
  apply actions;
  Alcotest.(check bool) "kept" true (how = Machine.Kept);
  Alcotest.check strings "the changed input restarts, the same key runs on"
    [ "stop " ^ call_id "b"; "call " ^ call_id "c" ^ " /api/item" ]
    (described actions);
  Alcotest.check strings "running" [ call_id "a"; call_id "c" ] !running;
  let snapshot, actions, _ =
    Machine.step watcher snapshot (`Set [ "a"; "c" ])
  in
  Alcotest.check strings "unchanged keys keep running" [] (described actions);
  let snapshot, actions, _ =
    Machine.step watcher snapshot (`Set [ "a"; "c"; "a" ])
  in
  apply actions;
  Alcotest.check strings "a duplicate gets #2"
    [ "call " ^ call_id "a" ^ "#2 /api/item" ]
    (described actions);
  Alcotest.check strings "active agrees" !running
    (List.sort compare (resource_ids watcher snapshot));
  let _, actions, how =
    Machine.step watcher snapshot (`Restart [ "a"; "c"; "a" ])
  in
  apply actions;
  Alcotest.(check bool) "reenter entered" true (how = Machine.Entered);
  Alcotest.check strings "reenter restarts all"
    [
      "stop " ^ call_id "a";
      "stop " ^ call_id "c";
      "stop " ^ call_id "a" ^ "#2";
      "call " ^ call_id "a" ^ " /api/item";
      "call " ^ call_id "c" ^ " /api/item";
      "call " ^ call_id "a" ^ "#2 /api/item";
    ]
    (described actions);
  Alcotest.(check string)
    "source key" "/api/item \"a\""
    (Source.key (Remote.call list_endpoint "a" (fun _ -> ())));
  Alcotest.(check string)
    "timer key" "after 5ms"
    (Source.key (Source.after 5 ()))

let test_anchor_place_is_in_key () =
  let popup = Ref.make "popup" and trigger = Ref.make "trigger" in
  let anchored side =
    Source.anchor ~place:{ Place.default with side } ~anchor:trigger popup
  in
  let machine =
    Machine.make ~id:"anchored" ~init:Place.Bottom
      ~name:(fun _ -> "open")
      ~during:(fun side -> [ anchored side ])
      (fun _ side -> Machine.Go side)
  in
  let snapshot, _ = Machine.start machine in
  let _, actions, how = Machine.step machine snapshot Place.Top in
  Alcotest.(check bool) "kept" true (how = Machine.Kept);
  Alcotest.(check int)
    "the old anchor stops, the new one starts" 2 (List.length actions);
  Alcotest.(check bool)
    "keys differ" true
    (Source.key (anchored Place.Top) <> Source.key (anchored Place.Bottom))

let test_state_names () =
  Alcotest.check strings "every constructor, in order"
    [ "idle"; "typing"; "searching"; "shown"; "failed" ]
    (Option.value ~default:[] (Machine.states search));
  List.iter
    (fun state ->
      Alcotest.(check bool)
        (Machine.name search state ^ " listed")
        true
        (List.mem (Machine.name search state) states)
    )
    [
      Idle; Typing ""; Searching ""; Shown ("", []); Failed ("", Remote.Http 1);
    ]

let test_event_name () =
  Alcotest.(check (option string))
    "no event_name: None" None
    (Machine.event_name search Fire);
  let named =
    Machine.make ~id:"named" ~init:Idle ~name
      ~event_name:(function
        | Input _ ->
            "input"
        | Clear ->
            "clear"
        | Fire ->
            "fire"
        | Got _ ->
            "got"
        | Retry ->
            "retry"
        )
      (fun _ _ -> Machine.Ignore)
  in
  Alcotest.(check (option string))
    "event_name: Some" (Some "input")
    (Machine.event_name named (Input "a"));
  Alcotest.(check (option string))
    "event_name of data" (Some "got")
    (Machine.event_name named (Got (Remote.Ok [])))

type gate = Open | Closed

let test_final_ignores_events () =
  let machine =
    Machine.make ~id:"gate" ~init:Open
      ~name:(function Open -> "open" | Closed -> "closed")
      ~final:(function Closed -> true | Open -> false)
      (fun state event ->
        match (state, event) with
        | _, `Reopen ->
            Machine.Go Open
        | Open, `Close ->
            Machine.Go Closed
        | Closed, `Close ->
            Machine.Stay [ note "ignored" ]
        | Open, `Other ->
            Machine.Ignore
        | Closed, `Other ->
            Machine.Ignore
      )
  in
  let snapshot, _ = Machine.start machine in
  let closed, _, _ = Machine.step machine snapshot `Close in
  Alcotest.(check bool) "final" true (Machine.is_final machine closed);
  List.iter
    (fun event ->
      let next, actions, how = Machine.step machine closed event in
      Alcotest.(check bool) "unhandled" true (how = Machine.Unhandled);
      Alcotest.(check int) "no actions" 0 (List.length actions);
      Alcotest.(check bool) "same" true (next == closed);
      Alcotest.(check bool) "cannot" false (Machine.can machine closed event)
    )
    [ `Close; `Reopen ];
  let _, _, how = Machine.step machine snapshot `Reopen in
  Alcotest.(check bool) "handled while not final" true (how <> Machine.Unhandled)

let test_can () =
  let idle = start () in
  Alcotest.(check bool)
    "input in idle" true
    (Machine.can search idle (Input "a"));
  Alcotest.(check bool) "fire in idle" false (Machine.can search idle Fire);
  let typing, _, _ = send idle (Input "a") in
  Alcotest.(check bool) "fire while typing" true (Machine.can search typing Fire);
  Alcotest.(check bool)
    "retry while typing" false
    (Machine.can search typing Retry);
  let typing_again, _, _ = send typing Retry in
  Alcotest.(check bool) "can agrees with step" true (typing_again == typing)

type form = { name : string; saving : bool }
type field = Editing of string | Saving_field of string

let field_step field event =
  match (field, event) with
  | Editing _, `Type text ->
      Machine.Go (Editing text)
  | Editing text, `Save ->
      Machine.Reenter (Saving_field text)
  | Editing _, `Note ->
      Machine.Stay [ note "noted" ]
  | Saving_field _, (`Type _ | `Save | `Note)
  | Editing _, `Other
  | Saving_field _, `Other ->
      Machine.Ignore

let test_map_next () =
  let into_form saving = function
    | Editing name ->
        { name; saving }
    | Saving_field name ->
        { name; saving = true }
  in
  let text form =
    form.name
    ^
    if form.saving then
      " (saving)"
    else
      ""
  in
  let shown = function
    | Machine.Ignore ->
        "ignore"
    | Machine.Stay [ Action.Focus r ] ->
        "stay " ^ Ref.name r
    | Machine.Stay _ ->
        "stay"
    | Machine.Go form ->
        "go " ^ text form
    | Machine.Go_with (form, _) ->
        "go_with " ^ text form
    | Machine.Reenter form ->
        "reenter " ^ text form
  in
  let step event =
    shown (Machine.map_next (into_form false) (field_step (Editing "a") event))
  in
  Alcotest.(check string) "go" "go ab" (step (`Type "ab"));
  Alcotest.(check string) "reenter" "reenter a (saving)" (step `Save);
  Alcotest.(check string) "ignore" "ignore" (step `Other);
  Alcotest.(check string) "stay keeps its actions" "stay noted" (step `Note);
  Alcotest.(check string)
    "go_with" "go_with abc (saving)"
    (shown
       (Machine.map_next (into_form true) (Machine.Go_with (Editing "abc", [])))
    )

type item_state = Showing | Loading_item of int

let item_endpoint =
  Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let item_machine on_props =
  Machine.make_with_props ~id:"item" ~init:Showing
    ~name:(function Showing -> "showing" | Loading_item _ -> "loading_item")
    ~during:(fun number -> function
      | Showing ->
          [ Source.after (number * 10) `Tick ]
      | Loading_item n ->
          [ Remote.call item_endpoint n (fun _ -> `Answer) ]
      )
    ~on_props
    (fun _ state event ->
      match (state, event) with
      | Showing, `Tick ->
          Machine.Stay []
      | _ ->
          Machine.Ignore
    )

let test_on_props () =
  let seen = ref [] in
  let machine =
    item_machine (fun previous number state ->
        seen := (previous, number) :: !seen;
        match (previous, state) with
        | None, _ ->
            Machine.Stay [ note ("start " ^ string_of_int number) ]
        | Some previous, Showing when number > 5 ->
            Machine.Go_with
              (Loading_item number, [ note ("was " ^ string_of_int previous) ])
        | Some _, _ ->
            Machine.Ignore
    )
  in
  Alcotest.(check (option unit))
    "no default props" None
    (Option.map ignore (Machine.default_props machine));
  let snapshot, actions = Machine.start_with_props machine 1 in
  Alcotest.(check (list (pair (option int) int)))
    "start: previous None"
    [ (None, 1) ]
    !seen;
  Alcotest.check strings "start: entry then on_props"
    [ "start item/showing/after 10ms 10ms"; "note start 1" ]
    (described actions);
  let snapshot, actions, how =
    Machine.set_props machine ~previous:1 2 snapshot
  in
  Alcotest.(check (list (pair (option int) int)))
    "change: previous Some"
    [ (Some 1, 2); (None, 1) ]
    !seen;
  Alcotest.(check bool) "kept" true (how = Machine.Kept);
  Alcotest.check strings "a source that depends on the props restarts"
    [ "stop item/showing/after 10ms"; "start item/showing/after 20ms 20ms" ]
    (described actions);
  let snapshot, actions, how =
    Machine.set_props machine ~previous:2 2 snapshot
  in
  Alcotest.(check bool) "same props: unhandled" true (how = Machine.Unhandled);
  Alcotest.check strings "same props: nothing" [] (described actions);
  let snapshot, actions, how =
    Machine.set_props machine ~previous:2 7 snapshot
  in
  Alcotest.(check bool) "go: entered" true (how = Machine.Entered);
  Alcotest.check strings "go: exit with the old props, enter with the new"
    [
      "stop item/showing/after 20ms";
      "note was 2";
      "call item/loading_item//api/double 7 /api/double";
    ]
    (described actions);
  Alcotest.(check bool) "state" true (snapshot = Loading_item 7);
  Alcotest.(check bool)
    "update gets the props" true
    (Machine.can_with_props machine 7 Showing `Tick)

let test_on_props_default () =
  let machine = item_machine (fun _ _ _ -> Machine.Ignore) in
  let snapshot, actions = Machine.start_with_props machine 3 in
  Alcotest.check strings "start"
    [ "start item/showing/after 30ms 30ms" ]
    (described actions);
  let _, actions, how = Machine.set_props machine ~previous:3 4 snapshot in
  Alcotest.(check bool) "sources moved: kept" true (how = Machine.Kept);
  Alcotest.check strings "ignore still restarts the keyed timer"
    [ "stop item/showing/after 30ms"; "start item/showing/after 40ms 40ms" ]
    (described actions)

let spec_state ?(final = false) ?(timers = []) ?(sources = []) name =
  { Spec.name; final; timers; sources; held = [] }

let edges pairs =
  List.map
    (fun (from, target) -> { Spec.from; target; event = ""; kept = false })
    pairs

let search_spec =
  {
    Spec.id = "search";
    init = "idle";
    states =
      [
        spec_state "idle";
        spec_state "failed";
        spec_state ~sources:[ "/api/search" ] "searching";
        spec_state "shown";
        spec_state ~timers:[ 300 ] "typing";
      ];
    edges =
      edges
        [
          ("failed", "searching");
          ("idle", "typing");
          ("searching", "failed");
          ("searching", "shown");
          ("shown", "idle");
          ("typing", "searching");
          ("typing", "typing");
        ];
  }

let test_lint () =
  Alcotest.(check (list string))
    "search is clean" []
    (List.map Spec.issue_to_string (Spec.lint search_spec));
  let bad =
    {
      Spec.id = "bad";
      init = "start";
      states =
        [
          spec_state "start";
          spec_state "island";
          spec_state "sink";
          spec_state ~timers:[ 1 ] "timed";
          spec_state ~final:true "done";
        ];
      edges =
        edges
          [
            ("island", "done");
            ("start", "sink");
            ("start", "timed");
            ("timed", "timed");
          ];
    }
  in
  Alcotest.(check (list string))
    "issues"
    [
      "bad.island: unreachable";
      "bad.sink: non-final dead end";
      "bad.timed: non-final dead end";
      "bad.done: unreachable";
    ]
    (List.map Spec.issue_to_string (Spec.lint bad))

let test_dot () =
  let expected =
    {|digraph "search" {
rankdir=LR;
"search.__init" [shape=point];
"search.__init" -> "search.idle";
"search.idle" [label="idle" shape=box];
"search.failed" [label="failed" shape=box];
"search.searching" [label="searching\ninvoke /api/search" shape=box];
"search.shown" [label="shown" shape=box];
"search.typing" [label="typing\nafter 300ms" shape=box];
"search.failed" -> "search.searching";
"search.idle" -> "search.typing";
"search.searching" -> "search.failed";
"search.searching" -> "search.shown";
"search.shown" -> "search.idle";
"search.typing" -> "search.searching";
"search.typing" -> "search.typing";
}
|}
  in
  Alcotest.(check string) "dot" expected (Spec.to_dot search_spec)

let double =
  Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let test_remote_machine () =
  let machine = Remote_machine.make ~id:"r" double in
  let snapshot, actions = Machine.start machine in
  Alcotest.(check bool) "idle" true (snapshot = Remote_machine.Idle);
  Alcotest.(check int) "no entry" 0 (List.length actions);
  let loading, actions, _ =
    Machine.step machine snapshot (Remote_machine.Load 4)
  in
  Alcotest.(check bool)
    "loading" true
    (loading = Remote_machine.Loading { input = 4; last = None });
  let first_id =
    match actions with
    | [
     Action.Start_call { id; endpoint; input; on_result; timeout_ms = None };
    ] -> (
        Alcotest.(check string) "path" "/api/double" (Remote.path endpoint);
        Alcotest.(check string)
          "input" "4"
          (Codec.to_string (Remote.input endpoint) input);
        match on_result (Remote.Failed (Network "x")) with
        | Remote_machine.Settled (Remote.Failed (Network "x")) ->
            id
        | _ ->
            Alcotest.fail "continuation must build Settled"
      )
    | _ ->
        Alcotest.fail "expected one Start_call"
  in
  let loaded, actions, _ =
    Machine.step machine loading (Remote_machine.Settled (Remote.Ok 8))
  in
  Alcotest.(check bool)
    "ready" true
    (loaded = Remote_machine.Ready { input = 4; data = 8 });
  Alcotest.check strings "stops the call"
    [ "stop " ^ first_id ]
    (described actions);
  let failed, _, _ =
    Machine.step machine loading
      (Remote_machine.Settled (Remote.Failed (Http 500)))
  in
  Alcotest.(check bool)
    "failed" true
    (failed = Remote_machine.Failed { input = 4; error = Http 500; last = None });
  let rejected, _, _ =
    Machine.step machine loading (Remote_machine.Settled (Remote.Rejected "no"))
  in
  Alcotest.(check bool)
    "rejected" true
    (rejected
    = Remote_machine.Rejected { input = 4; reason = "no"; last = None }
    );
  let _, _, how =
    Machine.step machine snapshot (Remote_machine.Settled (Remote.Ok 1))
  in
  Alcotest.(check bool) "result without a call" true (how = Machine.Unhandled);
  let reset, _, _ = Machine.step machine loaded Remote_machine.Reset in
  Alcotest.(check bool) "reset" true (reset = Remote_machine.Idle)

type slow = Waiting | Done

let test_call_timeout () =
  let machine =
    Machine.make ~id:"slow" ~init:Waiting
      ~name:(function Waiting -> "waiting" | Done -> "done")
      ~during:(function
        | Waiting ->
            [
              Remote.call ~timeout_ms:250 double 1 Fun.id;
              Remote.call double 2 Fun.id;
            ]
        | Done ->
            []
        )
      (fun state _ ->
        match state with Waiting -> Machine.Go Done | Done -> Machine.Ignore
      )
  in
  let _, actions = Machine.start machine in
  let timeouts =
    List.map
      (function
        | Action.Start_call { timeout_ms; _ } ->
            timeout_ms
        | _ ->
            Alcotest.fail "expected Start_call"
        )
      actions
  in
  Alcotest.(check (list (option int)))
    "timeout only where asked" [ Some 250; None ] timeouts

let test_remote_machine_reload () =
  let machine = Remote_machine.make ~id:"r" double in
  let snapshot, _ = Machine.start machine in
  let snapshot, _, _ = Machine.step machine snapshot (Remote_machine.Load 1) in
  let snapshot, actions, _ =
    Machine.step machine snapshot (Remote_machine.Load 2)
  in
  Alcotest.(check bool)
    "newest input" true
    (snapshot = Remote_machine.Loading { input = 2; last = None });
  Alcotest.check strings "old call stops, new call starts"
    [
      "stop r/loading//api/double 1"; "call r/loading//api/double 2 /api/double";
    ]
    (described actions);
  let _, actions, _ = Machine.step machine snapshot Reset in
  Alcotest.check strings "reset stops"
    [ "stop r/loading//api/double 2" ]
    (described actions)

type connection = Connecting | Live | Offline | Closed

let connection_topic = Topic.declare ~name:"feed" ~state:Codec.int

let connection_name = function
  | Connecting ->
      "list.connecting"
  | Live ->
      "list.live"
  | Offline ->
      "list.offline"
  | Closed ->
      "closed"

let test_sub_states_keep_sources () =
  let machine =
    Machine.make ~id:"feed" ~init:Connecting ~name:connection_name
      ~entry:(fun state -> [ note ("enter " ^ connection_name state) ])
      ~during:(function
        | Connecting | Live | Offline ->
            [
              Topic.subscribe connection_topic "main" (fun _ -> Connecting);
              Remote.call endpoint "change" (fun _ -> Connecting);
            ]
        | Closed ->
            []
        )
      (fun _ next -> Machine.Go next)
  in
  let actions from next =
    let _, actions, how = Machine.step machine from next in
    (described actions, how)
  in
  let sub_state =
    Alcotest.testable
      (fun format (actions, how) ->
        Format.fprintf format "%s (%s)"
          (String.concat "; " actions)
          ( match how with
          | Machine.Kept ->
              "kept"
          | Machine.Entered ->
              "entered"
          | Machine.Unhandled ->
              "unhandled"
          )
      )
      ( = )
  in
  Alcotest.check sub_state
    "list.connecting to list.live keeps the subscription and the call"
    ([], Machine.Kept) (actions Connecting Live);
  Alcotest.check sub_state "list.live to list.offline too" ([], Machine.Kept)
    (actions Live Offline);
  Alcotest.check sub_state
    "list.offline to closed leaves list: stops both, enters closed"
    ( [
        "stop feed/list/live:feed main";
        "stop feed/list//api/search \"change\"";
        "note enter closed";
      ],
      Machine.Entered
    )
    (actions Offline Closed)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica machine"
    [
      ( "machine",
        [
          t "start" test_start;
          t "timer starts on entry" test_timer_starts_on_entry;
          t "timer stops on exit" test_timer_stops_on_exit;
          t "reenter restarts the timer" test_reenter_restarts;
          t "source stops on exit" test_source_stops_on_exit;
          t "late result is unhandled" test_late_result_is_unhandled;
          t "source input uses the data" test_start_input_uses_data;
          t "active follows the actions" test_active_follows_actions;
          t "entry and exit order" test_entry_exit_order;
          t "self goto keeps resources" test_self_goto_keeps_resources;
          t "keyed sources" test_keyed_sources;
          t "sub-states of a dotted name keep the sources"
            test_sub_states_keep_sources;
          t "state names cover every state" test_state_names;
          t "event_name" test_event_name;
          t "final ignores events" test_final_ignores_events;
          t "anchor place is in the key" test_anchor_place_is_in_key;
          t "can" test_can;
          t "map_next" test_map_next;
          t "on_props" test_on_props;
          t "props without on_props" test_on_props_default;
        ]
      );
      ("spec", [ t "lint" test_lint; t "dot" test_dot ]);
      ( "remote machine",
        [
          t "states" test_remote_machine;
          t "reload restarts the call" test_remote_machine_reload;
          t "call timeout is carried to the host" test_call_timeout;
        ]
      );
    ]
