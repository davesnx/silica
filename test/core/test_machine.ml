open Silica

type event =
  | Input of string
  | Clear
  | Fire
  | Got of (string list, Remote.error) result
  | Retry

module State = struct
  type _ t =
    | Idle : unit t
    | Typing : string t
    | Searching : string t
    | Shown : (string * string list) t
    | Failed : (string * Remote.error) t
end

module Search = Machine.Make (State)
open State

let endpoint =
  Remote.declare ~path:"/api/search" ~input:Codec.string
    ~output:(Codec.list Codec.string)

let note text = Cmd.Focus (Ref.make text)

let describe : type e. e Cmd.host -> string = function
  | Cmd.Start_timer { id; ms; _ } ->
      Printf.sprintf "start %s %dms" id ms
  | Cmd.Start_call { id; endpoint; _ } ->
      Printf.sprintf "call %s %s" id (Remote.path endpoint)
  | Cmd.Stop id ->
      "stop " ^ id
  | Cmd.Run (Cmd.Focus r) ->
      "note " ^ Ref.name r
  | _ ->
      "other"

let described : type e. e Cmd.host list -> string list =
 fun cmds -> List.map describe cmds
let strings = Alcotest.(list string)

let search =
  Search.make ~id:"search" ~init:(Search.at Idle ())
    [
      Search.always ~to_:[ Typing; Idle ] (function
        | Input query ->
            Search.reenter Typing query
        | Clear ->
            Search.goto Idle ()
        | _ ->
            Search.unhandled
        );
      Search.state Idle "idle" Search.ignore;
      Search.state Typing "typing"
        ~after:[ (300, Fire) ]
        ~to_:[ Searching ]
        (fun query -> function
          | Fire -> Search.goto Searching query | _ -> Search.unhandled
          );
      Search.state Searching "searching" ~to_:[ Shown; Failed ]
        ~invoke:[ Source.call endpoint Fun.id (fun result -> Got result) ]
        (fun query -> function
          | Got (Ok hits) ->
              Search.goto Shown (query, hits)
          | Got (Error error) ->
              Search.goto Failed (query, error)
          | _ ->
              Search.unhandled
          );
      Search.state Shown "shown" Search.ignore;
      Search.state Failed "failed" ~to_:[ Searching ]
        (fun (query, _) -> function
        | Retry ->
            Search.goto Searching query
        | _ ->
            Search.unhandled
        );
    ]

let send snapshot event =
  let snapshot, cmds, handled = Machine.send search snapshot event in
  (snapshot, described cmds, handled)

let state_name = Machine.key search
let start () = fst (Machine.start search)

let test_start () =
  let snapshot, cmds = Machine.start search in
  Alcotest.(check string) "init" "idle" (state_name snapshot);
  Alcotest.check strings "no entry" [] (described cmds);
  Alcotest.(check string) "id" "search" (Machine.id search);
  Alcotest.(check bool) "not final" false (Machine.is_final search snapshot)

let test_timer_starts_on_entry () =
  let snapshot, cmds, handled = send (start ()) (Input "ab") in
  Alcotest.(check string) "state" "typing" (state_name snapshot);
  Alcotest.(check bool) "handled" true (handled = `Handled);
  Alcotest.check strings "start" [ "start search/typing/after0 300ms" ] cmds

let test_timer_stops_on_exit () =
  let snapshot, _, _ = send (start ()) (Input "ab") in
  let snapshot, cmds, _ = send snapshot Fire in
  Alcotest.(check string) "state" "searching" (state_name snapshot);
  Alcotest.check strings "stop then call"
    [ "stop search/typing/after0"; "call search/searching/invoke0 /api/search" ]
    cmds;
  let typing, _, _ = send (start ()) (Input "x") in
  let _, cmds, _ = send typing Clear in
  Alcotest.check strings "clear stops the timer"
    [ "stop search/typing/after0" ]
    cmds

let test_reenter_restarts () =
  let snapshot, _, _ = send (start ()) (Input "a") in
  let snapshot, cmds, _ = send snapshot (Input "ab") in
  Alcotest.(check string) "state" "typing" (state_name snapshot);
  Alcotest.check strings "debounce"
    [ "stop search/typing/after0"; "start search/typing/after0 300ms" ]
    cmds;
  let query : Search.snapshot -> string option = function
    | Search.At (Typing, query) ->
        Some query
    | _ ->
        None
  in
  Alcotest.(check (option string)) "new data" (Some "ab") (query snapshot)

let test_source_stops_on_exit () =
  let snapshot, _, _ = send (start ()) (Input "q") in
  let snapshot, _, _ = send snapshot Fire in
  let snapshot, cmds, _ = send snapshot (Got (Ok [ "hit" ])) in
  Alcotest.(check string) "state" "shown" (state_name snapshot);
  Alcotest.check strings "stop" [ "stop search/searching/invoke0" ] cmds;
  let snapshot, _, _ = send (start ()) (Input "q") in
  let snapshot, _, _ = send snapshot Fire in
  let _, cmds, _ = send snapshot Clear in
  Alcotest.check strings "leaving early stops the call"
    [ "stop search/searching/invoke0" ]
    cmds

let test_late_result_is_unhandled () =
  let snapshot, _, _ = send (start ()) (Input "q") in
  let late, cmds, handled = send snapshot (Got (Ok [ "old" ])) in
  Alcotest.(check bool) "unhandled" true (handled = `Unhandled);
  Alcotest.check strings "no commands" [] cmds;
  Alcotest.(check bool) "same snapshot" true (late == snapshot)

let test_start_input_uses_data () =
  let snapshot, _, _ = send (start ()) (Input "cats") in
  let _, cmds, _ = Machine.send search snapshot Fire in
  match cmds with
  | [ _; Cmd.Start_call { input; endpoint; _ } ] ->
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

let test_active_follows_commands () =
  let running = ref [] in
  let apply cmds =
    List.iter
      (function
        | Cmd.Start_timer { id; _ } | Cmd.Start_call { id; _ } ->
            running := List.sort_uniq compare (id :: !running)
        | Cmd.Stop id ->
            running := List.filter (( <> ) id) !running
        | _ ->
            ()
        )
      cmds
  in
  let snapshot, cmds = Machine.start search in
  apply cmds;
  let snapshot = ref snapshot in
  List.iter
    (fun event ->
      let next, cmds, _ = Machine.send search !snapshot event in
      snapshot := next;
      apply cmds;
      Alcotest.check strings
        (state_name next ^ " resources")
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
      Got (Error (Remote.Http 500));
      Retry;
      Got (Ok []);
      Clear;
    ]

let test_entry_exit_order () =
  let module Pair = struct
    type _ t = A : unit t | B : unit t
  end in
  let module Pair_machine = Machine.Make (Pair) in
  let machine =
    Pair_machine.make ~id:"pair"
      ~init:(Pair_machine.at Pair.A ())
      [
        Pair_machine.state Pair.A "a" ~to_:[ Pair.B ]
          ~entry:(fun () -> [ note "enter a" ])
          ~exit:(fun () -> [ note "exit a" ])
          ~after:[ (5, ()) ]
          (fun () () -> Pair_machine.goto ~cmds:[ note "move" ] Pair.B ());
        Pair_machine.state Pair.B "b"
          ~entry:(fun () -> [ note "enter b" ])
          ~after:[ (7, ()) ]
          Pair_machine.ignore;
      ]
  in
  let describe cmds =
    List.map
      (function
        | Cmd.Start_timer { id; _ } ->
            "start " ^ id
        | Cmd.Stop id ->
            "stop " ^ id
        | Cmd.Run (Cmd.Focus r) ->
            Ref.name r
        | _ ->
            "other"
        )
      cmds
  in
  let snapshot, cmds = Machine.start machine in
  Alcotest.check strings "start"
    [ "enter a"; "start pair/a/after0" ]
    (describe cmds);
  let _, cmds, _ = Machine.send machine snapshot () in
  Alcotest.check strings "transition"
    [ "exit a"; "stop pair/a/after0"; "move"; "enter b"; "start pair/b/after0" ]
    (describe cmds);
  Alcotest.check strings "exit"
    [ "exit a"; "stop pair/a/after0" ]
    (describe (Machine.exit machine snapshot))

let test_self_goto_keeps_resources () =
  let module Counter = struct
    type _ t = Count : int t
  end in
  let module Counter_machine = Machine.Make (Counter) in
  let machine =
    Counter_machine.make ~id:"counter"
      ~init:(Counter_machine.at Counter.Count 0)
      [
        Counter_machine.state Counter.Count "count"
          ~entry:(fun _ -> [ note "enter" ])
          ~exit:(fun _ -> [ note "exit" ])
          ~after:[ (10, `Tick) ]
          (fun n -> function
            | `Tick ->
                Counter_machine.goto Counter.Count (n + 1)
            | `Again ->
                Counter_machine.reenter Counter.Count (n + 1)
            );
      ]
  in
  let snapshot, _ = Machine.start machine in
  let snapshot, cmds, _ = Machine.send machine snapshot `Tick in
  Alcotest.(check int) "goto: no commands" 0 (List.length cmds);
  let _, cmds, _ = Machine.send machine snapshot `Again in
  Alcotest.(check int) "reenter: exit, stop, enter, start" 4 (List.length cmds)

let declared_error f =
  match f () with
  | exception Machine.Undeclared_target { machine; from; target } ->
      (machine, from, target)
  | _ ->
      Alcotest.fail "expected Undeclared_target"

let test_undeclared_target () =
  let module Pair = struct
    type _ t = A : unit t | B : unit t | C : unit t
  end in
  let module Pair_machine = Machine.Make (Pair) in
  let machine =
    Pair_machine.make ~id:"bad"
      ~init:(Pair_machine.at Pair.A ())
      [
        Pair_machine.always ~to_:[ Pair.A ] (function
          | `To_c ->
              Pair_machine.goto Pair.C ()
          | _ ->
              Pair_machine.unhandled
          );
        Pair_machine.state Pair.A "a" ~to_:[ Pair.B ] (fun () -> function
          | `To_b ->
              Pair_machine.goto Pair.B ()
          | `To_c_here ->
              Pair_machine.goto Pair.C ()
          | `Self ->
              Pair_machine.goto Pair.A ()
          | `Again ->
              Pair_machine.reenter Pair.A ()
          | _ ->
              Pair_machine.unhandled
          );
        Pair_machine.state Pair.B "b" Pair_machine.ignore;
        Pair_machine.state Pair.C "c" Pair_machine.ignore;
      ]
  in
  let snapshot, _ = Machine.start machine in
  let ok event = ignore (Machine.send machine snapshot event) in
  ok `To_b;
  ok `Self;
  ok `Again;
  Alcotest.(check (triple string string string))
    "state handler" ("bad", "a", "c")
    (declared_error (fun () -> Machine.send machine snapshot `To_c_here));
  Alcotest.(check (triple string string string))
    "always handler" ("bad", "a", "c")
    (declared_error (fun () -> Machine.send machine snapshot `To_c))

let test_missing_case () =
  let module Pair = struct
    type _ t = A : unit t | B : unit t | Orphan : unit t
  end in
  let module Pair_machine = Machine.Make (Pair) in
  let table =
    [
      Pair_machine.state Pair.A "a" ~to_:[ Pair.B ] (fun () () ->
          Pair_machine.goto Pair.B ()
      );
      Pair_machine.state Pair.B "b" Pair_machine.ignore;
    ]
  in
  Alcotest.check_raises "initial state"
    (Machine.Missing_case { machine = "m"; index = 2 })
    (fun () ->
      ignore
        (Pair_machine.make ~id:"m" ~init:(Pair_machine.at Pair.Orphan ()) table)
    );
  let machine =
    Pair_machine.make ~id:"m" ~init:(Pair_machine.at Pair.A ()) table
  in
  Pair_machine.cases_cover machine [ Pair.A; Pair.B ];
  Alcotest.check_raises "cases_cover"
    (Machine.Missing_case { machine = "m"; index = 2 })
    (fun () -> Pair_machine.cases_cover machine [ Pair.A; Pair.Orphan ]);
  Alcotest.check_raises "to_ lists a state with no case"
    (Machine.Missing_case { machine = "m"; index = 2 })
    (fun () ->
      ignore
        (Pair_machine.make ~id:"m"
           ~init:(Pair_machine.at Pair.A ())
           [
             Pair_machine.state Pair.A "a" ~to_:[ Pair.Orphan ]
               Pair_machine.ignore;
           ]
        )
    );
  let wild =
    Pair_machine.make ~id:"m"
      ~init:(Pair_machine.at Pair.A ())
      [
        Pair_machine.state Pair.A "a" (fun () () ->
            Pair_machine.goto Pair.Orphan ()
        );
        Pair_machine.state Pair.B "b" Pair_machine.ignore;
      ]
  in
  let snapshot, _ = Machine.start wild in
  match Machine.send wild snapshot () with
  | exception Machine.Undeclared_target { target; _ } ->
      Alcotest.(check string) "goto to a state with no case" "#2" target
  | _ ->
      Alcotest.fail "expected Undeclared_target"

let test_search_covers_all_states () =
  Search.cases_cover search [ Idle; Typing; Searching; Shown; Failed ]

let test_final_ignores_events () =
  let module Gate = struct
    type _ t = Open : unit t | Closed : unit t
  end in
  let module Gate_machine = Machine.Make (Gate) in
  let machine =
    Gate_machine.make ~id:"gate"
      ~init:(Gate_machine.at Gate.Open ())
      [
        Gate_machine.always ~to_:[ Gate.Open ] (function
          | `Reopen ->
              Gate_machine.goto Gate.Open ()
          | _ ->
              Gate_machine.unhandled
          );
        Gate_machine.state Gate.Open "open" ~to_:[ Gate.Closed ]
          (fun () -> function
          | `Close ->
              Gate_machine.goto Gate.Closed ()
          | _ ->
              Gate_machine.unhandled
          );
        Gate_machine.state Gate.Closed "closed" ~final:true ~to_:[ Gate.Open ]
          (fun () -> function
          | `Close ->
              Gate_machine.stay [ note "ignored" ]
          | _ ->
              Gate_machine.unhandled
          );
      ]
  in
  let snapshot, _ = Machine.start machine in
  let closed, _, _ = Machine.send machine snapshot `Close in
  Alcotest.(check bool) "final" true (Machine.is_final machine closed);
  List.iter
    (fun event ->
      let next, cmds, handled = Machine.send machine closed event in
      Alcotest.(check bool) "unhandled" true (handled = `Unhandled);
      Alcotest.(check int) "no commands" 0 (List.length cmds);
      Alcotest.(check bool) "same" true (next == closed)
    )
    [ `Close; `Reopen ];
  let _, _, handled = Machine.send machine snapshot `Reopen in
  Alcotest.(check bool) "always works while not final" true (handled = `Handled);
  let spec = Machine.spec machine in
  Alcotest.(check bool)
    "no edge leaves a final state" true
    (not (List.exists (fun (a, _) -> a = "closed") spec.edges))

let test_always_order () =
  let module One = struct
    type _ t = Only : unit t
  end in
  let module One_machine = Machine.Make (One) in
  let machine =
    One_machine.make ~id:"one"
      ~init:(One_machine.at One.Only ())
      [
        One_machine.always (function
          | `Both ->
              One_machine.stay [ note "first always" ]
          | _ ->
              One_machine.unhandled
          );
        One_machine.always (function
          | `Both | `Second ->
              One_machine.stay [ note "second always" ]
          | _ ->
              One_machine.unhandled
          );
        One_machine.state One.Only "only" (fun () -> function
          | `Own | `Both ->
              One_machine.stay [ note "own" ]
          | _ ->
              One_machine.unhandled
          );
      ]
  in
  let snapshot, _ = Machine.start machine in
  let names event =
    let _, cmds, _ = Machine.send machine snapshot event in
    List.map (function Cmd.Run (Cmd.Focus r) -> Ref.name r | _ -> "?") cmds
  in
  Alcotest.check strings "state first" [ "own" ] (names `Both);
  Alcotest.check strings "always in list order" [ "second always" ]
    (names `Second);
  Alcotest.check strings "nothing" [] (names `Other)

let test_spec () =
  let spec = Machine.spec search in
  Alcotest.(check string) "id" "search" spec.id;
  Alcotest.(check string) "init" "idle" spec.init;
  Alcotest.(check (list string))
    "states"
    [ "idle"; "typing"; "searching"; "shown"; "failed" ]
    (List.map (fun (s : Spec.state) -> s.name) spec.states);
  let typing =
    List.find (fun (s : Spec.state) -> s.name = "typing") spec.states
  in
  Alcotest.(check (list int)) "timers" [ 300 ] typing.timers;
  let searching =
    List.find (fun (s : Spec.state) -> s.name = "searching") spec.states
  in
  Alcotest.(check (list string)) "sources" [ "/api/search" ] searching.sources;
  let has edge = List.mem edge spec.edges in
  Alcotest.(check bool) "to_" true (has ("typing", "searching"));
  Alcotest.(check bool)
    "always reaches every state" true
    (List.for_all
       (fun name -> has (name, "typing") && has (name, "idle"))
       [ "idle"; "typing"; "searching"; "shown"; "failed" ]
    );
  Alcotest.(check bool) "retry" true (has ("failed", "searching"));
  Alcotest.(check bool)
    "no edge searching to retry target" false
    (has ("shown", "searching"));
  Alcotest.(check (list (pair string string)))
    "sorted and unique"
    (List.sort_uniq compare spec.edges)
    spec.edges

let test_spec_self_edges () =
  let module Pair = struct
    type _ t =
      | Plain : unit t
      | Timed : unit t
      | Hooked : unit t
      | Next : unit t
  end in
  let module Pair_machine = Machine.Make (Pair) in
  let machine =
    Pair_machine.make ~id:"self"
      ~init:(Pair_machine.at Pair.Plain ())
      [
        Pair_machine.state Pair.Plain "plain"
          ~to_:[ Pair.Timed; Pair.Hooked; Pair.Next ] (fun () () ->
            Pair_machine.goto Pair.Plain ()
        );
        Pair_machine.state Pair.Timed "timed"
          ~after:[ (1, ()) ]
          ~to_:[ Pair.Next ]
          (fun () () -> Pair_machine.goto Pair.Timed ());
        Pair_machine.state Pair.Hooked "hooked"
          ~exit:(fun () -> [])
          ~to_:[ Pair.Next ] Pair_machine.ignore;
        Pair_machine.state Pair.Next "next" ~final:true Pair_machine.ignore;
      ]
  in
  let edges = (Machine.spec machine).edges in
  Alcotest.(check bool)
    "goto to self adds no edge" false
    (List.mem ("plain", "plain") edges);
  Alcotest.(check bool)
    "timer can restart" true
    (List.mem ("timed", "timed") edges);
  Alcotest.(check bool)
    "exit can run again" true
    (List.mem ("hooked", "hooked") edges);
  Alcotest.(check bool) "final has none" false (List.mem ("next", "next") edges)

let test_lint () =
  Alcotest.(check (list string))
    "search is clean" []
    (List.map Spec.issue_to_string (Spec.lint (Machine.spec search)));
  let module Bad = struct
    type _ t =
      | Start : unit t
      | Island : unit t
      | Sink : unit t
      | Timed : unit t
      | Done : unit t
  end in
  let module Bad_machine = Machine.Make (Bad) in
  let machine =
    Bad_machine.make ~id:"bad"
      ~init:(Bad_machine.at Bad.Start ())
      [
        Bad_machine.state Bad.Start "start" ~to_:[ Bad.Sink; Bad.Timed ]
          Bad_machine.ignore;
        Bad_machine.state Bad.Island "island" ~to_:[ Bad.Done ]
          Bad_machine.ignore;
        Bad_machine.state Bad.Sink "sink" Bad_machine.ignore;
        Bad_machine.state Bad.Timed "timed"
          ~after:[ (1, ()) ]
          Bad_machine.ignore;
        Bad_machine.state Bad.Done "done" ~final:true Bad_machine.ignore;
      ]
  in
  Alcotest.(check (list string))
    "issues"
    [
      "bad.island: unreachable";
      "bad.sink: non-final dead end";
      "bad.timed: non-final dead end";
      "bad.done: unreachable";
    ]
    (List.map Spec.issue_to_string (Spec.lint (Machine.spec machine)))

let test_dot () =
  let expected =
    {|digraph "search" {
rankdir=LR;
"search.__init" [shape=point];
"search.__init" -> "search.idle";
"search.idle" [label="idle" shape=box];
"search.typing" [label="typing\nafter 300ms" shape=box];
"search.searching" [label="searching\ninvoke /api/search" shape=box];
"search.shown" [label="shown" shape=box];
"search.failed" [label="failed" shape=box];
"search.failed" -> "search.idle";
"search.failed" -> "search.searching";
"search.failed" -> "search.typing";
"search.idle" -> "search.idle";
"search.idle" -> "search.typing";
"search.searching" -> "search.failed";
"search.searching" -> "search.idle";
"search.searching" -> "search.searching";
"search.searching" -> "search.shown";
"search.searching" -> "search.typing";
"search.shown" -> "search.idle";
"search.shown" -> "search.typing";
"search.typing" -> "search.idle";
"search.typing" -> "search.searching";
"search.typing" -> "search.typing";
}
|}
  in
  Alcotest.(check string) "dot" expected (Spec.to_dot (Machine.spec search))

let test_map_snapshot () =
  let doubled =
    Machine.map_snapshot search ~get:(fun snapshot -> (snapshot, 1)) ~set:fst
  in
  let snapshot, cmds = Machine.start doubled in
  Alcotest.(check int) "view" 1 (snd snapshot);
  Alcotest.(check int) "no commands" 0 (List.length cmds);
  let next, cmds, _ = Machine.send doubled snapshot (Input "a") in
  Alcotest.(check string) "key" "typing" (Machine.key doubled next);
  Alcotest.check strings "commands"
    [ "start search/typing/after0 300ms" ]
    (described cmds);
  Alcotest.(check bool)
    "spec shared" true
    (Machine.spec doubled == Machine.spec search)

let test_remote_machine () =
  let ep =
    Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
  in
  let machine = Remote_machine.make ~id:"r" ep in
  let snapshot, cmds = Machine.start machine in
  let status (state : (int, int) Remote_machine.state) = state.status in
  Alcotest.(check bool) "idle" true (status snapshot = Status.Idle);
  Alcotest.(check int) "no entry" 0 (List.length cmds);
  let loading, cmds, _ =
    Machine.send machine snapshot (Remote_machine.Load 4)
  in
  Alcotest.(check bool) "loading" true (status loading = Status.Loading None);
  let first_id =
    match cmds with
    | [ Cmd.Start_call { id; endpoint; input; on_result; timeout_ms = None } ]
      -> (
        Alcotest.(check string) "path" "/api/double" (Remote.path endpoint);
        Alcotest.(check string)
          "input" "4"
          (Codec.to_string (Remote.input endpoint) input);
        match on_result (Error (Network "x")) with
        | Remote_machine.Settled (Error (Network "x")) ->
            id
        | _ ->
            Alcotest.fail "continuation must build Settled"
      )
    | _ ->
        Alcotest.fail "expected one Start_call"
  in
  let loaded, cmds, _ =
    Machine.send machine loading (Remote_machine.Settled (Ok 8))
  in
  Alcotest.(check bool)
    "ready" true
    (status loaded = Status.Ready { data = 8; stale = false });
  Alcotest.check strings "stops the call" [ "stop " ^ first_id ] (described cmds);
  let failed, _, _ =
    Machine.send machine loading (Remote_machine.Settled (Error (Http 500)))
  in
  Alcotest.(check bool)
    "failed" true
    (status failed = Status.Failed { error = Http 500; last = None });
  let _, _, handled =
    Machine.send machine snapshot (Remote_machine.Settled (Ok 1))
  in
  Alcotest.(check bool) "result without a call" true (handled = `Unhandled);
  let reset, _, _ = Machine.send machine loaded Remote_machine.Reset in
  Alcotest.(check bool) "reset" true (status reset = Status.Idle)

let test_call_timeout () =
  let module Slow = struct
    type _ t = Waiting : unit t | Done : unit t
  end in
  let module Slow_machine = Machine.Make (Slow) in
  let ep =
    Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
  in
  let machine =
    Slow_machine.make ~id:"slow"
      ~init:(Slow_machine.at Waiting ())
      [
        Slow_machine.state Waiting "waiting" ~to_:[ Done ]
          ~invoke:
            [
              Source.call ~timeout_ms:250 ep (fun () -> 1) (fun r -> r);
              Source.call ep (fun () -> 2) (fun r -> r);
            ]
          (fun () -> function Ok _ | Error _ -> Slow_machine.goto Done ());
        Slow_machine.state Done "done" Slow_machine.ignore;
      ]
  in
  let _, cmds = Machine.start machine in
  let timeouts =
    List.map
      (function
        | Cmd.Start_call { timeout_ms; _ } ->
            timeout_ms
        | _ ->
            Alcotest.fail "expected Start_call"
        )
      cmds
  in
  Alcotest.(check (list (option int)))
    "timeout only where asked" [ Some 250; None ] timeouts

let test_remote_machine_reload () =
  let ep =
    Remote.declare ~path:"/api/double" ~input:Codec.int ~output:Codec.int
  in
  let machine = Remote_machine.make ~id:"r" ep in
  let snapshot, _ = Machine.start machine in
  let snapshot, _, _ = Machine.send machine snapshot (Remote_machine.Load 1) in
  let snapshot, cmds, _ =
    Machine.send machine snapshot (Remote_machine.Load 2)
  in
  Alcotest.(check bool)
    "newest input" true
    (snapshot.Remote_machine.input = Some 2);
  Alcotest.check strings "old call stops, new call starts"
    [ "stop r/loading/invoke0"; "call r/loading/invoke0 /api/double" ]
    (described cmds);
  let _, cmds, _ = Machine.send machine snapshot Reset in
  Alcotest.check strings "reset stops"
    [ "stop r/loading/invoke0" ]
    (described cmds)

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
          t "active follows the commands" test_active_follows_commands;
          t "entry and exit order" test_entry_exit_order;
          t "self goto keeps resources" test_self_goto_keeps_resources;
          t "undeclared target" test_undeclared_target;
          t "missing case" test_missing_case;
          t "cases_cover on a full table" test_search_covers_all_states;
          t "final ignores events" test_final_ignores_events;
          t "always order" test_always_order;
          t "map_snapshot" test_map_snapshot;
        ]
      );
      ( "spec",
        [
          t "shape" test_spec;
          t "self edges" test_spec_self_edges;
          t "lint" test_lint;
          t "dot" test_dot;
        ]
      );
      ( "remote machine",
        [
          t "states" test_remote_machine;
          t "reload restarts the call" test_remote_machine_reload;
          t "call timeout is carried to the host" test_call_timeout;
        ]
      );
    ]
