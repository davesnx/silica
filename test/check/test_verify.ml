open Silica

let codec_int =
  Codec.make
    ~encode:(fun n -> Silica_json.Json.declassify (`Int n))
    ~decode:(fun _ -> Ok 0)

let codec_unit =
  Codec.make
    ~encode:(fun () -> Silica_json.Json.declassify `Null)
    ~decode:(fun _ -> Ok ())

let endpoint = Remote.declare ~path:"/api/n" ~input:codec_unit ~output:codec_int
let other =
  Remote.declare ~path:"/api/other" ~input:codec_unit ~output:codec_int
let topic = Topic.declare ~name:"numbers" ~state:codec_int

let show_error : Remote.error -> string = function
  | Http code ->
      Printf.sprintf "http %d" code
  | Network m ->
      "network " ^ m
  | Decode m ->
      "decode " ^ m

let show_status : int Status.t -> string = function
  | Idle ->
      "idle"
  | Loading None ->
      "loading"
  | Loading (Some n) ->
      Printf.sprintf "loading %d" n
  | Ready { data; stale } ->
      Printf.sprintf "%s %d"
        ( if stale then
            "stale"
          else
            "ready"
        )
        data
  | Failed { error; last } ->
      Printf.sprintf "failed %s%s" (show_error error)
        (match last with Some n -> Printf.sprintf " last %d" n | None -> "")

let report_to_string show report =
  Format.asprintf "%a"
    (fun f r ->
      Check.pp_report
        ~pp_event:(fun f e -> Format.pp_print_string f (show e))
        f r
    )
    report

let expect_violation show name trace report =
  match report with
  | Check.Violation v ->
      Alcotest.(check string) "invariant" name v.invariant;
      Alcotest.(check (list string)) "trace" trace (List.map show v.trace)
  | r ->
      Alcotest.failf "expected violation %s, got %s" name
        (report_to_string show r)

(* Remote answers *)

type load = Loaded of (int, Remote.error) result | Retry

let show_load = function
  | Loaded (Ok n) ->
      Printf.sprintf "loaded %d" n
  | Loaded (Error e) ->
      "loaded " ^ show_error e
  | Retry ->
      "retry"

module Load = struct
  type _ t = Loading : unit t | Ready : int t | Failed : int t
end

module Load_machine = Machine.Make (Load)

let loader ?(network = true) () =
  Load_machine.make ~id:"loader"
    ~init:(Load_machine.at Loading ())
    [
      Load_machine.state Loading "loading" ~to_:[ Ready; Failed ]
        ~invoke:[ Source.call endpoint Fun.id (fun r -> Loaded r) ]
        (fun () -> function
          | Loaded (Ok n) ->
              Load_machine.goto Ready n
          | Loaded (Error (Http code)) ->
              Load_machine.goto Failed code
          | Loaded (Error (Network _)) when network ->
              Load_machine.goto Failed 0
          | Loaded (Error _) | Retry ->
              Load_machine.unhandled
          );
      Load_machine.state Ready "ready" ~final:true Load_machine.ignore;
      Load_machine.state Failed "failed" ~to_:[ Loading ] (fun _ -> function
        | Retry ->
            Load_machine.goto Loading ()
        | Loaded _ ->
            Load_machine.unhandled
        );
    ]

let loader_view (Load_machine.At (state, _) : Load_machine.snapshot) =
  match state with
  | Load.Failed ->
      Node.element "button" [ Attr.on_press Retry ] [ Node.text "retry" ]
  | Load.Loading | Load.Ready ->
      Node.text "x"

let test_answers_are_delivered () =
  let c = Component.make (loader ()) loader_view in
  let readies : int list ref = ref [] in
  let seen =
    {
      Check.name = "record";
      holds =
        (fun (Load_machine.At (state, data) : Load_machine.snapshot) _ ->
          ( match state with
          | Load.Ready ->
              readies := data :: !readies
          | _ ->
              ()
          );
          true
        );
    }
  in
  match
    Check.component
      ~samples:[ Check.answers endpoint [ 1; 2 ] ]
      ~invariants:[ seen ] c
  with
  | Check.Ok { states; _ } ->
      Alcotest.(check (list int))
        "each sample reaches ready" [ 1; 2 ]
        (List.sort compare !readies);
      Alcotest.(check int)
        "loading, ready 1, ready 2, failed 500, failed 0" 5 states
  | r ->
      Alcotest.fail (report_to_string show_load r)

let test_unhandled_network_error_is_a_dead_end () =
  let c = Component.make (loader ~network:false ()) loader_view in
  expect_violation show_load "dead-end" [ "loaded network down" ]
    (Check.component ~samples:[ Check.answers endpoint [ 1 ] ] c)

let test_decode_only_on_request () =
  let errors : int list ref = ref [] in
  let record =
    {
      Check.name = "record";
      holds =
        (fun (Load_machine.At (state, data) : Load_machine.snapshot) _ ->
          (match state with Load.Failed -> errors := data :: !errors | _ -> ());
          true
        );
    }
  in
  let decode_dead =
    Load_machine.make ~id:"loader"
      ~init:(Load_machine.at Loading ())
      [
        Load_machine.state Loading "loading" ~to_:[ Ready; Failed ]
          ~invoke:[ Source.call endpoint Fun.id (fun r -> Loaded r) ]
          (fun () -> function
            | Loaded (Ok n) ->
                Load_machine.goto Ready n
            | Loaded (Error (Http code)) ->
                Load_machine.goto Failed code
            | Loaded (Error (Network _)) ->
                Load_machine.goto Failed 0
            | Loaded (Error (Decode _)) | Retry ->
                Load_machine.unhandled
            );
        Load_machine.state Ready "ready" ~final:true Load_machine.ignore;
        Load_machine.state Failed "failed" ~to_:[ Loading ] (fun _ -> function
          | Retry ->
              Load_machine.goto Loading ()
          | Loaded _ ->
              Load_machine.unhandled
          );
      ]
  in
  let c = Component.make decode_dead loader_view in
  ( match
      Check.component
        ~samples:[ Check.answers endpoint [] ]
        ~invariants:[ record ] c
    with
  | Check.Unreached { edges } ->
      Alcotest.(check (list (pair string string)))
        "no ok answer, ready never reached"
        [ ("loading", "ready") ]
        edges;
      Alcotest.(check (list int))
        "only the two default errors" [ 0; 500 ]
        (List.sort_uniq compare !errors)
  | r ->
      Alcotest.fail (report_to_string show_load r)
  );
  expect_violation show_load "dead-end" [ "loaded decode sample" ]
    (Check.component ~samples:[ Check.answers ~decode:true endpoint [ 1 ] ] c)

let test_answers_of_another_endpoint_are_not_used () =
  let c = Component.make (loader ()) loader_view in
  match Check.component ~samples:[ Check.answers other [ 7 ] ] c with
  | Check.Unreached { edges } ->
      Alcotest.(check (list (pair string string)))
        "no ok answer for /api/n"
        [ ("loading", "ready") ]
        edges
  | r ->
      Alcotest.fail (report_to_string show_load r)

let test_one_answer_per_call () =
  let answers = ref 0 in
  let counting =
    Load_machine.make ~id:"loader"
      ~init:(Load_machine.at Loading ())
      [
        Load_machine.state Loading "loading" ~to_:[ Ready ]
          ~invoke:[ Source.call endpoint Fun.id (fun r -> Loaded r) ]
          (fun () -> function
            | Loaded _ ->
                incr answers;
                Load_machine.stay []
            | Retry ->
                Load_machine.goto Ready 0
            );
        Load_machine.state Ready "ready" ~final:true Load_machine.ignore;
      ]
  in
  let view _ =
    Node.element "button" [ Attr.on_press Retry ] [ Node.text "go" ]
  in
  match
    Check.component
      ~samples:[ Check.answers endpoint [ 1 ] ]
      (Component.make counting view)
  with
  | Check.Ok { states; transitions; edges; _ } ->
      Alcotest.(check int)
        "loading with the call, loading after the answer, ready" 3 states;
      Alcotest.(check int)
        "four from the first state, one from each other" 6 transitions;
      Alcotest.(check int) "three answers handled" 3 !answers;
      Alcotest.(check (list (pair string bool)))
        "the answers kept the state, the retry entered ready"
        [ ("loading", true); ("ready", false) ]
        (List.map (fun (l : Spec.label) -> (l.target, l.kept)) edges)
  | r ->
      Alcotest.fail (report_to_string show_load r)

module Tick = struct
  type _ t = Waiting : int t
end

module Tick_machine = Machine.Make (Tick)

let test_a_timer_fires_once () =
  let ticker =
    Tick_machine.make ~id:"ticker"
      ~init:(Tick_machine.at Waiting 0)
      [
        Tick_machine.state Waiting "waiting"
          ~after:[ (100, Retry) ]
          (fun count -> function
            | Retry ->
                Tick_machine.goto Waiting (count + 1)
            | Loaded _ ->
                Tick_machine.unhandled
            );
      ]
  in
  let view _ =
    Node.element "button" [ Attr.on_press (Loaded (Ok 0)) ] [ Node.text "x" ]
  in
  let once =
    {
      Check.name = "at most one tick";
      holds =
        (fun (Tick_machine.At (Waiting, count) : Tick_machine.snapshot) _ ->
          count <= 1
        );
    }
  in
  match Check.component ~invariants:[ once ] (Component.make ticker view) with
  | Check.Ok { states; _ } ->
      Alcotest.(check int) "before and after the tick" 2 states
  | r ->
      Alcotest.fail (report_to_string show_load r)

(* Live states *)

type watch = Update of int Topic.update | Leave

let show_watch = function
  | Update { seq; status } ->
      Printf.sprintf "%d %s" seq (show_status status)
  | Leave ->
      "leave"

module Watch = struct
  type _ t = Watching : int t | Broken : unit t | Away : unit t
end

module Watch_machine = Machine.Make (Watch)

let watcher ~on_stale =
  Watch_machine.make ~id:"watcher"
    ~init:(Watch_machine.at Watching 0)
    [
      Watch_machine.state Watching "watching" ~to_:[ Broken; Away ]
        ~invoke:[ Source.live topic (fun _ -> "all") (fun u -> Update u) ]
        (fun count -> function
          | Update { status = Ready { stale = true; _ }; _ } ->
              on_stale count
          | Update { status = Ready { stale = false; _ }; _ } ->
              Watch_machine.goto Watching (count + 1)
          | Update _ ->
              Watch_machine.stay []
          | Leave ->
              Watch_machine.goto Away ()
          );
      Watch_machine.state Broken "broken" ~final:true Watch_machine.ignore;
      Watch_machine.state Away "away" ~final:true Watch_machine.ignore;
    ]

let watcher_view (Watch_machine.At (state, _) : Watch_machine.snapshot) =
  match state with
  | Watch.Watching ->
      Node.element "button" [ Attr.on_press Leave ] [ Node.text "leave" ]
  | Watch.Broken | Watch.Away ->
      Node.text "x"

let not_broken =
  {
    Check.name = "not broken";
    holds =
      (fun (Watch_machine.At (state, _) : Watch_machine.snapshot) _ ->
        match state with Watch.Broken -> false | _ -> true
      );
  }

let test_stale_update_is_delivered () =
  let c =
    Component.make
      (watcher ~on_stale:(fun _ -> Watch_machine.goto Broken ()))
      watcher_view
  in
  expect_violation show_watch "not broken"
    [ "0 loading"; "1 ready 4"; "0 stale 4" ]
    (Check.component
       ~samples:[ Check.states topic [ 4; 5 ] ]
       ~invariants:[ not_broken ] c
    )

let below n =
  {
    Check.name = Printf.sprintf "fewer than %d states" n;
    holds =
      (fun (Watch_machine.At (state, count) : Watch_machine.snapshot) _ ->
        match state with Watch.Watching -> count < n | _ -> true
      );
  }

let ok_watcher =
  watcher ~on_stale:(fun count -> Watch_machine.goto Watching count)

let test_arrivals_are_bounded () =
  let c = Component.make ok_watcher watcher_view in
  ( match
      Check.component ~arrivals:1
        ~samples:[ Check.states topic [ 4; 5 ] ]
        ~invariants:[ below 3 ]
        ~allow_unreached:[ ("watching", "broken") ]
        c
    with
  | Check.Ok _ ->
      ()
  | r ->
      Alcotest.fail (report_to_string show_watch r)
  );
  expect_violation show_watch "fewer than 2 states"
    [ "0 loading"; "1 ready 4"; "0 stale 4"; "2 ready 4" ]
    (Check.component ~arrivals:1
       ~samples:[ Check.states topic [ 4; 5 ] ]
       ~invariants:[ below 2 ]
       c
    );
  expect_violation show_watch "fewer than 3 states"
    [ "0 loading"; "1 ready 4"; "2 ready 4"; "0 stale 4"; "3 ready 4" ]
    (Check.component
       ~samples:[ Check.states topic [ 4; 5 ] ]
       ~invariants:[ below 3 ]
       c
    )

let test_failure_ends_the_subscription () =
  let failed : int option list ref = ref [] in
  let after_failure =
    Watch_machine.make ~id:"watcher"
      ~init:(Watch_machine.at Watching 0)
      [
        Watch_machine.state Watching "watching" ~to_:[ Away ]
          ~invoke:[ Source.live topic (fun _ -> "all") (fun u -> Update u) ]
          (fun count -> function
            | Update { status = Failed { last; _ }; _ } ->
                failed := last :: !failed;
                Watch_machine.goto Watching (count + 100)
            | Update { status = Ready { stale = false; _ }; _ }
              when count >= 100 ->
                Watch_machine.goto Watching (count + 1000)
            | Update { status = Ready { stale = false; _ }; _ } ->
                Watch_machine.goto Watching (count + 1)
            | Update _ ->
                Watch_machine.stay []
            | Leave ->
                Watch_machine.goto Away ()
            );
        Watch_machine.state Away "away" ~final:true Watch_machine.ignore;
      ]
  in
  let nothing_after_failure =
    {
      Check.name = "nothing arrives after a failure";
      holds =
        (fun (Watch_machine.At (state, count) : Watch_machine.snapshot) _ ->
          match state with Watch.Watching -> count < 1000 | _ -> true
        );
    }
  in
  match
    Check.component
      ~samples:[ Check.states topic [ 4 ] ]
      ~invariants:[ nothing_after_failure ]
      (Component.make after_failure watcher_view)
  with
  | Check.Ok _ ->
      Alcotest.(check (list (option int)))
        "a failure carries the last state" [ None; Some 4 ]
        (List.sort_uniq compare !failed)
  | r ->
      Alcotest.fail (report_to_string show_watch r)

(* Ownership and cases: machines that break the rules on purpose, through a
   [map_snapshot] whose [set] does not give back the snapshot that [get] saw. *)

type go = Go | Wait

let show_go = function Go -> "go" | Wait -> "wait"

module Three = struct
  type _ t = A : unit t | B : unit t | C : unit t
end

module Three_machine = Machine.Make (Three)

let wrong ~b ~c =
  let table =
    Three_machine.make ~id:"wrong" ~init:(Three_machine.at A ())
      [
        Three_machine.state A "a" ~to_:[ B ] (fun () -> function
          | Go ->
              Three_machine.goto B ()
          | Wait ->
              Three_machine.unhandled
          );
        b;
        c;
      ]
  in
  Machine.map_snapshot table
    ~get:(fun (Three_machine.At (state, _) : Three_machine.snapshot) ->
      match state with Three.A -> 0 | Three.B -> 1 | Three.C -> 2
    )
    ~set:(function 0 -> Three_machine.at A () | _ -> Three_machine.at C ())

let idle_c =
  Three_machine.state C "c" ~to_:[ A ] (fun () -> function
    | Wait ->
        Three_machine.goto A ()
    | Go ->
        Three_machine.unhandled
    )

let go_view _ =
  Node.list
    [
      Node.element "button" [ Attr.on_press Go ] [ Node.text "go" ];
      Node.element "button" [ Attr.on_press Wait ] [ Node.text "wait" ];
    ]

let test_timer_outside_its_state () =
  let b =
    Three_machine.state B "b"
      ~after:[ (100, Wait) ]
      ~to_:[ A ]
      (fun () -> function
        | Wait -> Three_machine.goto A () | Go -> Three_machine.unhandled
        )
  in
  expect_violation show_go "timer-owner" [ "go" ]
    (Check.component (Component.make (wrong ~b ~c:idle_c) go_view))

let test_call_outside_its_state () =
  let b =
    Three_machine.state B "b"
      ~invoke:[ Source.call endpoint Fun.id (fun _ -> Wait) ]
      ~to_:[ A ]
      (fun () -> function
        | Wait -> Three_machine.goto A () | Go -> Three_machine.unhandled
        )
  in
  expect_violation show_go "call-owner" [ "go" ]
    (Check.component (Component.make (wrong ~b ~c:idle_c) go_view))

let test_subscription_outside_its_state () =
  let b =
    Three_machine.state B "b"
      ~invoke:[ Source.live topic (fun _ -> "all") (fun _ -> Wait) ]
      ~to_:[ A ]
      (fun () -> function
        | Wait -> Three_machine.goto A () | Go -> Three_machine.unhandled
        )
  in
  expect_violation show_go "call-owner" [ "go" ]
    (Check.component (Component.make (wrong ~b ~c:idle_c) go_view))

module Two = struct
  type _ t = A : unit t | B : unit t | C : unit t
end

module Two_machine = Machine.Make (Two)

let test_missing_case_at_run_time () =
  let table =
    Two_machine.make ~id:"uncovered" ~init:(Two_machine.at A ())
      [
        Two_machine.state A "a" ~to_:[ B ] (fun () -> function
          | Go ->
              Two_machine.goto B ()
          | Wait ->
              Two_machine.unhandled
          );
        Two_machine.state B "b" ~to_:[ A ] (fun () -> function
          | Wait ->
              Two_machine.goto A ()
          | Go ->
              Two_machine.unhandled
          );
      ]
  in
  let machine =
    Machine.map_snapshot table
      ~get:(fun (Two_machine.At (state, _) : Two_machine.snapshot) ->
        match state with Two.A -> 0 | Two.B -> 1 | Two.C -> 2
      )
      ~set:(function 0 -> Two_machine.at A () | _ -> Two_machine.at C ())
  in
  expect_violation show_go "missing-case" [ "go" ]
    (Check.component (Component.make machine go_view))

let app_trace show report =
  match report with
  | Check.Violation v ->
      ( v.invariant,
        List.map (Format.asprintf "%a" (Check.pp_app_event show)) v.trace
      )
  | r ->
      ( Format.asprintf "%a"
          (fun f r -> Check.pp_report ~pp_event:(Check.pp_app_event show) f r)
          r,
        []
      )

let pp_of show f e = Format.pp_print_string f (show e)

let test_app_root_gets_answers () =
  let machine = loader () in
  let root _ = Component.make machine loader_view in
  let start, _ = Machine.start machine in
  match
    Check.app ~label:show_load
      ~samples:[ Check.answers endpoint [ 1 ] ]
      ~routes:[ start ]
      ~parse:(fun _ -> start)
      ~print:(fun _ -> Url.of_string "/")
      ~url_event:(fun _ -> Retry)
      root
  with
  | Check.Ok { edges; _ } ->
      Alcotest.(check bool)
        "the answer of the root call is delivered" true
        (List.exists
           (fun (l : Spec.label) -> l.target = "ready" && l.event = "loaded 1")
           edges
        )
  | r ->
      Alcotest.fail (fst (app_trace (pp_of show_load) r))

let test_app_root_timer_owner () =
  let b =
    Three_machine.state B "b"
      ~after:[ (100, Wait) ]
      ~to_:[ A ]
      (fun () -> function
        | Wait -> Three_machine.goto A () | Go -> Three_machine.unhandled
        )
  in
  let machine = wrong ~b ~c:idle_c in
  Alcotest.(check (pair string (list string)))
    "the root machine is checked for ownership" ("timer-owner", [ "go" ])
    (app_trace (pp_of show_go)
       (Check.app ~routes:[ 0 ]
          ~parse:(fun _ -> 0)
          ~print:(fun _ -> Url.of_string "/")
          ~url_event:(fun _ -> Wait)
          (fun _ -> Component.make machine go_view)
       )
    )

(* Spec drift *)

module Drift = struct
  type _ t = Start : unit t | Done : unit t | Never : unit t
end

module Drift_machine = Machine.Make (Drift)

let drifting =
  Drift_machine.make ~id:"drift"
    ~init:(Drift_machine.at Start ())
    [
      Drift_machine.state Start "start" ~to_:[ Done; Never ] (fun () -> function
        | Go ->
            Drift_machine.goto Done ()
        | Wait ->
            Drift_machine.unhandled
        );
      Drift_machine.state Done "done" ~final:true Drift_machine.ignore;
      Drift_machine.state Never "never" ~final:true Drift_machine.ignore;
    ]

let test_unreached_edge () =
  let c = Component.make drifting go_view in
  ( match Check.component c with
  | Check.Unreached { edges } ->
      Alcotest.(check (list (pair string string)))
        "declared, never taken"
        [ ("start", "never") ]
        edges
  | r ->
      Alcotest.fail (report_to_string show_go r)
  );
  match Check.component ~allow_unreached:[ ("start", "never") ] c with
  | Check.Ok _ ->
      ()
  | r ->
      Alcotest.fail (report_to_string show_go r)

let test_edges_are_labelled () =
  let c = Component.make (loader ()) loader_view in
  match
    Check.component ~label:show_load ~samples:[ Check.answers endpoint [ 1 ] ] c
  with
  | Check.Ok { edges; _ } ->
      Alcotest.(check (list (triple string string string)))
        "taken edges with the event names"
        [
          ("failed", "loading", "retry");
          ("loading", "failed", "loaded http 500");
          ("loading", "failed", "loaded network down");
          ("loading", "ready", "loaded 1");
        ]
        (List.map (fun (l : Spec.label) -> (l.from, l.target, l.event)) edges)
  | r ->
      Alcotest.fail (report_to_string show_load r)

(* Props from the parent *)

type child = Set of int | Dec

let show_child = function Set n -> Printf.sprintf "set %d" n | Dec -> "dec"

let child =
  Component.make
    (Silica_test_support.Simple.machine ~id:"child" ~init:0 (fun n -> function
      | Set v ->
          Some (v, [])
      | Dec ->
          Some (n - 1, [])
      ))
    (fun n ->
      if n > 0 then
        Node.element "button" [ Attr.on_press Dec ] [ Node.text "dec" ]
      else
        Node.text "zero"
    )

let parent =
  Component.make
    (Silica_test_support.Simple.machine ~id:"parent" ~init:0 (fun n -> function
      | Go ->
          Some (min 1 (n + 1), [])
      | Wait ->
          None
      ))
    (fun n ->
      Node.list
        [
          Node.element "button" [ Attr.on_press Go ] [ Node.text "go" ];
          Node.component
            ~props:
              ( if n = 0 then
                  Set 1
                else
                  Set 7
              )
            child;
          Node.component ~props:(Set 1) child;
        ]
    )

let below_five = { Check.name = "below five"; holds = (fun n _ -> n < 5) }

let test_props_reach_the_child () =
  match Check.component parent with
  | Check.Ok { children; not_explored; _ } ->
      Alcotest.(check (list string)) "listed" [ "component child" ] not_explored;
      let props = Check.props children child in
      Alcotest.(check (list string))
        "props seen in the parent views" [ "set 1"; "set 7" ]
        (List.sort compare (List.map show_child props));
      Alcotest.(check (list string))
        "no props for the parent itself" []
        (List.map show_go (Check.props children parent));
      expect_violation show_child "below five" [ "set 7" ]
        (Check.component ~props ~invariants:[ below_five ] child)
  | r ->
      Alcotest.fail (report_to_string show_go r)

(* Export *)

let door : Spec.t =
  {
    id = "door";
    init = "closed";
    states =
      [
        { name = "closed"; final = false; timers = []; sources = []; held = [] };
        {
          name = "open";
          final = false;
          timers = [ 300 ];
          sources = [ "/api/x" ];
          held = [ "focus_trap" ];
        };
        { name = "gone"; final = true; timers = []; sources = []; held = [] };
      ];
    edges =
      [
        ("closed", "open");
        ("open", "closed");
        ("open", "gone");
        ("open", "open");
      ];
  }

let door_labels : Spec.label list =
  [
    { from = "closed"; target = "open"; event = "press"; kept = false };
    { from = "open"; target = "closed"; event = "press"; kept = false };
    { from = "open"; target = "closed"; event = "after 300ms"; kept = false };
    { from = "open"; target = "closed"; event = "press"; kept = false };
    { from = "open"; target = "open"; event = "type"; kept = true };
  ]

let test_quint () =
  let expected =
    {|// Generated from the spec of the machine "door". Do not edit.
module door {
  type State = | Closed | Open | Gone
  type Event = OnStart | OnAny | OnAfter300ms | OnPress | OnType

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Closed => Set()
    | Open => Set("open/after 300ms", "open/invoke /api/x", "open/hold focus_trap")
    | Gone => Set()
  }

  pure val finals: Set[State] = Set(Gone)
  pure val movable: Set[State] = Set(Closed, Open)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Closed,
    running' = owned(Closed),
    last' = OnStart,
  }

  action closed_to_open_on_press = all { state == Closed, enter(Open, OnPress) }
  action open_to_closed_on_after_300ms = all { state == Open, enter(Closed, OnAfter300ms) }
  action open_to_closed_on_press = all { state == Open, enter(Closed, OnPress) }
  action open_to_gone_on_any = all { state == Open, enter(Gone, OnAny) }
  action open_to_open_on_any = all { state == Open, enter(Open, OnAny) }
  action open_stays_on_type = all { state == Open, state' = state, running' = running, last' = OnType }

  action step = any {
    closed_to_open_on_press,
    open_to_closed_on_after_300ms,
    open_to_closed_on_press,
    open_to_gone_on_any,
    open_to_open_on_any,
    open_stays_on_type,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
|}
  in
  Alcotest.(check string)
    "quint" expected
    (Spec.to_quint ~labels:door_labels door)

let test_quint_without_edges () =
  let single : Spec.t =
    {
      id = "1-page";
      init = "shown";
      states =
        [
          {
            name = "shown";
            final = false;
            timers = [];
            sources = [];
            held = [];
          };
        ];
      edges = [];
    }
  in
  let text = Spec.to_quint single in
  let contains needle =
    let n = String.length needle and h = String.length text in
    let rec go i = i + n <= h && (String.sub text i n = needle || go (i + 1)) in
    go 0
  in
  Alcotest.(check bool)
    "module name is an identifier" true
    (contains "module m_1_page {");
  Alcotest.(check bool)
    "a machine with no edge stutters" true
    (contains
       "action step = all {\n\
       \    state' = state,\n\
       \    running' = running,\n\
       \    last' = last,\n\
       \  }"
    );
  Alcotest.(check bool)
    "no state can move" true
    (contains "pure val movable: Set[State] = Set()")

let test_quint_state_names () =
  let state name : Spec.state =
    { name; final = false; timers = []; sources = []; held = [] }
  in
  let spec : Spec.t =
    {
      id = "routes";
      init = "/";
      states = [ state "/"; state "/*"; state "a-b"; state "a_b" ];
      edges = [ ("/", "/*"); ("a-b", "a_b") ];
    }
  in
  let lines = String.split_on_char '\n' (Spec.to_quint spec) in
  Alcotest.(check bool)
    "empty and repeated names get the position" true
    (List.mem "  type State = | S1 | S2 | AB | S4AB" lines);
  Alcotest.(check bool)
    "action names come from the constructors" true
    (List.mem "  action s1_to_s2_on_any = all { state == S1, enter(S2, OnAny) }"
       lines
    )

let test_dot_labels () =
  let dot = Spec.to_dot ~labels:door_labels door in
  let lines = String.split_on_char '\n' dot in
  Alcotest.(check bool)
    "events of an edge, sorted, once each" true
    (List.mem {|"door.open" -> "door.closed" [label="after 300ms, press"];|}
       lines
    );
  Alcotest.(check bool)
    "an edge with no label has no label" true
    (List.mem {|"door.open" -> "door.gone";|} lines)

let () =
  Alcotest.run "verify"
    [
      ( "remote",
        [
          Alcotest.test_case "answers are delivered" `Quick
            test_answers_are_delivered;
          Alcotest.test_case "an unhandled network error is a dead end" `Quick
            test_unhandled_network_error_is_a_dead_end;
          Alcotest.test_case "decode errors only on request" `Quick
            test_decode_only_on_request;
          Alcotest.test_case "answers of another endpoint are not used" `Quick
            test_answers_of_another_endpoint_are_not_used;
          Alcotest.test_case "one answer per call" `Quick
            test_one_answer_per_call;
          Alcotest.test_case "a timer fires once" `Quick test_a_timer_fires_once;
        ]
      );
      ( "live",
        [
          Alcotest.test_case "a stale update is delivered" `Quick
            test_stale_update_is_delivered;
          Alcotest.test_case "arrivals are bounded" `Quick
            test_arrivals_are_bounded;
          Alcotest.test_case "a failure ends the subscription" `Quick
            test_failure_ends_the_subscription;
        ]
      );
      ( "ownership",
        [
          Alcotest.test_case "timer outside its state" `Quick
            test_timer_outside_its_state;
          Alcotest.test_case "call outside its state" `Quick
            test_call_outside_its_state;
          Alcotest.test_case "subscription outside its state" `Quick
            test_subscription_outside_its_state;
          Alcotest.test_case "missing case at run time" `Quick
            test_missing_case_at_run_time;
          Alcotest.test_case "app: the root machine gets answers" `Quick
            test_app_root_gets_answers;
          Alcotest.test_case "app: the root machine owns its timers" `Quick
            test_app_root_timer_owner;
        ]
      );
      ( "drift",
        [
          Alcotest.test_case "unreached edge" `Quick test_unreached_edge;
          Alcotest.test_case "edges are labelled" `Quick test_edges_are_labelled;
        ]
      );
      ( "export",
        [
          Alcotest.test_case "quint module" `Quick test_quint;
          Alcotest.test_case "quint module with no edge" `Quick
            test_quint_without_edges;
          Alcotest.test_case "quint state names" `Quick test_quint_state_names;
          Alcotest.test_case "dot labels" `Quick test_dot_labels;
        ]
      );
      ( "props",
        [
          Alcotest.test_case "props reach the child" `Quick
            test_props_reach_the_child;
        ]
      );
    ]
