open Silica

let codec_int =
  Codec.make
    ~encode:(fun n -> Silica_json.Json.declassify (`Int n))
    ~decode:(fun _ -> Ok 0)

let codec_unit =
  Codec.make
    ~encode:(fun () -> Silica_json.Json.declassify `Null)
    ~decode:(fun _ -> Ok ())

let endpoint =
  Remote.declare ~path:"/api/n" ~input:codec_unit ~output:codec_int
    ~rejected:Codec.string

let other =
  Remote.declare ~path:"/api/other" ~input:codec_unit ~output:codec_int
    ~rejected:Codec.string

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

let show_input show : (_, _) Check.input -> string = function
  | Check.Event event ->
      show event
  | Check.Props _ ->
      "new props"
  | Check.In_child _ ->
      "child"

let report_to_string report = Format.asprintf "%a" Check.pp_report report

let trace_of (violation : (_, _) Check.violation) =
  List.map
    (fun (step : _ Check.step) -> step.input)
    (Check.steps violation.counterexample)

let expect_violation show name trace check =
  let report = Check.run check in
  match report.violations with
  | first :: _ ->
      Alcotest.(check string) "rule" name first.rule;
      Alcotest.(check (list string))
        "trace" trace
        (List.map (show_input show) (trace_of first))
  | [] ->
      Alcotest.failf "expected violation %s, got %s" name
        (report_to_string report)

let expect_passed report =
  if not (Check.passed report) then Alcotest.fail (report_to_string report)

let expect_unreached report =
  if report.Check.violations <> [] || not report.complete then
    Alcotest.fail (report_to_string report);
  report.unreached

type load = Loaded of (int, string) Remote.answer | Retry

let show_load = function
  | Loaded (Remote.Ok n) ->
      Printf.sprintf "loaded %d" n
  | Loaded (Remote.Rejected reason) ->
      "loaded rejected " ^ reason
  | Loaded (Remote.Failed e) ->
      "loaded " ^ show_error e
  | Retry ->
      "retry"

type loading = Loading | Ready of int | Failed of int

let loading_name = function
  | Loading ->
      "loading"
  | Ready _ ->
      "ready"
  | Failed _ ->
      "failed"

let loading_states = [ "loading"; "ready"; "failed" ]

let loader ?(network = true) ?(decode = true) () =
  Machine.make ~id:"loader" ~init:Loading ~name:loading_name
    ~states:loading_states ~event_name:show_load
    ~final:(function Ready _ -> true | Loading | Failed _ -> false)
    ~during:(function
      | Loading ->
          [ Remote.call endpoint () (fun r -> Loaded r) ]
      | Ready _ | Failed _ ->
          []
      )
    (fun state event ->
      match (state, event) with
      | Loading, Loaded (Remote.Ok n) ->
          Machine.Go (Ready n)
      | Loading, Loaded (Remote.Failed (Http code)) ->
          Machine.Go (Failed code)
      | Loading, Loaded (Remote.Failed (Network _)) when network ->
          Machine.Go (Failed 0)
      | Loading, Loaded (Remote.Failed (Decode _)) when decode ->
          Machine.Go (Failed 1)
      | Failed _, Retry ->
          Machine.Go Loading
      | _ ->
          Machine.Ignore
    )

let loader_view = function
  | Failed _ ->
      Node.element "button" [ Attr.on_press Retry ] [ Node.text "retry" ]
  | Loading | Ready _ ->
      Node.text "x"

let test_answers_are_delivered () =
  let c = Component.make (loader ()) loader_view in
  let readies : int list ref = ref [] in
  let seen =
    Check.always ~name:"record"
      (Check.state (fun state ->
           (match state with Ready n -> readies := n :: !readies | _ -> ());
           true
       )
      )
  in
  let report =
    Check.run
      (Check.component
         ~samples:[ Check.answers endpoint [ 1; 2 ] ]
         ~properties:[ seen ] c
      )
  in
  expect_passed report;
  Alcotest.(check (list int))
    "each sample reaches ready" [ 1; 2 ]
    (List.sort compare !readies);
  Alcotest.(check int)
    "loading, ready 1, ready 2, failed 500, failed 0" 5 report.states

let test_rejected_answers_are_delivered () =
  let rejected = ref false in
  let machine =
    Machine.make ~id:"loader" ~init:Loading ~name:loading_name
      ~states:loading_states
      ~final:(function Ready _ -> true | Loading | Failed _ -> false)
      ~during:(function
        | Loading -> [ Remote.call endpoint () (fun r -> Loaded r) ] | _ -> []
        )
      (fun state event ->
        match (state, event) with
        | Loading, Loaded (Remote.Ok n) ->
            Machine.Go (Ready n)
        | Loading, Loaded (Remote.Rejected reason) ->
            if reason = "no" then rejected := true;
            Machine.Go (Ready (String.length reason))
        | Loading, Loaded (Remote.Failed _) ->
            Machine.Go (Ready 0)
        | _ ->
            Machine.Ignore
      )
  in
  let report =
    Check.run
      (Check.component
         ~allow:
           [
             Check.allow ~rule:"unreached-state" ~state:"failed"
               ~reason:"Every answer leads to Ready, so no step goes to Failed."
               ();
           ]
         ~samples:[ Check.answers ~rejected:[ "no" ] endpoint [ 1 ] ]
         (Component.make machine loader_view)
      )
  in
  expect_passed report;
  Alcotest.(check bool) "the rejection reached the machine" true !rejected;
  Alcotest.(check int) "loading, ready 1, ready 2 (no), ready 0" 4 report.states

let test_unhandled_network_error_is_an_ignored_event () =
  let c = Component.make (loader ~network:false ()) loader_view in
  expect_violation show_load "ignored-event" [ "loaded network down" ]
    (Check.component ~samples:[ Check.answers endpoint [ 1 ] ] c)

let test_decode_only_on_request () =
  let errors : int list ref = ref [] in
  let record =
    Check.always ~name:"record"
      (Check.state (fun state ->
           (match state with Failed n -> errors := n :: !errors | _ -> ());
           true
       )
      )
  in
  let c = Component.make (loader ~decode:false ()) loader_view in
  let report =
    Check.run
      (Check.component
         ~samples:[ Check.answers endpoint [] ]
         ~properties:[ record ] c
      )
  in
  Alcotest.(check (list string))
    "no ok answer, ready never reached" [ "ready" ] (expect_unreached report);
  Alcotest.(check (list int))
    "only the two default errors" [ 0; 500 ]
    (List.sort_uniq compare !errors);
  expect_violation show_load "ignored-event" [ "loaded decode sample" ]
    (Check.component ~samples:[ Check.answers ~decode:true endpoint [ 1 ] ] c)

let test_answers_of_another_endpoint_are_not_used () =
  let c = Component.make (loader ()) loader_view in
  let report =
    Check.run (Check.component ~samples:[ Check.answers other [ 7 ] ] c)
  in
  Alcotest.(check (list string))
    "no ok answer for /api/n" [ "ready" ] (expect_unreached report)

let test_answered_call_runs_again () =
  let answers = ref [] in
  let counting =
    Machine.make ~id:"loader" ~init:Loading ~name:loading_name
      ~states:loading_states
      ~final:(function Ready _ -> true | Loading | Failed _ -> false)
      ~during:(function
        | Loading -> [ Remote.call endpoint () (fun r -> Loaded r) ] | _ -> []
        )
      (fun state event ->
        match (state, event) with
        | Loading, Loaded answer ->
            answers := show_load (Loaded answer) :: !answers;
            Machine.Stay []
        | Loading, Retry ->
            Machine.Go (Ready 0)
        | _ ->
            Machine.Ignore
      )
  in
  let view = function
    | Loading ->
        Node.element "button" [ Attr.on_press Retry ] [ Node.text "go" ]
    | Ready _ | Failed _ ->
        Node.text "done"
  in
  let report =
    Check.run
      (Check.component
         ~allow:
           [
             Check.allow ~rule:"unreached-state" ~state:"failed"
               ~reason:"Loading goes only to Ready, so no step goes to Failed."
               ();
           ]
         ~samples:[ Check.answers endpoint [ 1 ] ]
         (Component.make counting view)
      )
  in
  expect_passed report;
  Alcotest.(check int)
    "loading with the call, ready: an answered call that the state still owns \
     runs again"
    2 report.states;
  Alcotest.(check int) "four from loading, none from ready" 4 report.transitions;
  Alcotest.(check (list string))
    "three answers handled"
    [ "loaded 1"; "loaded http 500"; "loaded network down" ]
    (List.sort_uniq compare !answers);
  Alcotest.(check (list (triple string string bool)))
    "the answers kept the state, the retry entered ready; with no event_name \
     the edges are labelled by cause"
    [
      ("failed /api/n", "loading", true);
      ("ok /api/n", "loading", true);
      ("press button \"go\"", "ready", false);
    ]
    (List.map
       (fun (label : Spec.label) -> (label.event, label.target, label.kept))
       (List.hd report.specs).edges
    )

type ticking = Waiting of int

let ticking_name = function Waiting _ -> "waiting"

let ticking_states = [ "waiting" ]

let test_fired_timer_runs_again () =
  let ticker =
    Machine.make ~id:"ticker" ~init:(Waiting 0) ~name:ticking_name
      ~states:ticking_states
      ~during:(fun _ -> [ Source.after 100 Retry ])
      (fun (Waiting count) -> function
        | Retry ->
            Machine.Go (Waiting (count + 1))
        | Loaded _ ->
            Machine.Stay []
        )
  in
  let view _ =
    Node.element "button"
      [ Attr.on_press (Loaded (Remote.Ok 0)) ]
      [ Node.text "x" ]
  in
  let once =
    Check.always ~name:"at most one tick"
      (Check.state (fun (Waiting count) -> count <= 1))
  in
  let check =
    Check.component
      ~within:(fun (Waiting count) -> count <= 3)
      ~properties:[ once ]
      (Component.make ticker view)
  in
  expect_violation show_load "at most one tick" [ "retry"; "retry" ] check;
  let report = Check.run check in
  Alcotest.(check bool) "within ends the search" true report.complete;
  Alcotest.(check int) "waiting 0 to 3" 4 report.states

let test_violation_does_not_stop_exploration () =
  let ticker =
    Machine.make ~id:"ticker" ~init:(Waiting 0) ~name:ticking_name
      ~states:ticking_states
      ~during:(fun _ -> [ Source.after 100 Retry ])
      (fun (Waiting count) -> function
        | Retry ->
            Machine.Go (Waiting (count + 1))
        | Loaded _ ->
            Machine.Stay []
        )
  in
  let view _ =
    Node.element "button"
      [ Attr.on_press (Loaded (Remote.Ok 0)) ]
      [ Node.text "x" ]
  in
  let report =
    Check.run
      (Check.component
         ~within:(fun (Waiting count) -> count <= 5)
         ~properties:
           [
             Check.always ~name:"never one"
               (Check.state (fun (Waiting count) -> count <> 1));
           ]
         (Component.make ticker view)
      )
  in
  Alcotest.(check (list string))
    "one violation" [ "never one" ]
    (List.map
       (fun (violation : _ Check.violation) -> violation.rule)
       report.violations
    );
  Alcotest.(check int)
    "every state past the violation is explored" 6 report.states;
  Alcotest.(check bool) "complete" true report.complete;
  Alcotest.(check bool) "not truncated" false report.truncated

type watch = Update of int Topic.update | Leave

let show_watch = function
  | Update { seq; status } ->
      Printf.sprintf "%d %s" seq (show_status status)
  | Leave ->
      "leave"

type watching = Watching of int | Broken | Away

let watching_name = function
  | Watching _ ->
      "watching"
  | Broken ->
      "broken"
  | Away ->
      "away"

let watching_states = [ "watching"; "broken"; "away" ]

let watch_source = Source.live topic "all" (fun u -> Update u)

let watcher ~on_stale =
  Machine.make ~id:"watcher" ~init:(Watching 0) ~name:watching_name
    ~states:watching_states
    ~final:(function Broken | Away -> true | Watching _ -> false)
    ~during:(function Watching _ -> [ watch_source ] | Broken | Away -> [])
    (fun state event ->
      match (state, event) with
      | Watching count, Update { status = Ready { stale = true; _ }; _ } ->
          on_stale count
      | Watching count, Update { status = Ready { stale = false; _ }; _ } ->
          Machine.Go (Watching (count + 1))
      | Watching _, Update _ ->
          Machine.Stay []
      | Watching _, Leave ->
          Machine.Go Away
      | (Broken | Away), _ ->
          Machine.Ignore
    )

let watcher_view = function
  | Watching _ ->
      Node.element "button" [ Attr.on_press Leave ] [ Node.text "leave" ]
  | Broken | Away ->
      Node.text "x"

let not_broken =
  Check.always ~name:"not broken"
    (Check.state (function Broken -> false | _ -> true))

let test_stale_update_is_delivered () =
  let c =
    Component.make (watcher ~on_stale:(fun _ -> Machine.Go Broken)) watcher_view
  in
  expect_violation show_watch "not broken"
    [ "0 loading"; "1 ready 4"; "0 stale 4" ]
    (Check.component
       ~samples:[ Check.states topic [ 4; 5 ] ]
       ~properties:[ not_broken ] c
    )

let below n =
  Check.always
    ~name:(Printf.sprintf "fewer than %d states" n)
    (Check.state (function Watching count -> count < n | _ -> true))

let ok_watcher = watcher ~on_stale:(fun count -> Machine.Go (Watching count))

let test_arrivals_are_bounded () =
  let c = Component.make ok_watcher watcher_view in
  expect_passed
    (Check.run
       (Check.component ~arrivals:1
          ~samples:[ Check.states topic [ 4; 5 ] ]
          ~properties:[ below 3 ]
          ~allow:
            [
              Check.allow ~rule:"unreached-state" ~state:"broken"
                ~reason:
                  "A stale update goes back to Watching, so no step goes to \
                   Broken."
                ();
            ]
          c
       )
    );
  expect_violation show_watch "fewer than 2 states"
    [ "0 loading"; "1 ready 4"; "0 stale 4"; "2 ready 4" ]
    (Check.component ~arrivals:1
       ~samples:[ Check.states topic [ 4; 5 ] ]
       ~properties:[ below 2 ]
       c
    );
  expect_violation show_watch "fewer than 3 states"
    [ "0 loading"; "1 ready 4"; "2 ready 4"; "0 stale 4"; "3 ready 4" ]
    (Check.component
       ~samples:[ Check.states topic [ 4; 5 ] ]
       ~properties:[ below 3 ]
       c
    )

let test_failure_ends_the_subscription () =
  let failed : int option list ref = ref [] in
  let after_failure =
    Machine.make ~id:"watcher" ~init:(Watching 0) ~name:watching_name
      ~states:watching_states
      ~final:(function Broken | Away -> true | Watching _ -> false)
      ~during:(function Watching _ -> [ watch_source ] | Broken | Away -> [])
      (fun state event ->
        match (state, event) with
        | Watching count, Update { status = Failed { last; _ }; _ } ->
            failed := last :: !failed;
            Machine.Go (Watching (count + 100))
        | Watching count, Update { status = Ready { stale = false; _ }; _ }
          when count >= 100 ->
            Machine.Go (Watching (count + 1000))
        | Watching count, Update { status = Ready { stale = false; _ }; _ } ->
            Machine.Go (Watching (count + 1))
        | Watching _, Update _ ->
            Machine.Stay []
        | Watching _, Leave ->
            Machine.Go Away
        | (Broken | Away), _ ->
            Machine.Ignore
      )
  in
  let nothing_after_failure =
    Check.always ~name:"nothing arrives after a failure"
      (Check.state (function Watching count -> count < 1000 | _ -> true))
  in
  expect_passed
    (Check.run
       (Check.component
          ~allow:
            [
              Check.allow ~rule:"unreached-state" ~state:"broken"
                ~reason:"No step of the update goes to Broken." ();
            ]
          ~samples:[ Check.states topic [ 4 ] ]
          ~properties:[ nothing_after_failure ]
          (Component.make after_failure watcher_view)
       )
    );
  Alcotest.(check (list (option int)))
    "a failure carries the last state" [ None; Some 4 ]
    (List.sort_uniq compare !failed)

type go = Go | Wait

let show_go = function Go -> "go" | Wait -> "wait"

type three = A | B

let three_name = function A -> "a" | B -> "b"

let three_states = [ "a"; "b" ]

let drifting_owns source =
  let calls = ref 0 in
  function
  | A ->
      []
  | B ->
      incr calls;
      if !calls = 1 then
        [ source ]
      else
        []

let wrong source =
  Machine.make ~id:"wrong" ~init:A ~name:three_name ~states:three_states
    ~during:(drifting_owns source) (fun state event ->
      match (state, event) with
      | A, Go ->
          Machine.Go B
      | B, Wait ->
          Machine.Go A
      | _ ->
          Machine.Stay []
  )

let go_view _ =
  Node.list
    [
      Node.element "button" [ Attr.on_press Go ] [ Node.text "go" ];
      Node.element "button" [ Attr.on_press Wait ] [ Node.text "wait" ];
    ]

let test_timer_outside_its_state () =
  expect_violation show_go "timer-owner" [ "go" ]
    (Check.component (Component.make (wrong (Source.after 100 Wait)) go_view))

let test_call_outside_its_state () =
  expect_violation show_go "call-owner" [ "go" ]
    (Check.component
       (Component.make (wrong (Remote.call endpoint () (fun _ -> Wait))) go_view)
    )

let test_subscription_outside_its_state () =
  expect_violation show_go "call-owner" [ "go" ]
    (Check.component
       (Component.make (wrong (Source.live topic "all" (fun _ -> Wait))) go_view)
    )

type attempt = Fetching of int | Fetched of int

let attempt_name = function Fetching _ -> "fetching" | Fetched _ -> "fetched"

let attempt_states = [ "fetching"; "fetched" ]

type attempt_event = Got of int * (int, string) Remote.answer | Again

let test_answers_rebuilt_at_each_start () =
  let machine =
    Machine.make ~id:"attempts" ~init:(Fetching 0) ~name:attempt_name
      ~states:attempt_states
      ~during:(function
        | Fetching attempt ->
            [ Remote.call endpoint () (fun answer -> Got (attempt, answer)) ]
        | Fetched _ ->
            []
        )
      (fun state event ->
        match (state, event) with
        | Fetching attempt, Got (seen, Remote.Ok _) when seen = attempt ->
            Machine.Go (Fetched attempt)
        | Fetching attempt, Got (seen, (Remote.Rejected _ | Remote.Failed _))
          when seen = attempt && attempt < 2 ->
            Machine.Reenter (Fetching (attempt + 1))
        | Fetching _, Got _ ->
            Machine.Ignore
        | Fetched _, Again ->
            Machine.Go (Fetching 0)
        | Fetching _, Again | Fetched _, Got _ ->
            Machine.Ignore
      )
  in
  let view = function
    | Fetching _ ->
        Node.text "fetching"
    | Fetched _ ->
        Node.element "button" [ Attr.on_press Again ] [ Node.text "again" ]
  in
  let report =
    Check.run
      (Check.component
         ~samples:[ Check.answers endpoint [ 1 ] ]
         ~within:(function Fetching attempt | Fetched attempt -> attempt < 2)
         (Component.make machine view)
      )
  in
  expect_passed report;
  Alcotest.(check bool)
    "a later attempt gets its own answers" true
    (List.mem_assoc "fetched" report.paths)

type moving = Still | Moving

let moving_name = function Still -> "still" | Moving -> "moving"

let moving_states = [ "still"; "moving" ]

type moving_event = Moved

let test_start_away_from_init () =
  let machine =
    Machine.make_with_props ~id:"moving" ~init:Still ~name:moving_name
      ~states:moving_states
      ~on_props:(fun _ moves _ ->
        if moves then
          Machine.Go Moving
        else
          Machine.Ignore
      )
      (fun _ _ Moved -> Machine.Stay [])
  in
  let view _ =
    Node.element "button" [ Attr.on_press Moved ] [ Node.text "move" ]
  in
  let report =
    Check.run (Check.component ~props:[ true ] (Component.make machine view))
  in
  expect_passed report;
  let spec = List.hd report.specs in
  Alcotest.(check string) "the state reached first" "moving" spec.init;
  Alcotest.(check bool)
    "the initial state of the spec is one of its states" true
    (List.exists
       (fun (state : Spec.state) -> state.name = spec.init)
       spec.states
    )

type asking = Ready_to_ask | Asking | Closed

let asking_name = function
  | Ready_to_ask ->
      "ready_to_ask"
  | Asking ->
      "asking"
  | Closed ->
      "closed"

let asking_states = [ "ready_to_ask"; "asking"; "closed" ]

type asking_event = Ask | Close | Measured

let test_late_action_result_is_allowed () =
  let machine =
    Machine.make ~id:"asking" ~init:Ready_to_ask ~name:asking_name
      ~states:asking_states
      ~final:(function Closed -> true | Ready_to_ask | Asking -> false)
      (fun state event ->
        match (state, event) with
        | Ready_to_ask, Ask ->
            Machine.Go_with
              ( Asking,
                [
                  Action.Measure
                    { target = Ref.make "box"; on_rect = (fun _ -> Measured) };
                ]
              )
        | Asking, Measured ->
            Machine.Stay []
        | (Ready_to_ask | Asking), Close ->
            Machine.Go Closed
        | Ready_to_ask, Measured | Asking, Ask | Closed, (Ask | Close | Measured)
          ->
            Machine.Ignore
      )
  in
  let close =
    Node.element "button" [ Attr.on_press Close ] [ Node.text "close" ]
  in
  let view = function
    | Ready_to_ask ->
        Node.element "div"
          [ Attr.ref_ (Ref.make "box") ]
          [
            Node.element "button" [ Attr.on_press Ask ] [ Node.text "ask" ];
            close;
          ]
    | Asking ->
        Node.element "div" [ Attr.ref_ (Ref.make "box") ] [ close ]
    | Closed ->
        Node.text "closed"
  in
  expect_passed (Check.run (Check.component (Component.make machine view)))

let show_app_event show : _ Check.app_event -> string = function
  | Event event ->
      show event
  | Link url ->
      "link " ^ Url.to_string url
  | Open url ->
      "open " ^ Url.to_string url
  | Browser_back ->
      "back"
  | Browser_forward ->
      "forward"
  | In_child _ ->
      "child"

let app_trace show check =
  let report = Check.run check in
  match report.violations with
  | first :: _ ->
      (first.rule, List.map (show_app_event show) (trace_of first))
  | [] ->
      (report_to_string report, [])

let test_app_root_gets_answers () =
  let machine = loader () in
  let root _ = Component.make machine loader_view in
  let start, _ = Machine.start machine () in
  let printed = ref [] in
  let print state =
    let path = "/" ^ string_of_int (Hashtbl.hash state) in
    printed := (path, state) :: !printed;
    Url.of_string path
  in
  let parse url = List.assoc (Url.to_string url) !printed in
  let report =
    Check.run
      (Check.app
         ~samples:[ Check.answers endpoint [ 1 ] ]
         ~routes:[ start ] ~parse ~print
         ~url_event:(fun _ -> Retry)
         root
      )
  in
  expect_passed report;
  Alcotest.(check bool)
    "the answer of the root call is delivered" true
    (List.exists
       (fun (label : Spec.label) ->
         label.target = "ready" && label.event = "loaded 1"
       )
       (List.hd report.specs).edges
    )

let test_app_root_timer_owner () =
  let machine = wrong (Source.after 100 Wait) in
  Alcotest.(check (pair string (list string)))
    "the root machine is checked for ownership" ("timer-owner", [ "go" ])
    (app_trace show_go
       (Check.app ~routes:[ A ]
          ~parse:(fun _ -> A)
          ~print:(fun _ -> Url.of_string "/")
          ~url_event:(fun _ -> Wait)
          (fun _ -> Component.make machine go_view)
       )
    )

type drift = Start | Done | Never [@@warning "-37"]

let drift_name = function Start -> "start" | Done -> "done" | Never -> "never"

let drift_states = [ "start"; "done"; "never" ] [@@warning "-37"]

let drifting =
  Machine.make ~id:"drift" ~init:Start ~name:drift_name ~states:drift_states
    ~final:(function Done | Never -> true | Start -> false)
    (fun state event ->
      match (state, event) with
      | Start, Go ->
          Machine.Go Done
      | Start, Wait ->
          Machine.Stay []
      | (Done | Never), _ ->
          Machine.Ignore
    )

let drift_view = function
  | Start ->
      go_view ()
  | Done | Never ->
      Node.text "done"

let test_unreached_state () =
  let c = Component.make drifting drift_view in
  Alcotest.(check (list string))
    "declared, never reached" [ "never" ]
    (expect_unreached (Check.run (Check.component c)));
  expect_passed
    (Check.run
       (Check.component
          ~allow:
            [
              Check.allow ~rule:"unreached-state" ~state:"never"
                ~reason:"No step of the update goes to Never." ();
            ]
          c
       )
    )

let test_edges_are_labelled () =
  let c = Component.make (loader ()) loader_view in
  let report =
    Check.run (Check.component ~samples:[ Check.answers endpoint [ 1 ] ] c)
  in
  expect_passed report;
  Alcotest.(check (list (triple string string string)))
    "taken edges with the event names"
    [
      ("failed", "loading", "retry");
      ("loading", "failed", "loaded http 500");
      ("loading", "failed", "loaded network down");
      ("loading", "ready", "loaded 1");
    ]
    (List.map
       (fun (label : Spec.label) -> (label.from, label.target, label.event))
       (List.hd report.specs).edges
    )

type child_event = Dec
type child_state = Count of int

let child_state_name = function Count _ -> "count"

let child_state_states = [ "count" ]

let show_child Dec = "dec"

let child =
  Component.make
    (Machine.make_with_props ~id:"child" ~init:(Count 1) ~name:child_state_name
       ~states:child_state_states
       ~final:(fun (Count n) -> n <= 0)
       ~on_props:(fun _ value _ -> Machine.Go (Count value))
       (fun _ (Count n) Dec -> Machine.Go (Count (n - 1)))
    )
    (fun (Count n) ->
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
          Node.component_with_props
            ~props:
              ( if n = 0 then
                  1
                else
                  7
              )
            child;
          Node.component_with_props ~props:1 child;
        ]
    )

let below_five =
  Check.always ~name:"below five" (Check.state (fun (Count n) -> n < 5))

let test_props_reach_the_child () =
  let report = Check.run (Check.component parent) in
  expect_passed report;
  Alcotest.(check (list string))
    "listed" [ "component child" ] report.not_explored;
  let props = Check.props report.children child in
  Alcotest.(check (list int))
    "props seen in the parent views" [ 1; 7 ] (List.sort compare props);
  Alcotest.(check int)
    "no props for the parent itself" 0
    (List.length (Check.props report.children parent));
  expect_violation show_child "below five" []
    (Check.component ~props ~properties:[ below_five ] child);
  expect_passed
    (Check.run (Check.component ~props:[ 1 ] ~properties:[ below_five ] child))

let test_new_props_are_a_step () =
  let at_seven =
    Check.always ~name:"never seven" (Check.state (fun (Count n) -> n <> 7))
  in
  let only_after_props =
    Component.make
      (Machine.make_with_props ~id:"child" ~init:(Count 0)
         ~name:child_state_name ~states:child_state_states
         ~on_props:(fun previous value _ ->
           match previous with
           | None ->
               Machine.Ignore
           | Some _ ->
               Machine.Go (Count value)
         )
         (fun _ (Count n) Dec -> Machine.Go (Count (n - 1)))
      )
      (fun _ -> Node.text "zero")
  in
  expect_violation show_child "never seven" [ "new props" ]
    (Check.component ~props:[ 1; 7 ] ~properties:[ at_seven ] only_after_props)

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
        { from = "closed"; target = "open"; event = "press"; kept = false };
        { from = "open"; target = "closed"; event = "press"; kept = false };
        {
          from = "open";
          target = "closed";
          event = "after 300ms";
          kept = false;
        };
        { from = "open"; target = "closed"; event = "press"; kept = false };
        { from = "open"; target = "gone"; event = ""; kept = false };
        { from = "open"; target = "open"; event = ""; kept = false };
        { from = "open"; target = "open"; event = "type"; kept = true };
      ];
  }

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
  Alcotest.(check string) "quint" expected (Spec.to_quint door)

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
      edges =
        [
          { from = "/"; target = "/*"; event = ""; kept = false };
          { from = "a-b"; target = "a_b"; event = ""; kept = false };
        ];
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
  let dot = Spec.to_dot door in
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
          Alcotest.test_case "rejected answers are delivered" `Quick
            test_rejected_answers_are_delivered;
          Alcotest.test_case "an unhandled network error is an ignored event"
            `Quick test_unhandled_network_error_is_an_ignored_event;
          Alcotest.test_case "decode errors only on request" `Quick
            test_decode_only_on_request;
          Alcotest.test_case "answers of another endpoint are not used" `Quick
            test_answers_of_another_endpoint_are_not_used;
          Alcotest.test_case "an answered call runs again" `Quick
            test_answered_call_runs_again;
          Alcotest.test_case "a fired timer runs again" `Quick
            test_fired_timer_runs_again;
          Alcotest.test_case "a property violation does not stop exploration"
            `Quick test_violation_does_not_stop_exploration;
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
          Alcotest.test_case "answers are rebuilt at each start" `Quick
            test_answers_rebuilt_at_each_start;
          Alcotest.test_case "a start away from init" `Quick
            test_start_away_from_init;
          Alcotest.test_case "a late action result is allowed" `Quick
            test_late_action_result_is_allowed;
          Alcotest.test_case "app: the root machine gets answers" `Quick
            test_app_root_gets_answers;
          Alcotest.test_case "app: the root machine owns its timers" `Quick
            test_app_root_timer_owner;
        ]
      );
      ( "drift",
        [
          Alcotest.test_case "unreached state" `Quick test_unreached_state;
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
          Alcotest.test_case "new props are a step" `Quick
            test_new_props_are_a_step;
        ]
      );
    ]
