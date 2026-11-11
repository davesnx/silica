open Silica

let codec_int =
  Codec.make
    ~encode:(fun number -> Silica_json.Json.declassify (`Int number))
    ~decode:(fun _ -> Ok 0)

let codec_unit =
  Codec.make
    ~encode:(fun () -> Silica_json.Json.declassify `Null)
    ~decode:(fun _ -> Ok ())

let search_endpoint =
  Remote.declare ~path:"/api/search" ~input:codec_unit ~output:codec_int
    ~rejected:Codec.string

let save_endpoint =
  Remote.declare ~path:"/api/save" ~input:codec_unit ~output:codec_int
    ~rejected:Codec.string

let print report = Format.asprintf "%a" Check.pp_report report

let contains text part =
  let length = String.length part in
  let rec go index =
    index + length <= String.length text
    && (String.sub text index length = part || go (index + 1))
  in
  go 0

let button event label =
  Node.element "button" [ Attr.on_press event ] [ Node.text label ]

let answer_name = function
  | Remote.Ok number ->
      Printf.sprintf "ok %d" number
  | Remote.Rejected reason ->
      "rejected " ^ reason
  | Remote.Failed (Http code) ->
      Printf.sprintf "http %d" code
  | Remote.Failed (Network _) ->
      "network"
  | Remote.Failed (Decode _) ->
      "decode"

let only_violation report =
  match report.Check.violations with
  | [ violation ] ->
      violation
  | _ ->
      Alcotest.failf "expected exactly one violation, got:\n%s" (print report)

let events steps = List.map (fun (step : _ Check.step) -> step.event) steps

let replays machine (stem : _ Check.step list) (cycle : _ Check.step list) =
  let play state (step : (unit, _) Check.input Check.step) =
    match step.input with
    | Check.Event event ->
        let next, _, _ = Machine.step machine () state event in
        Alcotest.(check string)
          ("the step " ^ step.event ^ " reaches its target")
          step.target
          (Machine.name machine next);
        next
    | Check.Props _ | Check.In_child _ ->
        Alcotest.fail "no props or children in these fixtures"
  in
  let start, _ = Machine.start machine () in
  let entry = List.fold_left play start stem in
  let back = List.fold_left play entry cycle in
  Alcotest.(check bool)
    "the cycle returns to its first snapshot" true (back = entry)

type search_event =
  | Typed of string
  | Fire
  | Found of (int, string) Remote.answer

type search = Idle | Typing of string | Searching | Results of int | Error
[@@deriving state_shape]

let search_events =
  Machine.shape
    ~name:(function
      | Typed text ->
          "typed " ^ text
      | Fire ->
          "fire"
      | Found answer ->
          "found " ^ answer_name answer
      )
    ()

let debounce =
  Machine.make ~id:"search" ~init:Idle ~shape:search_shape ~events:search_events
    ~owns:(function
      | Typing _ ->
          [ Source.after 300 Fire ]
      | Searching ->
          [ Remote.call search_endpoint () (fun answer -> Found answer) ]
      | Idle | Results _ | Error ->
          []
      )
    (fun state event ->
      match (state, event) with
      | (Idle | Typing _ | Results _ | Error), Typed text ->
          Reenter (Typing text)
      | Typing _, Fire ->
          Go Searching
      | Searching, Found (Ok count) ->
          Go (Results count)
      | Searching, Found _ ->
          Go Error
      | _ ->
          Ignore
    )

let test_debounce_holds () =
  let report =
    Check.run
      (Check.component
         ~inputs:(fun _ -> [ Typed "a"; Typed "b" ])
         ~samples:[ Check.answers search_endpoint [ 3 ] ]
         ~properties:
           [
             Check.leads_to ~name:"typing leads to results or an error"
               ~from:(Check.event (function Typed _ -> true | _ -> false))
               ~to_:Check.(state_named "results" ||| state_named "error")
               ();
           ]
         (Component.make debounce (fun _ -> Node.text "search"))
      )
  in
  if not (Check.passed report) then Alcotest.fail (print report)

type save_event = Save | Answer of (int, string) Remote.answer
type save = Editing | Saving | Saved | Failed [@@deriving state_shape]

let save_events =
  Machine.shape
    ~name:(function
      | Save -> "save" | Answer answer -> "answer " ^ answer_name answer
      )
    ()

let saver ~lost =
  Machine.make ~id:"saver" ~init:Editing ~shape:save_shape ~events:save_events
    ~final:(function Saved -> true | Editing | Saving | Failed -> false)
    ~owns:(function
      | Saving ->
          [ Remote.call save_endpoint () (fun answer -> Answer answer) ]
      | Editing | Saved | Failed ->
          []
      )
    (fun state event ->
      match (state, event) with
      | (Editing | Failed), Save ->
          Go Saving
      | Saving, Answer (Ok _) ->
          if lost then
            Stay []
          else
            Go Saved
      | Saving, Answer _ ->
          Go Failed
      | _ ->
          Ignore
    )

let save_view = function
  | Editing | Failed ->
      button Save "Save"
  | Saving ->
      Node.text "saving"
  | Saved ->
      Node.text "saved"

let settles =
  Check.leads_to ~name:"save leads to saved or failed"
    ~from:(Check.event (( = ) Save))
    ~to_:Check.(state_named "saved" ||| state_named "failed")
    ()

let save_check ~lost =
  Check.component
    ~allow:
      [
        Check.allow ~rule:"unreached-state" ~state:"saved"
          ~reason:"The lost answer never reaches Saved." ();
      ]
    ~samples:[ Check.answers save_endpoint [ 1 ] ]
    ~properties:[ settles ]
    (Component.make (saver ~lost) save_view)

let test_save_holds () =
  let report = Check.run (save_check ~lost:false) in
  Alcotest.(check int) "no violation" 0 (List.length report.violations);
  Alcotest.(check bool) "complete" true report.complete

let test_lost_answer_loops () =
  let report = Check.run (save_check ~lost:true) in
  let violation = only_violation report in
  Alcotest.(check string) "rule" "save leads to saved or failed" violation.rule;
  Alcotest.(check string)
    "the cycle starts in saving" "saving" violation.state_name;
  match violation.counterexample with
  | Loop { stem; cycle } ->
      Alcotest.(check (list string)) "stem" [ "save" ] (events stem);
      Alcotest.(check (list string)) "cycle" [ "answer ok 1" ] (events cycle);
      Alcotest.(check bool)
        "the message names the cycle and what it delivers" true
        (contains violation.message "saving -> saving forever"
        && contains violation.message "call saver/saving/"
        );
      Alcotest.(check bool)
        "the cause of the cycle step is the answer" true
        ( match (List.hd cycle).cause with
        | Check.Answer { path; _ } ->
            path = "/api/save"
        | _ ->
            false
        );
      replays (saver ~lost:true) stem cycle
  | Path _ | Waits _ ->
      Alcotest.failf "expected a loop:\n%s" (print report)

type sync_event = Sync | Synced_answer of (int, string) Remote.answer
type sync = Unsynced | Syncing | Synced | Offline [@@deriving state_shape]

let sync_events =
  Machine.shape
    ~name:(function
      | Sync -> "sync" | Synced_answer answer -> "answer " ^ answer_name answer
      )
    ()

let syncer =
  Machine.make ~id:"sync" ~init:Unsynced ~shape:sync_shape ~events:sync_events
    ~final:(function Synced -> true | Unsynced | Syncing | Offline -> false)
    ~owns:(function
      | Syncing ->
          [ Remote.call save_endpoint () (fun answer -> Synced_answer answer) ]
      | Unsynced | Synced | Offline ->
          []
      )
    (fun state event ->
      match (state, event) with
      | (Unsynced | Offline), Sync ->
          Go Syncing
      | Syncing, Synced_answer (Ok _) ->
          Go Synced
      | Syncing, Synced_answer _ ->
          Go Offline
      | _ ->
          Ignore
    )

let test_missing_retry_waits () =
  let report =
    Check.run
      (Check.component
         ~samples:[ Check.answers save_endpoint [ 1 ] ]
         ~properties:
           [
             Check.leads_to ~name:"sync leads to synced"
               ~from:(Check.event (( = ) Sync))
               ~to_:(Check.state_named "synced")
               ();
           ]
         (Component.make syncer (function
           | Unsynced | Offline ->
               button Sync "Sync"
           | Syncing ->
               Node.text "syncing"
           | Synced ->
               Node.text "synced"
           ))
      )
  in
  let violation = only_violation report in
  Alcotest.(check string) "waits offline" "offline" violation.state_name;
  ( match violation.counterexample with
  | Waits steps ->
      Alcotest.(check (list string))
        "a shortest run to the waiting state"
        [ "sync"; "answer http 500" ]
        (events steps)
  | Path _ | Loop _ ->
      Alcotest.failf "expected waits:\n%s" (print report)
  );
  Alcotest.(check bool)
    "the hint says what to change" true
    (contains violation.hint "Make state offline own a timer or a call");
  Alcotest.(check bool)
    "the printed report says that the app waits" true
    (contains (print report) "the app waits for the user")

type list_event = Delete of int | Refresh
type items = Items of int list [@@deriving state_shape]

let lister ~stale =
  Machine.make ~id:"items"
    ~init:(Items [ 1; 2 ])
    ~shape:items_shape
    ~events:
      (Machine.shape
         ~name:(function
           | Delete number ->
               Printf.sprintf "delete %d" number
           | Refresh ->
               "refresh"
           )
         ()
      )
    ~owns:(fun _ -> [ Source.after 1000 Refresh ])
    (fun (Items numbers) -> function
      | Delete number ->
          Go (Items (List.filter (( <> ) number) numbers))
      | Refresh ->
          if stale then
            Go (Items [ 1; 2 ])
          else
            Go (Items numbers)
      )

let list_view (Items numbers) =
  Node.element "ul" []
    (List.map
       (fun number ->
         Node.element "li" []
           [
             Node.text (Printf.sprintf "item %d" number);
             button (Delete number) (Printf.sprintf "Delete %d" number);
           ]
       )
       numbers
    )

let deleted_check ~stale =
  Check.component
    ~properties:
      [
        Check.never_after ~name:"a deleted item never shows again"
          ~after:(Check.event (( = ) (Delete 1)))
          ~never:
            (Check.view (fun view ->
                 contains (View_text.to_string view) "item 1"
             )
            )
          ();
      ]
    (Component.make (lister ~stale) list_view)

let test_never_after_breach () =
  let report = Check.run (deleted_check ~stale:true) in
  let violation = only_violation report in
  Alcotest.(check string)
    "rule" "a deleted item never shows again" violation.rule;
  match violation.counterexample with
  | Path steps ->
      Alcotest.(check (list string))
        "delete, then the refresh brings it back" [ "delete 1"; "refresh" ]
        (events steps);
      Alcotest.(check bool)
        "the view of the failing state shows it" true
        (contains violation.view "item 1")
  | Waits _ | Loop _ ->
      Alcotest.failf "expected a path:\n%s" (print report)

let test_never_after_holds () =
  let report = Check.run (deleted_check ~stale:false) in
  if not (Check.passed report) then Alcotest.fail (print report)

type heartbeat_event = Beat | Timeout
type heartbeat = Connected | Expired [@@deriving state_shape]

let heartbeat =
  Machine.make ~id:"heartbeat" ~init:Connected ~shape:heartbeat_shape
    ~events:
      (Machine.shape ~name:(function Beat -> "beat" | Timeout -> "timeout") ())
    ~final:(function Expired -> true | Connected -> false)
    ~owns:(function
      | Connected ->
          [ Source.after 1000 Beat; Source.after 5000 Timeout ]
      | Expired ->
          []
      )
    (fun state event ->
      match (state, event) with
      | Connected, Beat ->
          Reenter Connected
      | Connected, Timeout ->
          Go Expired
      | Expired, _ ->
          Ignore
    )

let test_restart_loop_is_unfair () =
  let report =
    Check.run
      (Check.component
         ~properties:
           [
             Check.leads_to ~name:"connected leads to expired"
               ~from:(Check.state_named "connected")
               ~to_:(Check.state_named "expired")
               ();
           ]
         (Component.make heartbeat (fun _ -> Node.text "link"))
      )
  in
  if not (Check.passed report) then Alcotest.fail (print report)

type counting = Counting of int | Done [@@deriving state_shape]
type tick = Tick

let counter =
  Machine.make ~id:"counter" ~init:(Counting 0) ~shape:counting_shape
    ~events:(Machine.shape ~name:(fun Tick -> "tick") ())
    ~final:(function Done -> true | Counting _ -> false)
    ~owns:(function Counting _ -> [ Source.after 100 Tick ] | Done -> [])
    (fun state Tick ->
      match state with
      | Counting count when count >= 10 ->
          Go Done
      | Counting count ->
          Go (Counting (count + 1))
      | Done ->
          Ignore
    )

let counts_to_done =
  Check.leads_to ~name:"counting leads to done"
    ~from:(Check.state_named "counting")
    ~to_:(Check.state_named "done") ()

let test_cut_search_has_no_false_waits () =
  let counter = Component.make counter (fun _ -> Node.text "count") in
  let cut =
    Check.run
      (Check.component ~max_states:5 ~properties:[ counts_to_done ] counter)
  in
  Alcotest.(check bool) "max_states cut the search" false cut.complete;
  Alcotest.(check int)
    "no violation at the edge of the search" 0
    (List.length cut.violations);
  let bounded =
    Check.run
      (Check.component
         ~within:(function Counting count -> count < 3 | Done -> true)
         ~properties:[ counts_to_done ] counter
      )
  in
  Alcotest.(check int)
    "no violation where within cuts" 0
    (List.length bounded.violations);
  let truncated =
    Check.run
      (Check.component ~max_violations:1
         ~properties:
           [
             Check.always ~name:"below two"
               (Check.state (function
                 | Counting count ->
                     count < 2
                 | Done ->
                     true
                 ));
             Check.always ~name:"below three"
               (Check.state (function
                 | Counting count ->
                     count < 3
                 | Done ->
                     true
                 ));
             counts_to_done;
           ]
         counter
      )
  in
  Alcotest.(check bool) "max_violations cut the search" true truncated.truncated;
  Alcotest.(check (list string))
    "only the violation that stopped the search" [ "below two" ]
    (List.map
       (fun (violation : _ Check.violation) -> violation.rule)
       truncated.violations
    );
  let full =
    Check.run (Check.component ~properties:[ counts_to_done ] counter)
  in
  if not (Check.passed full) then Alcotest.fail (print full)

let test_step_conditions () =
  let report =
    Check.run
      (Check.component
         ~samples:[ Check.answers save_endpoint [ 1 ] ]
         ~properties:
           [
             Check.never_after ~name:"failing is not entered after a save"
               ~after:(Check.event (( = ) Save))
               ~never:(Check.entered "failed") ();
             Check.always ~name:"no call starts"
               (Check.not_
                  (Check.action (function
                    | Action.Start_call _ ->
                        true
                    | _ ->
                        false
                    ))
               );
           ]
         (Component.make (saver ~lost:false) save_view)
      )
  in
  Alcotest.(check (list (pair string string)))
    "the call start breaks the always property in saving, and the entry into \
     failed breaks the never_after property"
    [
      ("no call starts", "saving");
      ("failing is not entered after a save", "failed");
    ]
    (List.map
       (fun (violation : _ Check.violation) ->
         (violation.rule, violation.state_name)
       )
       report.violations
    );
  let entered = List.nth report.violations 1 in
  Alcotest.(check (list string))
    "save, then a failed answer"
    [ "save"; "answer http 500" ]
    (events (Check.steps entered.counterexample))

let () =
  Alcotest.run "temporal"
    [
      ( "leads_to",
        [
          Alcotest.test_case "a debounce holds" `Quick test_debounce_holds;
          Alcotest.test_case "a save that settles holds" `Quick test_save_holds;
          Alcotest.test_case "a lost answer gives a loop" `Quick
            test_lost_answer_loops;
          Alcotest.test_case "a missing retry gives waits" `Quick
            test_missing_retry_waits;
          Alcotest.test_case "a restart loop is unfair and passes" `Quick
            test_restart_loop_is_unfair;
          Alcotest.test_case "a cut search has no false waits" `Quick
            test_cut_search_has_no_false_waits;
        ]
      );
      ( "never_after",
        [
          Alcotest.test_case "a deleted item shows again" `Quick
            test_never_after_breach;
          Alcotest.test_case "a deleted item stays away" `Quick
            test_never_after_holds;
          Alcotest.test_case "step conditions" `Quick test_step_conditions;
        ]
      );
    ]
