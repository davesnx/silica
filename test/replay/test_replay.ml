open Silica
module Memory = Silica_memory

type in_memory = Same_view | Not_realisable | Raised

type case =
  | Case : {
      check : ('s, 'ev) Check.t;
      mount : unit -> unit Memory.t;
      expected : (string * string * in_memory) list;
    }
      -> case

let cases =
  [
    Case
      {
        check = Save.check;
        mount = (fun () -> Memory.mount Save.component);
        expected = [ ("ignored-event", "saved", Same_view) ];
      };
    Case
      {
        check = Nameless.check;
        mount = (fun () -> Memory.mount Nameless.component);
        expected = [ ("accessible-name", "editing", Same_view) ];
      };
    Case
      {
        check = Crash.check;
        mount = (fun () -> Memory.mount Crash.component);
        expected = [ ("raised", "ready", Raised) ];
      };
    Case
      {
        check = Upload.check;
        mount = (fun () -> Memory.mount Upload.component);
        expected =
          [
            ("short-error", "failed", Same_view);
            ("ignored-event", "failed", Same_view);
          ];
      };
    Case
      {
        check = Clock.check;
        mount = (fun () -> Memory.mount Clock.component);
        expected =
          [
            ("accessible-name", "timed_out", Not_realisable);
            ("ignored-event", "ready", Same_view);
          ];
      };
    Case
      {
        check = Lost.check;
        mount = (fun () -> Memory.mount Lost.component);
        expected = [ ("save leads to saved or failed", "saving", Same_view) ];
      };
    Case
      {
        check = Sync.check;
        mount = (fun () -> Memory.mount Sync.component);
        expected = [ ("sync leads to synced", "offline", Same_view) ];
      };
    Case
      {
        check = Pages.check;
        mount = (fun () -> Memory.mount_app ~url:"/" Pages.app);
        expected =
          [
            ("img-alt", "about", Same_view);
            ("home stays behind", "home", Same_view);
            ("forward stays back", "secret", Same_view);
          ];
      };
    Case
      {
        check = Stepper.typing;
        mount = (fun () -> Memory.mount Stepper.component);
        expected = [ ("the step is positive", "counting", Same_view) ];
      };
    Case
      {
        check = Search.check;
        mount = (fun () -> Memory.mount Search.component);
        expected = [ ("ignored-event", "done", Same_view) ];
      };
    Case
      {
        check = Feed.check;
        mount = (fun () -> Memory.mount Feed.component);
        expected =
          [
            ("a new score is not offline", "watching", Same_view);
            ("a lost feed keeps the last score", "watching", Same_view);
          ];
      };
  ]

let contains text part =
  let length = String.length part in
  let rec go index =
    index + length <= String.length text
    && (String.sub text index length = part || go (index + 1))
  in
  go 0

let round_trip trace =
  match Trace.of_jsonl (Trace.to_jsonl trace) with
  | Ok trace ->
      trace
  | Error message ->
      Alcotest.fail ("the exported trace does not read: " ^ message)

let last_point = function
  | Check.Replayed points -> (
      match List.rev points with
      | last :: _ ->
          last
      | [] ->
          Alcotest.fail "the replay has no point"
    )
  | Not_enabled { at; _ } ->
      Alcotest.failf "step %d of the replay is not enabled" at

let describe_memory = function
  | Memory.Replayed ->
      "replayed"
  | Different { seq; expected; got } ->
      Printf.sprintf "different at %d: expected %s, got %s" seq expected got
  | Not_realisable { seq; reason } ->
      Printf.sprintf "not realisable at %d: %s" seq reason
  | Not_found { seq; error } ->
      Printf.sprintf "not found at %d: %s" seq error

let each_violation () =
  List.iter
    (fun (Case { check; mount; expected }) ->
      let report = Check.run check in
      Alcotest.(check (list (pair string string)))
        (Check.id check ^ ": the violations")
        (List.map (fun (rule, state, _) -> (rule, state)) expected)
        (List.map
           (fun (violation : (_, _) Check.violation) ->
             (violation.rule, violation.state_name)
           )
           report.violations
        );
      List.iter2
        (fun (violation : (_, _) Check.violation) (_, _, in_memory) ->
          let label = Check.id check ^ " " ^ violation.rule in
          let trace =
            round_trip
              (Check.export check ~rule:violation.rule violation.counterexample)
          in
          ( match trace.header.origin with
          | Checker { rule; _ } ->
              Alcotest.(check string) (label ^ ": origin") violation.rule rule
          | Runtime _ ->
              Alcotest.fail (label ^ ": the origin is the runtime")
          );
          Alcotest.(check int)
            (label ^ ": a start and each step")
            (1 + List.length (Check.steps violation.counterexample))
            (List.length trace.transitions);
          ( match Check.replay_trace check trace with
          | Error message ->
              Alcotest.fail (label ^ ": " ^ message)
          | Ok replay ->
              let last = last_point replay in
              Alcotest.(check (list string))
                (label ^ ": the rule fails at the last step of the replay")
                [ violation.rule ]
                (List.filter
                   (fun rule -> rule = violation.rule)
                   (List.map
                      (fun (failure : Check.failure) -> failure.rule)
                      last.failing
                   )
                );
              Alcotest.(check string)
                (label ^ ": the state of the last step")
                violation.state_name last.state_name
          );
          let page = mount () in
          let result = Memory.replay page trace in
          match in_memory with
          | Same_view ->
              Alcotest.(check string)
                (label ^ ": replayed in memory")
                "replayed" (describe_memory result);
              Alcotest.(check string)
                (label ^ ": the same view in memory")
                violation.view (Memory.view_text page)
          | Raised ->
              Alcotest.(check bool)
                (label ^ ": the step raised in memory, so no transition")
                true
                ( match result with
                | Different { got; _ } ->
                    String.starts_with ~prefix:"no transition" got
                | _ ->
                    false
                )
          | Not_realisable ->
              Alcotest.(check bool)
                (label ^ ": not realisable in memory: " ^ describe_memory result)
                true
                (match result with Not_realisable _ -> true | _ -> false)
        )
        report.violations expected
    )
    cases

let points_of label = function
  | Check.Replayed points ->
      points
  | Not_enabled { at; _ } ->
      Alcotest.failf "%s: step %d is not enabled" label at

let replay_steps () =
  match Check.replay Save.check [ Event Save.Edit; Event Edit; Event Save ] with
  | Not_enabled _ ->
      Alcotest.fail "the inputs are enabled"
  | Replayed points ->
      Alcotest.(check (list string))
        "the start and each step"
        [ "editing"; "editing"; "editing"; "saved" ]
        (List.map
           (fun (point : (_, _) Check.replayed_step) -> point.state_name)
           points
        );
      Alcotest.(check (list string))
        "the cause of each step"
        [
          "the view offers press on button \"Edit\" at form > button[1]";
          "the view offers press on button \"Edit\" at form > button[1]";
          "the view offers press on button \"Save\" at form > button[2]";
        ]
        (List.filter_map
           (fun (point : (_, _) Check.replayed_step) ->
             Option.map
               (fun (step : _ Check.step) -> Check.describe_cause step.cause)
               point.step
           )
           points
        )

let not_enabled () =
  match Check.replay Save.check [ Event Save.Save; Event Edit ] with
  | Replayed _ ->
      Alcotest.fail "Edit is not offered in the saved state"
  | Not_enabled { at; steps; enabled; _ } ->
      Alcotest.(check int) "the step" 2 at;
      Alcotest.(check int) "the points before it" 2 (List.length steps);
      Alcotest.(check (list string))
        "what the state offers" [ "save" ]
        (List.map (fun (step : _ Check.step) -> step.event) enabled)

let steps_of replay =
  List.filter_map
    (fun (point : (_, _) Check.replayed_step) -> point.step)
    (points_of "steps" replay)

let loop_and_waits () =
  let edits =
    steps_of
      (Check.replay Save.check [ Event Save.Edit; Event Edit; Event Edit ])
  in
  let stem, cycle =
    match edits with
    | first :: rest ->
        ([ first ], rest)
    | [] ->
        Alcotest.fail "no steps"
  in
  let trace =
    round_trip (Check.export Save.check ~rule:"edits" (Loop { stem; cycle }))
  in
  Alcotest.(check (option int))
    "the cycle starts after the stem" (Some 3) trace.header.loop_from;
  ( match Check.replay_trace Save.check trace with
  | Error message ->
      Alcotest.fail message
  | Ok replay ->
      let points = points_of "loop" replay in
      let state index =
        (List.nth points index : (_, _) Check.replayed_step).state
      in
      Alcotest.(check bool)
        "the cycle returns to its first state" true
        (state 1 = state (List.length points - 1));
      Alcotest.(check bool) "and moves in between" true (state 1 <> state 2)
  );
  let saves = steps_of (Check.replay Save.check [ Event Save.Save ]) in
  let trace =
    round_trip (Check.export Save.check ~rule:"waits" (Waits saves))
  in
  Alcotest.(check (option int)) "no cycle" None trace.header.loop_from;
  match Check.replay_trace Save.check trace with
  | Error message ->
      Alcotest.fail message
  | Ok replay ->
      Alcotest.(check string)
        "the waiting state" "saved" (last_point replay).state_name

let trace_errors () =
  let trace =
    Check.export Save.check ~rule:"r"
      (Path (steps_of (Check.replay Save.check [ Event Save.Save ])))
  in
  ( match Check.replay_trace Upload.check trace with
  | Error message ->
      Alcotest.(check string)
        "another machine"
        "the trace is a run of save, and the check is of upload" message
  | Ok _ ->
      Alcotest.fail "a trace of another machine replays"
  );
  let broken =
    {
      trace with
      transitions =
        List.map
          (fun (record : Transition.t) ->
            if record.seq = 2 then
              { record with data = Some (Codec.constructor "Unknown" []) }
            else
              record
          )
          trace.transitions;
    }
  in
  match Check.replay_trace Save.check broken with
  | Error message ->
      Alcotest.(check bool)
        ("names the transition: " ^ message)
        true
        (String.starts_with ~prefix:"transition 2:" message)
  | Ok _ ->
      Alcotest.fail "data that does not decode replays"

let no_codecs () =
  let report = Check.run Nameless.check in
  let violation = List.hd report.violations in
  let trace =
    Check.export Nameless.check ~rule:violation.rule violation.counterexample
  in
  Alcotest.(check (list bool))
    "no event data" [ false; false ]
    (List.map
       (fun (record : Transition.t) -> Option.is_some record.data)
       trace.transitions
    );
  match Check.replay_trace Nameless.check trace with
  | Error message ->
      Alcotest.fail message
  | Ok replay ->
      Alcotest.(check string)
        "matched by event name and cause" "editing"
        (last_point replay).state_name

let answer_data () =
  let report = Check.run Upload.check in
  let violation = List.hd report.violations in
  let trace =
    Check.export Upload.check ~rule:violation.rule violation.counterexample
  in
  Alcotest.(check (list string))
    "the answer of the call"
    [ {|["Rejected","too many"]|} ]
    (List.filter_map
       (fun (record : Transition.t) ->
         match record.cause with
         | Answer { answer = Some answer; path = "/api/save"; _ } ->
             Some (Silica_json.Json.to_string answer)
         | _ ->
             None
       )
       trace.transitions
    )

let runtime_trace () =
  let page = Memory.mount Upload.component in
  Memory.press (Memory.by_role page Accessibility.Button ~name:"Save");
  ( match Memory.pending page with
  | [ request ] ->
      Memory.reply ~status:422 request {|"too many"|}
  | _ ->
      Alcotest.fail "one request"
  );
  let recorded = Memory.trace page in
  let again = Memory.mount Upload.component in
  Alcotest.(check string)
    "a recorded run replays" "replayed"
    (describe_memory (Memory.replay again recorded));
  Alcotest.(check string)
    "to the same view" (Memory.view_text page) (Memory.view_text again);
  let other = Memory.mount Save.component in
  Alcotest.(check bool)
    "a run of another component does not" true
    (match Memory.replay other recorded with Different _ -> true | _ -> false)

let real_time () =
  let page = Memory.mount Clock.component in
  Memory.press (Memory.by_role page Accessibility.Button ~name:"Start");
  Memory.advance page 300;
  let recorded = Memory.trace page in
  let again = Memory.mount Clock.component in
  Alcotest.(check string)
    "the timer replays by the time left on it" "replayed"
    (describe_memory (Memory.replay again recorded));
  Alcotest.(check int) "at the time it fired" 300 (Memory.now again)

let script_errors () =
  let report = Check.run Clock.check in
  let violation = List.hd report.violations in
  let trace =
    Check.export Clock.check ~rule:violation.rule violation.counterexample
  in
  match Check.script Clock.check trace with
  | Ok _ ->
      Alcotest.fail "the long timer cannot fire first in real time"
  | Error message ->
      Alcotest.(check string)
        "says why"
        "step 2 fires the timer clock/waiting/after 2000ms (2000 ms) before \
         the timer clock/waiting/after 300ms, which is due 1700 ms earlier in \
         real time. The checker lets running timers fire in any order; \
         Silica_memory keeps real time, so it cannot replay this order"
        message

let props () =
  let report = Check.run Labelled.check in
  let violation =
    match report.violations with
    | [ violation ] ->
        violation
    | violations ->
        Alcotest.failf "%d violations" (List.length violations)
  in
  let trace =
    round_trip
      (Check.export Labelled.check ~rule:violation.rule violation.counterexample)
  in
  Alcotest.(check (list string))
    "the start with the props that break the rule" [ {|""|} ]
    (List.filter_map
       (fun (record : Transition.t) ->
         Option.map Silica_json.Json.to_string record.props
       )
       trace.transitions
    );
  let page = Memory.mount_with_props ~props:"" Labelled.component in
  Alcotest.(check string)
    "the start replays" "replayed"
    (describe_memory (Memory.replay page trace));
  Alcotest.(check string) "the same view" violation.view (Memory.view_text page);
  let steps = steps_of (Check.replay Labelled.check [ Props "" ]) in
  let trace =
    round_trip (Check.export Labelled.check ~rule:"accessible-name" (Path steps))
  in
  let page = Memory.mount_with_props ~props:"Go" Labelled.component in
  Alcotest.(check string)
    "new props replay" "replayed"
    (describe_memory (Memory.replay page trace));
  Alcotest.(check string)
    "and give the view of the props" violation.view (Memory.view_text page)

let not_found () =
  let steps =
    steps_of (Check.replay Nameless.check [ Event Nameless.Edit; Event Done ])
  in
  let trace = Check.export Nameless.check ~rule:"edit" (Path steps) in
  let page = Memory.mount Nameless.fixed in
  match Memory.replay page trace with
  | Not_found { seq; error } ->
      Alcotest.(check int) "the press of the button with no name" 3 seq;
      Alcotest.(check bool)
        ("lists the view: " ^ error)
        true
        (String.ends_with ~suffix:"button \"Done\" [press]\n" error)
  | result ->
      Alcotest.fail (describe_memory result)

let export_not_enabled () =
  let save = steps_of (Check.replay Save.check [ Event Save.Save ]) in
  let edit = steps_of (Check.replay Save.check [ Event Save.Edit ]) in
  Alcotest.check_raises "a step the check does not offer"
    (Invalid_argument
       "Check.export: step 2 of the counterexample is not enabled in the check"
    ) (fun () -> ignore (Check.export Save.check ~rule:"r" (Path (save @ edit)))
  )

let stepper_trace () =
  let violation = List.hd (Check.run Stepper.check).violations in
  ( violation,
    round_trip
      (Check.export Stepper.check ~rule:violation.rule violation.counterexample)
  )

let password_check =
  Check.component ~texts:[ "hunter2" ]
    ~properties:
      [
        Check.always ~name:"never sent"
          (Check.state (fun (state : Silica_test_support.Password_form.state) ->
               not state.sent
           )
          );
      ]
    Silica_test_support.Password_form.component

let password_masked_in_exports () =
  Silica_test_support.Password_form.register ();
  let violation =
    List.find
      (fun (violation : (_, _) Check.violation) -> violation.rule = "never sent")
      (Check.run password_check).violations
  in
  Alcotest.(check bool)
    "the view of the violation shows the mask" true
    (contains violation.view {|value="••••••••"|});
  Alcotest.(check bool)
    "the view of the violation has no typed text" false
    (contains violation.view "hunter2");
  let exported =
    Check.export password_check ~rule:violation.rule violation.counterexample
  in
  let text = Trace.to_text exported and jsonl = Trace.to_jsonl exported in
  Alcotest.(check bool)
    "the trace text has no typed text" false (contains text "hunter2");
  Alcotest.(check bool)
    "the trace data has no typed text" false (contains jsonl "hunter2");
  ( match Check.replay_trace password_check (round_trip exported) with
  | Error message ->
      Alcotest.fail message
  | Ok replay ->
      Alcotest.(check (list string))
        "the masked trace replays in the check to the failure" [ "never sent" ]
        (List.filter
           (fun rule -> rule = "never sent")
           (List.map
              (fun (failure : Check.failure) -> failure.rule)
              (last_point replay).failing
           )
        )
  );
  match
    Memory.replay
      (Memory.mount Silica_test_support.Password_form.component)
      exported
  with
  | Not_realisable { reason; _ } ->
      Alcotest.(check bool)
        "memory does not type the mask" true (contains reason "masked")
  | other ->
      Alcotest.fail ("memory replayed the mask: " ^ describe_memory other)

let exact_without_codecs () =
  let violation, trace = stepper_trace () in
  Alcotest.(check (list bool))
    "the trace holds no event data" [ false; false ]
    (List.map
       (fun (record : Transition.t) -> Option.is_some record.data)
       trace.transitions
    );
  match Check.replay_trace Stepper.check trace with
  | Error message ->
      Alcotest.fail message
  | Ok replay ->
      let last = last_point replay in
      Alcotest.(check (list string))
        "the typed 0 of the counterexample, not the first typed input"
        [ violation.rule ]
        (List.map (fun (failure : Check.failure) -> failure.rule) last.failing)

let never_a_false_pass () =
  let _, trace = stepper_trace () in
  let names_only =
    {
      trace with
      header = { trace.header with origin = Runtime { platform = "browser" } };
    }
  in
  ( match Check.replay_trace Stepper.check names_only with
  | Ok _ ->
      Alcotest.fail "a trace with no data and no choices replayed"
  | Error message ->
      Alcotest.(check bool)
        ("says why: " ^ message) true
        (String.starts_with
           ~prefix:"transition 2 (typed) has no event data and no choice"
           message
        )
  );
  let other_build =
    {
      trace with
      header =
        {
          trace.header with
          origin =
            Checker
              { rule = "the step is positive"; start = 0; choices = [ 7 ] };
        };
    }
  in
  match Check.replay_trace Stepper.check other_build with
  | Ok _ ->
      Alcotest.fail "a choice that the check does not offer replayed"
  | Error message ->
      Alcotest.(check bool)
        ("says why: " ^ message) true
        (String.starts_with
           ~prefix:
             "step 1 of the trace is move 7 with the event typed, which this \
              check does not offer there, and 2 moves"
           message
        )

let typing_cause () =
  let violation = List.hd (Check.run Stepper.typing).violations in
  Alcotest.(check (list string))
    "typed into the named field"
    [ "the view offers input \"0\" on spinbutton \"Step size\" at div > input" ]
    (List.map
       (fun (step : _ Check.step) -> Check.describe_cause step.cause)
       (Check.steps violation.counterexample)
    );
  let trace =
    Check.export Stepper.typing ~rule:violation.rule violation.counterexample
  in
  match Check.script Stepper.typing trace with
  | Error message ->
      Alcotest.fail message
  | Ok text ->
      Alcotest.(check bool)
        ("the script types into the field:\n" ^ text)
        true
        (contains text
           {|Memory.input (Memory.by_role page Accessibility.Spinbutton ~name:"Step size") "0"|}
        )

let props_steps_without_codecs () =
  let violation = List.hd (Check.run Rows.changing_check).violations in
  let trace =
    round_trip
      (Check.export Rows.changing_check ~rule:violation.rule
         violation.counterexample
      )
  in
  Alcotest.(check (list string))
    "two props steps" [ "props"; "props" ]
    (List.filter_map
       (fun (record : Transition.t) ->
         match record.cause with Props -> Some "props" | _ -> None
       )
       trace.transitions
    );
  match Check.replay_trace Rows.changing_check trace with
  | Error message ->
      Alcotest.fail message
  | Ok replay ->
      Alcotest.(check (list string))
        "the rule fails at the last step" [ violation.rule ]
        (List.map
           (fun (failure : Check.failure) -> failure.rule)
           (last_point replay).failing
        )

let app_limit_hint () =
  let report =
    Check.run
      (Check.app ~max_states:2
         ~routes:[ Pages.Home; About; Secret ]
         ~parse:Pages.parse ~print:Pages.print
         ~url_event:(fun url -> Silica.Router.Url_changed url)
         Pages.app
      )
  in
  Alcotest.(check bool) "incomplete" false report.complete;
  Alcotest.(check bool)
    ("names max_history: " ^ report.bounds)
    true
    (contains report.bounds "~max_history (default 4)")

let () =
  Alcotest.run "replay"
    [
      ( "replay",
        [
          Alcotest.test_case
            "each violation: export, replay in the check and in memory" `Quick
            each_violation;
          Alcotest.test_case "inputs give steps with causes" `Quick replay_steps;
          Alcotest.test_case
            "a typed password is masked in views and exports and still replays \
             in the check"
            `Quick password_masked_in_exports;
          Alcotest.test_case "an input the state does not offer" `Quick
            not_enabled;
          Alcotest.test_case "a loop and a wait export and replay" `Quick
            loop_and_waits;
          Alcotest.test_case "a trace of another machine or bad data" `Quick
            trace_errors;
          Alcotest.test_case "a machine with no codecs replays by names" `Quick
            no_codecs;
          Alcotest.test_case "an export holds the answer of a call" `Quick
            answer_data;
          Alcotest.test_case "a run recorded in memory replays in memory" `Quick
            runtime_trace;
          Alcotest.test_case "a timer replays at its time" `Quick real_time;
          Alcotest.test_case "a script refuses an order real time forbids"
            `Quick script_errors;
          Alcotest.test_case "props at the start and as a step" `Quick props;
          Alcotest.test_case "an element that is not in the view" `Quick
            not_found;
          Alcotest.test_case "the checker types texts into a named field" `Quick
            typing_cause;
          Alcotest.test_case "a checker trace replays exactly with no codec"
            `Quick exact_without_codecs;
          Alcotest.test_case "a trace that cannot replay faithfully is an error"
            `Quick never_a_false_pass;
          Alcotest.test_case "the limit hint of an app names max_history" `Quick
            app_limit_hint;
          Alcotest.test_case "props steps replay with no codec" `Quick
            props_steps_without_codecs;
          Alcotest.test_case "export of a step that is not enabled" `Quick
            export_not_enabled;
        ]
      );
    ]
