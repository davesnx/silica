open Silica
module Memory = Silica_memory

let listed ?(samples = []) ?(max_instances = 3) component =
  Check_model.Child
    {
      component;
      samples;
      inputs = (fun _ -> []);
      texts = [];
      within = (fun _ -> true);
      max_instances;
      names = [];
      check = (fun ~snapshot:_ ~view:_ ~event:_ ~name:_ -> []);
    }

let job_samples = [ Check_model.answers Job.endpoint [ "ok" ] ]
let keys = Check_rules.default_keys

let model () =
  Check_tree.joint
    ~own_actions:(ref (fun _ -> []))
    ~keys ~texts:[] ~arrivals:2
    ~own:(fun event : (unit, Board.event) Check.input -> Event event)
    ~own_of:(function
      | (Event event : (unit, Board.event) Check.input) ->
          Some event
      | Props _ | In_child _ ->
          None
      )
    ~into:(fun child_event : (unit, Board.event) Check.input ->
      In_child child_event
    )
    ~out_of:(function
      | (In_child child_event : (unit, Board.event) Check.input) ->
          Some child_event
      | Event _ | Props _ ->
          None
      )
    ~children:
      [ listed Row.component; listed ~samples:job_samples Job.component ]
    (Check_model.component_model ~client_only:false ~keys ~texts:[]
       ~inputs:(fun _ -> [])
       ~props:[] ~samples:[] ~arrivals:2 Board.component
    )

let id_at view path =
  match
    List.find_opt
      (fun element -> Accessibility.path element = path)
      (Accessibility.elements view)
  with
  | Some element -> (
      match Accessibility.attribute element "id" with
      | Some id ->
          id
      | None ->
          Alcotest.failf "the element at %s has no id" path
    )
  | None ->
      Alcotest.failf "no element at %s" path

let newest_job_request memory =
  match
    List.rev
      (List.filter
         (fun request -> Memory.path request = "/api/job")
         (Memory.pending memory)
      )
  with
  | request :: _ ->
      request
  | [] ->
      Alcotest.fail "no pending job request"

let perform (model : (_, _, _, _) Check_model.model) memory node
    (move : _ Check_model.move) =
  let view = model.view node in
  match (move.via, model.cause node view move) with
  | Check_model.By_view, Check_model.View { handler = "press"; element; _ } ->
      Memory.press (Memory.by_id memory (id_at view element))
  | Check_model.By_answer _, _ -> (
      match (move.event : (unit, Board.event) Check.input) with
      | In_child (Child_event { witness; event; _ }) -> (
          match
            Type.Id.provably_equal witness (Machine.type_id Job.machine)
          with
          | Some Type.Equal -> (
              let request = newest_job_request memory in
              match event with
              | Job.Answered (Ok value) ->
                  Memory.reply request
                    (Codec.to_string (Remote.output Job.endpoint) value)
              | Job.Answered (Failed (Remote.Http status)) ->
                  Memory.reply ~status request "boom"
              | Job.Answered (Failed (Remote.Network reason)) ->
                  Memory.network_error request reason
              | Job.Answered (Rejected _ | Failed (Remote.Decode _)) | Job.Start
                ->
                  Alcotest.fail "the fixture gives no such answer"
            )
          | None ->
              Alcotest.fail "an answer for a machine other than the job"
        )
      | _ ->
          Alcotest.fail "an answer that is not of a child"
    )
  | _, cause ->
      Alcotest.failf "the fixture has no step %s" (Check.describe_cause cause)

let memory_equals_checker () =
  let model = model () in
  let start =
    match model.start () with
    | [ (node, _) ] ->
        node
    | _ ->
        Alcotest.fail "one start expected"
  in
  let seen = Hashtbl.create 1024 in
  let queue = Queue.create () in
  Hashtbl.replace seen start ();
  Queue.add (start, []) queue;
  let compared = ref 0 in
  let props_changes = ref 0
  and unmounted_calls = ref 0
  and reorders = ref 0
  and finals = ref 0 in
  let replay path =
    let memory = Memory.mount Board.component in
    ignore
      (List.fold_left
         (fun at move ->
           perform model memory at move;
           let next, _, _ = model.step at move in
           next
         )
         start path
      );
    memory
  in
  let sent (joint : _ Check_tree.joint) =
    List.length
      (List.filter
         (fun (entry : Check_tree.running) -> entry.final_sent)
         joint.running
      )
  in
  Alcotest.(check string)
    "the start"
    (View_text.to_string (model.view start))
    (Memory.view_text (replay []));
  while not (Queue.is_empty queue) do
    let node, path = Queue.pop queue in
    List.iter
      (fun (move : _ Check_model.move) ->
        let next, _, how = model.step node move in
        if how <> Machine.Unhandled || Check_model.ignorable move.via then (
          let steps = List.rev (move :: path) in
          Alcotest.(check string)
            (Printf.sprintf "view after %d steps" (List.length steps))
            (View_text.to_string (model.view next))
            (Memory.view_text (replay steps));
          incr compared;
          ( match move.event with
          | Check.Event Board.Relabel when node.running <> [] ->
              incr props_changes
          | Check.Event Board.Reverse ->
              incr reorders
          | Check.Event Board.Toggle_job
            when List.exists
                   (fun (entry : Check_tree.running) ->
                     Check_model.state_of model.tree node entry.instance
                     = Some "running"
                     && Check_model.state_of model.tree next entry.instance
                        = None
                   )
                   node.running ->
              incr unmounted_calls
          | _ ->
              ()
          );
          if sent next > sent node then incr finals;
          if not (Hashtbl.mem seen next) then (
            Hashtbl.replace seen next ();
            Queue.add (next, move :: path) queue
          )
        )
      )
      (model.enabled node (model.view node))
  done;
  Alcotest.(check bool) "many joint snapshots" true (!compared > 100);
  Alcotest.(check bool) "props changes" true (!props_changes > 0);
  Alcotest.(check bool) "reorders" true (!reorders > 0);
  Alcotest.(check bool)
    "unmounts with a running call" true (!unmounted_calls > 0);
  Alcotest.(check bool) "finals" true (!finals > 0)

let board ?(row = Check.child ~max_instances:3 Row.component) () =
  Check.component
    ~children:
      [
        row;
        Check.child
          ~samples:[ Check.answers Job.endpoint [ "ok" ] ]
          Job.component;
      ]
    Board.component

let joint_check_passes () =
  let report = Check.run (board ()) in
  if not (Check.passed report) then Alcotest.failf "%a" Check.pp_report report;
  Alcotest.(check (list string))
    "machines" [ "board"; "row"; "job" ]
    (List.map fst report.machines);
  Alcotest.(check (list string))
    "specs" [ "board"; "row"; "job" ]
    (List.map (fun (spec : Spec.t) -> spec.id) report.specs);
  Alcotest.(check (list string)) "nothing left out" [] report.not_explored;
  Alcotest.(check string)
    "the text report starts with the entry id" "board+row+job: "
    (String.sub (Format.asprintf "%a" Check.pp_report report) 0 15)

let unlisted_child_is_not_explored () =
  let report =
    Check.run
      (Check.component ~children:[ Check.child Row.component ] Board.component)
  in
  Alcotest.(check bool)
    "the job is not explored" true
    (List.mem "component job" report.not_explored)

let instances_over_the_bound_do_not_run () =
  let report =
    Check.run (board ~row:(Check.child ~max_instances:1 Row.component) ())
  in
  Alcotest.(check bool)
    "the second row is listed" true
    (List.mem
       "component row (up to 2 instances were shown at once, and only the \
        first 1 ran, as ~max_instances:1 of its Check.child says; the others \
        showed their start view and took no step. Raise ~max_instances to 2 to \
        run them all; the ~within of the parent bounds its own state, not how \
        many children run)"
       report.not_explored
    )

let contains text needle =
  let length = String.length needle in
  let rec scan index =
    index + length <= String.length text
    && (String.sub text index length = needle || scan (index + 1))
  in
  scan 0

let violation_names_the_instance () =
  let row =
    Check.child ~max_instances:3
      ~properties:
        [
          Check.always ~name:"rows never finish" (Check.state_named "counting");
        ]
      Row.component
  in
  let report = Check.run (board ~row ()) in
  match
    List.filter
      (fun (violation : (_, _) Check.violation) ->
        violation.rule = "rows never finish"
      )
      report.violations
  with
  | violation :: _ ->
      Alcotest.(check bool)
        "the message names the instance" true
        (contains violation.message "In the child row at instance board/");
      let steps = Check.steps violation.counterexample in
      Alcotest.(check bool)
        "a row took a step" true
        (List.exists
           (fun (step : _ Check.step) ->
             step.machine = "row" && step.instance <> "0" && step.event = "bump"
           )
           steps
        );
      Alcotest.(check (list string))
        "the last step goes to done" [ "done" ]
        (match List.rev steps with step :: _ -> [ step.target ] | [] -> [])
  | [] ->
      Alcotest.fail "no violation of the row property"

module Finisher = struct
  type event = Finish

  let component =
    Component.make
      (Machine.make ~id:"finisher" ~init:()
         ~name:(fun () -> "ready")
         ~states:[ "ready" ]
         ~event_name:(fun Finish -> "finish")
         (fun () Finish -> Machine.Stay [])
      )
      (fun () ->
        Node.element "button" [ Attr.on_press Finish ] [ Node.text "Finish" ]
      )
end

module Waiter = struct
  type event = Finished

  let component =
    Component.make
      (Machine.make ~id:"waiter" ~init:false
         ~name:(fun complete ->
           if complete then
             "complete"
           else
             "waiting"
         )
         ~states:[ "waiting"; "complete" ]
         ~event_name:(fun Finished -> "finished")
         (fun _ Finished -> Machine.Go true)
      )
      (fun _ ->
        Component.node
          ~output:(fun Finisher.Finish _ -> [ Finished ])
          Finisher.component
      )
end

let a_child_step_names_the_move_of_the_root () =
  let report =
    Check.run
      (Check.component
         ~children:[ Check.child Finisher.component ]
         ~properties:
           [ Check.always ~name:"never complete" (Check.state_named "waiting") ]
         Waiter.component
      )
  in
  match report.violations with
  | violation :: _ ->
      let steps = Check.steps violation.counterexample in
      Alcotest.(check (list (list (pair string (pair string string)))))
        "the press in the child moves the root"
        [ [ ("0", ("waiting", "complete")) ] ]
        (List.map
           (fun (step : _ Check.step) ->
             List.map
               (fun (move : Check.move) ->
                 (move.instance, (move.from, move.target))
               )
               step.moved
           )
           steps
        );
      Alcotest.(check bool)
        "the line names the move of the root" true
        (List.for_all
           (fun step ->
             contains
               (Check.describe_step ~root:"waiter" step)
               "; then waiter: waiting -> complete"
           )
           steps
        )
  | [] ->
      Alcotest.fail "no violation"

let a_child_step_shows_the_root_event_of_its_output () =
  let report =
    Check.run
      (Check.component
         ~children:[ Check.child Finisher.component ]
         ~properties:
           [
             Check.always ~name:"complete only after finished"
               Check.(
                 not_ (entered "complete")
                 ||| event (fun Waiter.Finished -> true)
               );
             Check.always ~name:"finished completes"
               Check.(
                 not_ (entered "complete")
                 ||| transition (fun before Waiter.Finished after ->
                     (not before) && after
                 )
               );
           ]
         Waiter.component
      )
  in
  if not (Check.passed report) then Alcotest.failf "%a" Check.pp_report report

module Unlabelled = struct
  type event = Typed of string

  let component =
    Component.make
      (Machine.make ~id:"unlabelled" ~init:()
         ~name:(fun () -> "open")
         ~states:[ "open" ]
         ~event_name:(fun (Typed _) -> "typed")
         (fun () (Typed _) -> Machine.Stay [])
      )
      (fun () ->
        Node.element "label" []
          [ Node.element "input" [ Attr.on_input (fun text -> Typed text) ] [] ]
      )
end

module Unlabelled_page = struct
  type event = Never

  let component =
    Component.make
      (Machine.make ~id:"unlabelled-page" ~init:()
         ~name:(fun () -> "editing")
         ~states:[ "editing" ]
         ~event_name:(fun Never -> "never")
         (fun () Never -> Machine.Stay [])
      )
      (fun () ->
        Node.element "main" []
          [
            Node.element "h1" [] [ Node.text "Profile" ];
            Node.element "button" [ Attr.on_press Never ] [ Node.text "Save" ];
            Component.node Unlabelled.component;
          ]
      )
end

let a_view_rule_names_the_child_that_renders_the_element () =
  let report =
    Check.run
      (Check.component
         ~children:[ Check.child Unlabelled.component ]
         Unlabelled_page.component
      )
  in
  match
    List.find_opt
      (fun (violation : (_, _) Check.violation) ->
        violation.rule = "field-label"
      )
      report.violations
  with
  | Some violation ->
      Alcotest.(check string)
        "the child and its state"
        "In the child unlabelled at instance unlabelled-page/2:unlabelled: In \
         state open, the textbox field at main > label > input has no label: \
         the <label> around it at main > label has no text."
        violation.message
  | None ->
      Alcotest.failf "no field-label violation:@.%a" Check.pp_report report

let text_of view = View_text.to_string view

let temporal_properties_hold_on_the_parent () =
  let report =
    Check.run
      (Check.component
         ~children:
           [
             Check.child ~max_instances:3 Row.component;
             Check.child
               ~samples:[ Check.answers Job.endpoint [ "ok" ] ]
               Job.component;
           ]
         ~properties:
           [
             Check.always ~name:"the board runs no action of its own"
               (Check.not_ (Check.action (fun _ -> true)));
             Check.leads_to ~name:"an own action leads to a job"
               ~from:(Check.action (fun _ -> true))
               ~to_:(Check.view (fun view -> contains (text_of view) "job"))
               ();
             Check.leads_to ~name:"a running job leads to its end"
               ~from:(Check.view (fun view -> contains (text_of view) "running"))
               ~to_:
                 (Check.view (fun view ->
                      not (contains (text_of view) "running")
                  )
                 )
               ();
           ]
         Board.component
      )
  in
  if not (Check.passed report) then Alcotest.failf "%a" Check.pp_report report

let round_trip trace =
  match Trace.of_jsonl (Trace.to_jsonl trace) with
  | Ok trace ->
      trace
  | Error message ->
      Alcotest.fail ("the exported trace does not read: " ^ message)

let last_failing label = function
  | Check.Replayed points -> (
      match List.rev points with
      | (last : (_, _) Check.replayed_step) :: _ ->
          ( last.state_name,
            List.map
              (fun (failure : Check.failure) -> failure.rule)
              last.failing
          )
      | [] ->
          Alcotest.fail (label ^ ": no point")
    )
  | Not_enabled { at; _ } ->
      Alcotest.failf "%s: step %d is not enabled" label at

let temporal_violation_replays () =
  let rule = "a bumped row finishes" in
  let report = Check.run Joint_cases.bumped in
  match
    List.filter
      (fun (violation : (_, _) Check.violation) -> violation.rule = rule)
      report.violations
  with
  | [ violation ] -> (
      let steps = Check.steps violation.counterexample in
      Alcotest.(check bool)
        "a row step is in the counterexample" true
        (List.exists
           (fun (step : _ Check.step) ->
             step.machine = "row" && step.instance = "board/6.#1:row"
           )
           steps
        );
      let replayed =
        Check.replay Joint_cases.bumped
          (List.map (fun (step : _ Check.step) -> step.input) steps)
      in
      Alcotest.(check (pair string bool))
        "the replay of the inputs ends where the rule fails"
        (violation.state_name, true)
        (let state, failing = last_failing "replay" replayed in
         (state, List.mem rule failing)
        );
      let trace =
        round_trip
          (Check.export Joint_cases.bumped ~rule violation.counterexample)
      in
      Alcotest.(check (list string))
        "the trace names the machine of each step"
        ("board" :: List.map (fun (step : _ Check.step) -> step.machine) steps)
        (List.map
           (fun (record : Transition.t) -> record.machine)
           trace.transitions
        );
      ( match Check.replay_trace Joint_cases.bumped trace with
      | Error message ->
          Alcotest.fail message
      | Ok replayed ->
          Alcotest.(check (pair string bool))
            "the replay of the trace ends where the rule fails"
            (violation.state_name, true)
            (let state, failing = last_failing "trace" replayed in
             (state, List.mem rule failing)
            )
      );
      let page = Memory.mount Board.component in
      match Memory.replay page trace with
      | Memory.Replayed ->
          Alcotest.(check string)
            "the same view in memory" violation.view (Memory.view_text page)
      | Different { seq; expected; got } ->
          Alcotest.failf "memory differs at %d: expected %s, got %s" seq
            expected got
      | Not_realisable { seq; reason } ->
          Alcotest.failf "not realisable at %d: %s" seq reason
      | Not_found { seq; error } ->
          Alcotest.failf "not found at %d: %s" seq error
    )
  | violations ->
      Alcotest.failf "expected one violation, got %d:@.%a"
        (List.length violations) Check.pp_report report

let temporal_child_property_is_rejected () =
  match
    Check.child
      ~properties:
        [
          Check.leads_to ~name:"temporal"
            ~from:(Check.state_named "counting")
            ~to_:(Check.state_named "done") ();
        ]
      Row.component
  with
  | _ ->
      Alcotest.fail "a temporal child property was accepted"
  | exception Check.Invalid_check message ->
      Alcotest.(check bool)
        "the message names the property" true
        (contains message "temporal")

module Reach = struct
  type event = In_fieldset | Hidden | Inert | Plain_div | Enabled

  let name = function
    | In_fieldset ->
        "in fieldset"
    | Hidden ->
        "hidden"
    | Inert ->
        "inert"
    | Plain_div ->
        "plain div"
    | Enabled ->
        "enabled"

  let machine =
    Machine.make ~id:"reach" ~init:None
      ~name:(function None -> "none" | Some event -> name event)
      ~event_name:name
      (fun _ event -> Go (Some event))

  let button label event =
    Node.element "button" [ Attr.on_press event ] [ Node.text label ]

  let component =
    Component.make machine (fun _ ->
        Node.element "main" []
          [
            Node.element "fieldset"
              [ Attr.bool "disabled" true ]
              [ button "in fieldset" In_fieldset ];
            Node.element "div"
              [ Attr.bool "hidden" true ]
              [ button "hidden" Hidden ];
            Node.element "div"
              [ Attr.string "inert" "" ]
              [ button "inert" Inert ];
            Node.element "div"
              [
                Attr.string "role" "button";
                Attr.bool "disabled" true;
                Attr.on_press Plain_div;
              ]
              [ Node.text "plain div" ];
            button "enabled" Enabled;
          ]
    )
end

module Modal_child = struct
  type event = Close

  let component =
    Component.make
      (Machine.make ~id:"modal-child" ~init:true
         ~name:(fun shown ->
           if shown then
             "open"
           else
             "closed"
         )
         ~states:[ "open"; "closed" ]
         ~event_name:(fun Close -> "close")
         (fun _ Close -> Machine.Go false)
      )
      (fun shown ->
        if shown then
          Node.portal
            [
              Node.element "div"
                [
                  Attr.string "role" "dialog";
                  Attr.string "aria-modal" "true";
                  Attr.string "aria-label" "Edit";
                ]
                [
                  Node.element "button"
                    [ Attr.on_press Close ]
                    [ Node.text "Close" ];
                ];
            ]
        else
          Node.list []
      )
end

module Modal_page = struct
  type event = Tab

  let component =
    Component.make
      (Machine.make ~id:"modal-page" ~init:()
         ~name:(fun () -> "page")
         ~states:[ "page" ]
         ~event_name:(fun Tab -> "tab")
         (fun () Tab -> Machine.Stay [])
      )
      (fun () ->
        Node.element "main" []
          [
            Node.element "button" [ Attr.on_press Tab ] [ Node.text "Tab" ];
            Component.node Modal_child.component;
          ]
      )
end

let the_page_behind_a_child_modal_is_inert () =
  let check =
    Check.component
      ~children:[ Check.child Modal_child.component ]
      Modal_page.component
  in
  match Check.replay check [ Check.Event Modal_page.Tab ] with
  | Replayed _ ->
      Alcotest.fail "the tab behind the open modal of the child took a press"
  | Not_enabled { enabled; _ } ->
      Alcotest.(check (list string))
        "only the button of the modal is offered"
        [
          "[modal-child at modal-page/1:modal-child] open -> closed: close, by \
           the view offers press on button \"Close\" at portal > div > button";
        ]
        (List.map (Check.describe_step ~root:"modal-page") enabled)

let checker_offers_what_a_browser_delivers () =
  let report = Check.run (Check.component Reach.component) in
  Alcotest.(check (list string))
    "the events of the presses that reach their element"
    [ "enabled"; "plain div" ]
    (List.sort_uniq compare
       (List.map
          (fun (edge : Spec.label) -> edge.event)
          (List.hd report.specs).edges
       )
    );
  let page = Memory.mount Reach.component in
  List.iter
    (fun (label, reachable) ->
      let press () =
        match Memory.press (Memory.by_text page label) with
        | () ->
            true
        | exception Memory.Not_reachable _ ->
            false
      in
      Alcotest.(check bool) ("memory presses " ^ label) reachable (press ())
    )
    [
      ("in fieldset", false);
      ("hidden", false);
      ("inert", false);
      ("plain div", true);
      ("enabled", true);
    ]

let () =
  Alcotest.run "joint"
    [
      ( "joint",
        [
          Alcotest.test_case "memory renders what the checker sees" `Quick
            memory_equals_checker;
          Alcotest.test_case "a child step shows the root event of its output"
            `Quick a_child_step_shows_the_root_event_of_its_output;
          Alcotest.test_case "a view rule names the child that renders it"
            `Quick a_view_rule_names_the_child_that_renders_the_element;
          Alcotest.test_case "a joint check passes" `Quick joint_check_passes;
          Alcotest.test_case "a step of a child names the move of the root"
            `Quick a_child_step_names_the_move_of_the_root;
          Alcotest.test_case "an unlisted child is not explored" `Quick
            unlisted_child_is_not_explored;
          Alcotest.test_case "instances over the bound do not run" `Quick
            instances_over_the_bound_do_not_run;
          Alcotest.test_case "a violation in a child names the instance" `Quick
            violation_names_the_instance;
          Alcotest.test_case "temporal properties hold on the parent" `Quick
            temporal_properties_hold_on_the_parent;
          Alcotest.test_case "a temporal violation in a child replays" `Quick
            temporal_violation_replays;
          Alcotest.test_case "the page behind a child modal is inert" `Quick
            the_page_behind_a_child_modal_is_inert;
          Alcotest.test_case "the checker offers what a browser delivers" `Quick
            checker_offers_what_a_browser_delivers;
          Alcotest.test_case "a temporal child property is rejected" `Quick
            temporal_child_property_is_rejected;
        ]
      );
    ]
