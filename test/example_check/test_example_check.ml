open Example_checks

let print_report report = Format.asprintf "%a" Check.pp_report report

let print name (report : (_, _) Check.report) =
  Printf.printf "%s: %d states, %d transitions\n" name report.states
    report.transitions

let expect_pass name report =
  if Check.passed report then
    print name report
  else
    Alcotest.fail (print_report report)

let router_report = lazy (Check.run (router ()))
let home_report = lazy (Check.run (home ()))

let test_router () =
  let report = Lazy.force router_report in
  expect_pass "router" report;
  Alcotest.(check (list string))
    "pages are listed, not explored"
    [
      "component about";
      "component edit";
      "component home";
      "component item";
      "component not_found";
    ]
    report.not_explored

let test_home () =
  let report = Lazy.force home_report in
  if not (Check.passed report) then Alcotest.fail (print_report report);
  Alcotest.(check int) "one state" 1 report.states;
  Alcotest.(check (list string))
    "the other components and the island are not run in the page check"
    [ "component todo"; "component vote"; "component votes"; "island counter" ]
    report.not_explored

let test_todo () = expect_pass "todo" (Check.run (todo ()))

let item_props () =
  Check.props (children (Lazy.force router_report)) Example_item.component

let test_item_props () =
  Alcotest.(check (list int))
    "the router gives the visited numbers" [ 1; 2 ]
    (List.sort compare (item_props ()))

let test_item_alone () =
  let report = Check.run (item ~props:(item_props ()) ()) in
  expect_pass "item" report;
  Alcotest.(check (list string)) "no children" [] report.not_explored

let test_item_reachable () =
  let never_loaded =
    Check.always ~name:"never loaded"
      (Check.view (fun view -> not (has_article view)))
  in
  let report =
    Check.run (item ~properties:[ never_loaded ] ~props:(item_props ()) ())
  in
  match report.violations with
  | first :: _ ->
      Alcotest.(check string) "the first violation" "never loaded" first.rule;
      Alcotest.(check string)
        "shortest trace" "settled ok"
        (String.concat "; "
           (List.map
              (fun (step : _ Check.step) ->
                Format.asprintf "%a" pp_item_input step.input
              )
              (Check.steps first.counterexample)
           )
        )
  | [] ->
      Alcotest.fail (print_report report)

let test_about () =
  let report = Check.run (about ()) in
  expect_pass "about" report;
  Alcotest.(check int) "one state" 1 report.states

let test_edit () =
  let report = Check.run (edit ()) in
  expect_pass "edit" report;
  Alcotest.(check bool)
    "clean, dirty and asking are reached" true (report.states >= 3);
  Alcotest.(check bool)
    "a navigation attempt is asked" true
    (List.exists
       (fun (label : Silica.Spec.label) ->
         label.from = "dirty" && label.target = "asking"
         && label.event = "attempt"
       )
       (List.hd report.specs).edges
    )

let test_not_found () =
  let props =
    Check.props
      (children (Lazy.force router_report))
      Example_not_found.component
  in
  Alcotest.(check int) "one not-found page from the router" 1 (List.length props);
  expect_pass "not_found" (Check.run (not_found ()))

let test_votes () = expect_pass "votes" (Check.run (votes ()))
let test_vote () = expect_pass "vote" (Check.run (vote ()))

let round_trip name codec value =
  let text = Silica.Codec.to_string codec value in
  Alcotest.(check bool)
    (name ^ " " ^ text ^ " round trips")
    true
    (Silica.Codec.of_string codec text = Ok value)

let codec name = function
  | Ok codec ->
      codec
  | Error message ->
      Alcotest.failf "%s: %s" name message

let round_trips name (machine : (unit, _, _) Silica.Machine.t) check =
  let event_codec = codec name (Silica.Machine.event_codec machine)
  and state_codec = codec name (Silica.Machine.state_codec machine) in
  let report = Check.run check in
  if not (Check.passed report) then
    Alcotest.failf "the check of %s did not pass" name;
  Alcotest.(check bool) (name ^ " has paths") true (report.paths <> []);
  List.iter
    (fun (_, steps) ->
      let start, _ = Silica.Machine.start machine () in
      round_trip name state_codec start;
      ignore
        (List.fold_left
           (fun state (step : (unit, _) Check.input Check.step) ->
             match step.input with
             | Event event ->
                 round_trip name event_codec event;
                 let state, _, _ = Silica.Machine.step machine () state event in
                 round_trip name state_codec state;
                 state
             | Props () ->
                 state
           )
           start steps
        )
    )
    report.paths

let test_every_machine_registered () =
  Example_codecs.register ();
  let directory = "../../example/app/shared" in
  Alcotest.(check int)
    "machines whose events derive JSON" 4
    (List.length (Silica_test_support.Codec_coverage.required directory));
  Alcotest.(check (list string))
    "each one is in Example_codecs" []
    (Silica_test_support.Codec_coverage.missing directory)

let test_codecs () =
  Example_codecs.register ();
  round_trips "about" Example_about.machine (about ());
  round_trips "votes" Example_votes.machine (votes ());
  round_trips "vote" Example_vote_button.machine (vote ());
  round_trips "counter" Example_counter.machine (counter_bounded ());
  let item_events =
    codec "item" (Silica.Machine.event_codec Example_item.machine)
  and item_states =
    codec "item" (Silica.Machine.state_codec Example_item.machine)
  in
  let sample =
    {
      Example_model.Item.id = 1;
      title = "Item 1";
      price_cents = 1999;
      tags = [ "a" ];
    }
  in
  List.iter
    (round_trip "item" item_events)
    Example_item.
      [
        Settled (Ok sample);
        Settled (Rejected "no item");
        Settled (Failed (Http 500));
      ];
  List.iter
    (round_trip "item" item_states)
    Example_item.
      [ Loading; Loaded sample; Missing "no item"; Failed (Network "down") ];
  let todo_events =
    codec "todo" (Silica.Machine.event_codec Example_todo.machine)
  and todo_states =
    codec "todo" (Silica.Machine.state_codec Example_todo.machine)
  in
  List.iter
    (round_trip "todo" todo_events)
    Example_todo.[ Draft "milk"; Add; Toggle 1; Delete 1 ];
  round_trip "todo" todo_states
    {
      Example_todo.todos = [ { id = 1; text = "milk"; completed = true } ];
      draft = "eggs";
      next_id = 2;
    }

let test_counter () =
  let report =
    Check.run (Check.component ~max_states:50 Example_counter.component)
  in
  Alcotest.(check int) "no violation" 0 (List.length report.violations);
  Alcotest.(check bool) "expected limit" false report.complete;
  Alcotest.(check int) "unbounded upward" 50 report.states

let test_counter_bounded () =
  let report = Check.run (counter_bounded ()) in
  expect_pass "counter" report;
  Alcotest.(check int) "states" 6 report.states

let () =
  Alcotest.run "example_check"
    [
      ( "example",
        [
          Alcotest.test_case "router" `Quick test_router;
          Alcotest.test_case "home page" `Quick test_home;
          Alcotest.test_case "todo alone" `Slow test_todo;
          Alcotest.test_case "item props from the router" `Quick test_item_props;
          Alcotest.test_case "item alone" `Quick test_item_alone;
          Alcotest.test_case "loaded item is reachable" `Quick
            test_item_reachable;
          Alcotest.test_case "about alone" `Quick test_about;
          Alcotest.test_case "edit alone" `Quick test_edit;
          Alcotest.test_case "not found alone" `Quick test_not_found;
          Alcotest.test_case "votes alone" `Quick test_votes;
          Alcotest.test_case "vote button alone" `Quick test_vote;
          Alcotest.test_case "counter hits the limit" `Quick test_counter;
          Alcotest.test_case "counter bounded" `Quick test_counter_bounded;
          Alcotest.test_case "events and states round trip through the codecs"
            `Quick test_codecs;
          Alcotest.test_case "the codec module registers every machine" `Quick
            test_every_machine_registered;
        ]
      );
    ]
