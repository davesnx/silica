open Example_checks

let print_report pp_event report =
  Format.asprintf "%a" (fun f r -> Check.pp_report ~pp_event f r) report

let print name = function
  | Check.Ok { states; transitions; _ } ->
      Printf.printf "%s: %d states, %d transitions\n" name states transitions
  | _ ->
      ()

let router_report = lazy (router ())
let home_report = lazy (home ())

let test_router () =
  match Lazy.force router_report with
  | Check.Ok { not_explored; _ } as r ->
      print "router" r;
      Alcotest.(check (list string))
        "pages are listed, not explored"
        [
          "component about";
          "component edit";
          "component home";
          "component item";
          "component not_found";
        ]
        not_explored
  | r ->
      Alcotest.fail (print_report (Check.pp_app_event pp_router_event) r)

let test_home () =
  match Lazy.force home_report with
  | Check.Ok { states; not_explored; _ } ->
      Alcotest.(check int) "one state" 1 states;
      Alcotest.(check (list string))
        "the other components and the island are not run in the page check"
        [
          "component todo";
          "component vote";
          "component votes";
          "island counter";
        ]
        not_explored
  | r ->
      Alcotest.fail
        (print_report (fun f _ -> Format.pp_print_string f "home event") r)

let test_todo () =
  match todo () with
  | Check.Ok _ as r ->
      print "todo" r
  | r ->
      Alcotest.fail
        (print_report (fun f _ -> Format.pp_print_string f "todo event") r)

let item_props () =
  Check.props (children (Lazy.force router_report)) Example_item.component

let test_item_props () =
  Alcotest.(check (list int))
    "the router gives the visited numbers" [ 1; 2 ]
    (List.sort compare (item_props ()))

let test_item_alone () =
  match item ~props:(item_props ()) () with
  | Check.Ok { not_explored; _ } as r ->
      Alcotest.(check (list string)) "no children" [] not_explored;
      print "item" r
  | r ->
      Alcotest.fail (print_report pp_item_input r)

let test_item_reachable () =
  let never_loaded =
    {
      Check.name = "never loaded";
      holds = (fun _ view -> not (has_article view));
    }
  in
  match item ~invariants:[ never_loaded ] ~props:(item_props ()) () with
  | Check.Violation { trace; _ } ->
      Alcotest.(check string)
        "shortest trace" "settled ok"
        (String.concat "; "
           (List.map (Format.asprintf "%a" pp_item_input) trace)
        )
  | r ->
      Alcotest.fail (print_report pp_item_input r)

let test_about () =
  match about () with
  | Check.Ok { states; _ } as r ->
      print "about" r;
      Alcotest.(check int) "one state" 1 states
  | r ->
      Alcotest.fail
        (print_report (fun f _ -> Format.pp_print_string f "about event") r)

let test_edit () =
  match edit () with
  | Check.Ok { states; edges; _ } as r ->
      print "edit" r;
      Alcotest.(check bool)
        "clean, dirty and asking are reached" true (states >= 3);
      Alcotest.(check bool)
        "a navigation attempt is asked" true
        (List.exists
           (fun (l : Silica.Spec.label) ->
             l.from = "dirty" && l.target = "asking" && l.event = "attempt"
           )
           edges
        )
  | r ->
      Alcotest.fail
        (print_report (fun f _ -> Format.pp_print_string f "edit event") r)

let test_not_found () =
  let props =
    Check.props
      (children (Lazy.force router_report))
      Example_not_found.component
  in
  Alcotest.(check int) "one not-found page from the router" 1 (List.length props);
  match not_found () with
  | Check.Ok _ as r ->
      print "not_found" r
  | r ->
      Alcotest.fail (print_report (fun f _ -> Format.pp_print_string f "url") r)

let test_votes () =
  match votes () with
  | Check.Ok _ as r ->
      print "votes" r
  | r ->
      Alcotest.fail (print_report (Check.pp_input pp_votes_event) r)

let test_vote () =
  match vote () with
  | Check.Ok _ as r ->
      print "vote" r
  | r ->
      Alcotest.fail
        (print_report (fun f _ -> Format.pp_print_string f "vote event") r)

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

let round_trips name (machine : (unit, _, _) Silica.Machine.t) report =
  let event_codec = codec name (Silica.Machine.event_codec machine)
  and state_codec = codec name (Silica.Machine.state_codec machine) in
  match report with
  | Check.Ok { paths; _ } ->
      Alcotest.(check bool) (name ^ " has paths") true (paths <> []);
      List.iter
        (fun (_, steps) ->
          let start, _ = Silica.Machine.start machine () in
          round_trip name state_codec start;
          ignore
            (List.fold_left
               (fun state (step : (unit, _) Check.input) ->
                 match step with
                 | Event event ->
                     round_trip name event_codec event;
                     let state, _, _ =
                       Silica.Machine.step machine () state event
                     in
                     round_trip name state_codec state;
                     state
                 | Props () ->
                     state
               )
               start steps
            )
        )
        paths
  | _ ->
      Alcotest.failf "the check of %s did not pass" name

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
  match Check.component ~max_states:50 Example_counter.component with
  | Check.Limit { states } ->
      Alcotest.(check int) "unbounded upward" 50 states
  | _ ->
      Alcotest.fail "expected limit"

let test_counter_bounded () =
  match counter_bounded () with
  | Check.Ok { states; _ } as r ->
      Alcotest.(check int) "states" 6 states;
      print "counter" r
  | r ->
      Alcotest.fail
        (print_report (fun f _ -> Format.pp_print_string f "event") r)

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
