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
  Alcotest.(check (list string))
    "the router gives the visited numbers" [ "props 1"; "props 2" ]
    (List.sort compare
       (List.map (Format.asprintf "%a" pp_item_event) (item_props ()))
    )

let test_item_alone () =
  match item ~props:(item_props ()) () with
  | Check.Ok { not_explored; _ } as r ->
      Alcotest.(check (list string)) "no children" [] not_explored;
      print "item" r
  | r ->
      Alcotest.fail (print_report pp_item_event r)

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
        "shortest trace" "props 1; settled ok"
        (String.concat "; "
           (List.map (Format.asprintf "%a" pp_item_event) trace)
        )
  | r ->
      Alcotest.fail (print_report pp_item_event r)

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
  Alcotest.(check int) "one url from the router" 1 (List.length props);
  match not_found ~props () with
  | Check.Ok _ as r ->
      print "not_found" r
  | r ->
      Alcotest.fail (print_report (fun f _ -> Format.pp_print_string f "url") r)

let test_votes () =
  match votes () with
  | Check.Ok _ as r ->
      print "votes" r
  | r ->
      Alcotest.fail (print_report pp_votes_event r)

let test_vote () =
  match vote () with
  | Check.Ok _ as r ->
      print "vote" r
  | r ->
      Alcotest.fail
        (print_report (fun f _ -> Format.pp_print_string f "vote event") r)

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
        ]
      );
    ]
