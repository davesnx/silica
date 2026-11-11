open Linear_checks

let print_report pp_event report =
  Format.asprintf "%a" (fun f r -> Check.pp_report ~pp_event f r) report

let print_component_report pp_event report =
  print_report (Check.pp_input pp_event) report

let report name = function
  | Check.Ok { states; transitions; not_explored; _ } ->
      Printf.printf "%s: %d states, %d transitions, not explored: [%s]\n" name
        states transitions
        (String.concat "; " not_explored)
  | _ ->
      ()

let router_report = lazy (router ())

let children () =
  match Lazy.force router_report with
  | Check.Ok { children; _ } ->
      children
  | r ->
      Alcotest.fail (print_report (Check.pp_app_event pp_router_event) r)

let test_router () =
  match Lazy.force router_report with
  | Check.Ok { states; transitions; not_explored; _ } as r ->
      report "router" r;
      Alcotest.(check bool)
        "explored something" true
        (states > 1 && transitions > 1);
      Alcotest.(check (list string))
        "the pages, the create button, the user and the theme are listed, not \
         explored"
        [
          "component create-issue";
          "component issue-detail";
          "component issue-list";
          "component me";
          "component theme";
        ]
        not_explored
  | r ->
      Alcotest.fail (print_report (Check.pp_app_event pp_router_event) r)

let test_props_from_the_router () =
  let children = children () in
  Alcotest.(check (list int))
    "the detail gets the numbers of the visited routes" [ 1; 2; 9 ]
    (List.sort compare (Check.props children Linear_detail.component));
  Alcotest.(check (list string))
    "the create modal gets open and closed"
    [ "props false"; "props false filtered"; "props true" ]
    (List.sort compare
       (List.map
          (Format.asprintf "%a" pp_create_props)
          (Check.props children Linear_create.component)
       )
    )

let test_list () =
  match list () with
  | Check.Ok _ as r ->
      report "list" r
  | r ->
      Alcotest.fail (print_component_report pp_list_event r)

let test_detail () =
  match
    detail ~props:(Check.props (children ()) Linear_detail.component) ()
  with
  | Check.Ok _ as r ->
      report "detail" r
  | r ->
      Alcotest.fail (print_component_report pp_detail_event r)

let test_create () =
  match
    create ~props:(Check.props (children ()) Linear_create.component) ()
  with
  | Check.Ok _ as r ->
      report "create" r
  | r ->
      Alcotest.fail (print_component_report pp_create_event r)

let test_create_dialog () =
  let create_children =
    match
      create ~props:(Check.props (children ()) Linear_create.component) ()
    with
    | Check.Ok { children; _ } ->
        children
    | r ->
        Alcotest.fail (print_component_report pp_create_event r)
  in
  let props =
    Check.props create_children Linear_create.dialog_component.component
  in
  Alcotest.(check (list string))
    "the create machine asks the dialog to open, to close and to stay while \
     sending"
    [ "props busy"; "props closed"; "props open" ]
    (List.sort_uniq compare (List.map dialog_props_label props));
  match create_dialog ~props () with
  | Check.Ok _ as r ->
      report "create dialog" r
  | r ->
      Alcotest.fail (print_component_report pp_dialog_event r)

let test_create_reaches_submitting () =
  let never =
    {
      Check.name = "never submitting";
      holds =
        (fun (state : Linear_create.state) _ ->
          match state with
          | Submitting _ ->
              false
          | Closed | Editing _ | Failed _ ->
              true
        );
    }
  in
  match
    create ~invariants:[ never ]
      ~props:(Check.props (children ()) Linear_create.component)
      ()
  with
  | Check.Violation { trace; _ } ->
      Alcotest.(check string)
        "shortest way to a send: the check starts with each props, so the open \
         dialog needs no props step"
        "title \"x\"; submit"
        (String.concat "; "
           (List.map
              (Format.asprintf "%a" (Check.pp_input pp_create_event))
              trace
           )
        )
  | r ->
      Alcotest.fail (print_component_report pp_create_event r)

let test_me () =
  match me () with
  | Check.Ok { states; _ } as r ->
      report "me" r;
      Alcotest.(check int) "loading, ready, failed" 3 states
  | r ->
      Alcotest.fail (print_component_report pp_me_event r)

let alone name pp check () =
  match check () with
  | Check.Ok _ as r ->
      report name r
  | r ->
      Alcotest.fail (print_component_report pp r)

let spec name = function
  | Check.Ok { spec; _ } ->
      spec
  | _ ->
      Alcotest.failf "the check of %s did not pass" name

let test_comment_of_another_author () =
  match comment ~props:[ (mine, false) ] () with
  | Check.Unreached { states } ->
      Alcotest.(check (list string))
        "a comment of another author is never edited"
        [ "editing"; "saving"; "edited" ]
        states
  | r ->
      Alcotest.fail (print_component_report pp_comment_event r)

let test_lint () =
  let open Silica in
  let create_report =
    create ~props:(Check.props (children ()) Linear_create.component) ()
  in
  let create_children =
    match create_report with
    | Check.Ok { children; _ } ->
        children
    | _ ->
        Alcotest.fail "the check of create-issue did not pass"
  in
  let issues =
    List.concat_map Spec.lint
      [
        spec "router" (Lazy.force router_report);
        spec "list" (list ());
        spec "detail"
          (detail ~props:(Check.props (children ()) Linear_detail.component) ());
        spec "create" create_report;
        spec "create dialog"
          (create_dialog
             ~props:
               (Check.props create_children
                  Linear_create.dialog_component.component
               )
             ()
          );
        spec "me" (me ());
        spec "comments" (comments ());
        spec "composer" (composer ());
        spec "comment" (comment ());
        spec "unsaved" (unsaved ());
      ]
  in
  Alcotest.(check (list string))
    "no unreachable state; the only dead end is the ready list, which stays \
     ready for the life of the page (a lost connection is stale data)"
    [ "issue-list.ready: non-final dead end" ]
    (List.map Spec.issue_to_string issues)

let () =
  Alcotest.run "linear_check"
    [
      ( "mini linear",
        [
          Alcotest.test_case "router" `Quick test_router;
          Alcotest.test_case "props from the router" `Quick
            test_props_from_the_router;
          Alcotest.test_case "list alone" `Quick test_list;
          Alcotest.test_case "detail alone" `Quick test_detail;
          Alcotest.test_case "unsaved alone" `Quick
            (alone "unsaved" pp_unsaved_event unsaved);
          Alcotest.test_case "create alone" `Quick test_create;
          Alcotest.test_case "create dialog alone" `Quick test_create_dialog;
          Alcotest.test_case "a send is reachable" `Quick
            test_create_reaches_submitting;
          Alcotest.test_case "me alone" `Quick test_me;
          Alcotest.test_case "comments alone" `Quick
            (alone "comments" pp_comments_event comments);
          Alcotest.test_case "composer alone" `Quick
            (alone "composer" pp_composer_event composer);
          Alcotest.test_case "comment alone" `Quick
            (alone "comment" pp_comment_event (fun () -> comment ()));
          Alcotest.test_case "a comment of another author" `Quick
            test_comment_of_another_author;
          Alcotest.test_case "every machine passes lint" `Quick test_lint;
        ]
      );
    ]
