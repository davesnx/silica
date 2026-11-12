open Linear_checks

let print_report report = Format.asprintf "%a" Check.pp_report report

let passed report =
  if not (Check.passed report) then Alcotest.fail (print_report report)

let report name (report : _ Check.report) =
  passed report;
  Printf.printf "%s: %d states, %d transitions, not explored: [%s]\n" name
    report.states report.transitions
    (String.concat "; " report.not_explored)

let router_report = lazy (router ())

let children () =
  let report = Lazy.force router_report in
  passed report;
  report.children

let test_router () =
  let routed = Lazy.force router_report in
  report "router" routed;
  let { Check.states; transitions; not_explored; _ } = routed in
  Alcotest.(check bool) "explored something" true (states > 1 && transitions > 1);
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

let test_list () = report "list" (list ())

let test_detail () =
  report "detail"
    (detail ~props:(Check.props (children ()) Linear_detail.component) ())

let create_report () =
  create ~props:(Check.props (children ()) Linear_create.component) ()

let test_create () = report "create" (create_report ())

let test_create_dialog () =
  let create_report = create_report () in
  passed create_report;
  let props =
    Check.props create_report.children Linear_create.dialog_component.component
  in
  Alcotest.(check (list string))
    "the create machine asks the dialog to open, to close and to stay while \
     sending"
    [ "props busy"; "props closed"; "props open" ]
    (List.sort_uniq compare (List.map dialog_props_label props));
  report "create dialog" (create_dialog ~props ())

let test_create_reaches_submitting () =
  let never =
    Check.always ~name:"never submitting"
      (Check.holds (fun (state : Linear_create.state) _ ->
           match state with
           | Submitting _ ->
               false
           | Closed | Editing _ | Failed _ ->
               true
       )
      )
  in
  let report =
    create ~properties:[ never ]
      ~props:(Check.props (children ()) Linear_create.component)
      ()
  in
  match report.violations with
  | first :: _
    when List.for_all
           (fun (violation : _ Check.violation) ->
             violation.rule = "never submitting"
           )
           report.violations ->
      Alcotest.(check string)
        "shortest way to a send: the check starts with each props, so the open \
         dialog needs no props step"
        "title \"x\"; submit"
        (String.concat "; "
           (List.map
              (fun (step : (_, _) Check.input Check.step) ->
                match step.input with
                | Check.Event event ->
                    Format.asprintf "%a" pp_create_event event
                | Check.Props _ ->
                    "props"
                | Check.In_child _ ->
                    "child"
              )
              (Check.steps first.counterexample)
           )
        )
  | _ ->
      Alcotest.fail (print_report report)

let test_me () =
  let me_report = me () in
  report "me" me_report;
  Alcotest.(check int) "loading, ready, failed" 3 me_report.states

let alone name check () = report name (check ())

let spec name (report : _ Check.report) =
  if not (Check.passed report) then
    Alcotest.failf "the check of %s did not pass" name;
  List.hd report.specs

let test_comment_of_another_author () =
  let report = comment ~props:[ (mine, false) ] () in
  match report with
  | { violations = []; complete = true; unreached; _ } ->
      Alcotest.(check (list string))
        "a comment of another author is never edited"
        [ "editing"; "saving"; "edited" ]
        unreached
  | _ ->
      Alcotest.fail (print_report report)

let list_report = lazy (list ())

let detail_report =
  lazy (detail ~props:(Check.props (children ()) Linear_detail.component) ())

let round_trip_path_events name machine (report : _ Check.report) =
  let codec =
    match Silica.Machine.event_codec machine with
    | Ok codec ->
        codec
    | Error message ->
        Alcotest.failf "%s: %s" name message
  in
  if not (Check.passed report) then
    Alcotest.failf "the check of %s did not pass" name;
  let events =
    List.concat_map
      (fun (_, steps) ->
        List.filter_map
          (fun (step : (_, _) Check.input Check.step) ->
            match step.input with
            | Check.Event event ->
                Some event
            | Check.Props _ | Check.In_child _ ->
                None
          )
          steps
      )
      report.paths
  in
  Alcotest.(check bool) (name ^ " has events on its paths") true (events <> []);
  List.iter
    (fun event ->
      let text = Silica.Codec.to_string codec event in
      Alcotest.(check bool)
        (name ^ " event " ^ text ^ " round trips")
        true
        (Silica.Codec.of_string codec text = Ok event)
    )
    events

let round_trip_states name machine states =
  let codec =
    match Silica.Machine.state_codec machine with
    | Ok codec ->
        codec
    | Error message ->
        Alcotest.failf "%s: %s" name message
  in
  List.iter
    (fun state ->
      let text = Silica.Codec.to_string codec state in
      Alcotest.(check bool)
        (name ^ " state " ^ text ^ " round trips")
        true
        (Silica.Codec.of_string codec text = Ok state)
    )
    states

let test_every_machine_registered () =
  Linear_codecs.register ();
  let directory = "../../linear/app/shared" in
  Alcotest.(check int)
    "machines whose events derive JSON" 7
    (List.length (Silica_test_support.Codec_coverage.required directory));
  Alcotest.(check (list string))
    "each one is in Linear_codecs" []
    (Silica_test_support.Codec_coverage.missing directory)

let test_codecs () =
  Linear_codecs.register ();
  round_trip_path_events "list" Linear_list.machine (Lazy.force list_report);
  round_trip_path_events "detail" Linear_detail.machine
    (Lazy.force detail_report);
  round_trip_path_events "create" Linear_create.machine (create_report ());
  round_trip_path_events "me" Linear_me.machine (me ());
  round_trip_path_events "comments" Linear_comments.machine (comments ());
  round_trip_path_events "composer" Linear_composer.machine (composer ());
  round_trip_path_events "comment" Linear_comment.machine (comment ());
  round_trip_states "me" Linear_me.machine
    Linear_me.[ Loading; Ready Linear_model.User.Ada; Failed ];
  round_trip_states "comments" Linear_comments.machine
    Linear_comments.
      [
        Loading;
        Ready { viewer = Linear_model.User.Ben; comments = [ mine ] };
        Failed;
      ];
  round_trip_states "composer" Linear_composer.machine
    Linear_composer.
      [
        Empty;
        Drafting "x";
        Sending "x";
        Failed { body = "x"; message = "The comment is too long" };
      ];
  round_trip_states "comment" Linear_comment.machine
    Linear_comment.
      [
        Viewing;
        Editing { draft = "x"; error = Some "Only the author can edit" };
        Saving "x";
        Edited mine;
      ];
  round_trip_states "create" Linear_create.machine
    Linear_create.
      [
        Closed;
        Editing { draft = Linear_model.Draft.empty; error = None };
        Submitting Linear_model.Draft.empty;
        Failed
          { draft = Linear_model.Draft.empty; message = "Title is required" };
      ]

let test_lint () =
  let open Silica in
  let create_report = create_report () in
  let issues =
    List.concat_map Spec.lint
      [
        spec "router" (Lazy.force router_report);
        spec "list" (Lazy.force list_report);
        spec "detail" (Lazy.force detail_report);
        spec "create" create_report;
        spec "create dialog"
          (create_dialog
             ~props:
               (Check.props create_report.children
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
    "no unreachable state; the only dead ends are the ready list and the ready \
     detail, which stay ready for the life of the page (a lost connection is \
     stale data, another issue number is shown in place)"
    [
      "issue-list.ready: non-final dead end";
      "issue-detail.ready: non-final dead end";
    ]
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
          Alcotest.test_case "list toasts, three saves" `Quick (fun () ->
              report "list toasts" (list_toasts ())
          );
          Alcotest.test_case "detail alone" `Quick test_detail;
          Alcotest.test_case "unsaved alone" `Quick (alone "unsaved" unsaved);
          Alcotest.test_case "create alone" `Quick test_create;
          Alcotest.test_case "create dialog alone" `Quick test_create_dialog;
          Alcotest.test_case "a send is reachable" `Quick
            test_create_reaches_submitting;
          Alcotest.test_case "me alone" `Quick test_me;
          Alcotest.test_case "comments alone" `Quick (alone "comments" comments);
          Alcotest.test_case "composer alone" `Quick (alone "composer" composer);
          Alcotest.test_case "comment alone" `Quick
            (alone "comment" (fun () -> comment ()));
          Alcotest.test_case "a comment of another author" `Quick
            test_comment_of_another_author;
          Alcotest.test_case "every machine passes lint" `Quick test_lint;
          Alcotest.test_case "events and states round trip through the codecs"
            `Quick test_codecs;
          Alcotest.test_case "the codec module registers every machine" `Quick
            test_every_machine_registered;
        ]
      );
    ]
