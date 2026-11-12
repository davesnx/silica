open Silica
open Silica_test_support
open Linear_model

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let has msg html needle =
  if not (contains html needle) then
    Alcotest.failf "%s: %S not found in %s" msg needle html

let lacks msg html needle =
  if contains html needle then
    Alcotest.failf "%s: %S found in %s" msg needle html

let strings = Alcotest.(list string)

let issue ?(title = "A title") ?(status = Issue_status.Todo) number : Issue.t =
  {
    number;
    title;
    description = "";
    status;
    priority = Priority.Medium;
    assignee = None;
    labels = [];
    created_at = 0;
    updated_at = 0;
    version = 1;
  }

let issues =
  [ issue 1 ~title:"First"; issue 2 ~title:"Second"; issue 3 ~title:"Third" ]

let ready seq data : Issues.t Topic.update =
  { seq; status = Status.Ready { data; stale = false } }

let offline data : Issues.t Topic.update =
  { seq = 0; status = Status.Ready { data; stale = true } }

let sim_app url = Sim.mount (Linear_app.app (Url.of_string url))

let answer_app sim =
  Sim.answer sim me_endpoint User.Ada;
  Sim.answer sim issues_endpoint issues

(* Store *)

let test_store_seed () =
  let store = Linear_store.create () in
  let all = Linear_store.issues store in
  Alcotest.(check int) "thirty issues" 30 (List.length all);
  Alcotest.(check (list int))
    "numbers 1 to 30"
    (List.init 30 (fun i -> i + 1))
    (List.map (fun (i : Issue.t) -> i.number) all);
  Alcotest.(check bool)
    "titles are not empty" true
    (List.for_all (fun (i : Issue.t) -> i.title <> "") all);
  List.iter
    (fun status ->
      Alcotest.(check bool)
        (Issue_status.label status ^ " is used")
        true
        (List.exists (fun (i : Issue.t) -> i.status = status) all)
    )
    Issue_status.all

let test_store_create () =
  let store = Linear_store.create () in
  let draft =
    {
      Draft.title = "  Fix the thing  ";
      description = "Details";
      status = Issue_status.In_progress;
      priority = Priority.Urgent;
    }
  in
  match Linear_store.create_issue store ~now:1000 draft with
  | Error message ->
      Alcotest.fail message
  | Ok created -> (
      Alcotest.(check int) "next number" 31 created.number;
      Alcotest.(check string) "title is trimmed" "Fix the thing" created.title;
      Alcotest.(check string) "description" "Details" created.description;
      Alcotest.(check bool)
        "status" true
        (created.status = Issue_status.In_progress);
      Alcotest.(check bool) "priority" true (created.priority = Priority.Urgent);
      Alcotest.(check bool) "no assignee" true (created.assignee = None);
      Alcotest.(check int) "created" 1000 created.created_at;
      Alcotest.(check int) "updated" 1000 created.updated_at;
      Alcotest.(check int) "version" 1 created.version;
      Alcotest.(check int) "stored" 31 (List.length (Linear_store.issues store));
      match Linear_store.create_issue store ~now:2000 draft with
      | Ok second ->
          Alcotest.(check int) "numbers keep growing" 32 second.number
      | Error message ->
          Alcotest.fail message
    )

let test_store_rejects () =
  let store = Linear_store.create () in
  let rejects name draft =
    match Linear_store.create_issue store ~now:0 draft with
    | Ok _ ->
        Alcotest.failf "%s: accepted" name
    | Error _ ->
        Alcotest.(check int)
          (name ^ ": nothing stored")
          30
          (List.length (Linear_store.issues store))
  in
  rejects "empty title" Draft.empty;
  rejects "blank title" { Draft.empty with title = " \n\t " };
  rejects "long title"
    { Draft.empty with title = String.make (Draft.max_title + 1) 'x' };
  rejects "long description"
    {
      Draft.empty with
      title = "ok";
      description = String.make (Draft.max_description + 1) 'x';
    }

let test_next_user () =
  let next = Linear_store.next_user () in
  let names = List.init 5 (fun _ -> User.name (next ())) in
  Alcotest.check strings "in turn" [ "Ada"; "Ben"; "Cleo"; "Ada"; "Ben" ] names

let test_dates () =
  let check expected seconds =
    Alcotest.(check string) expected expected (Linear_data.date_of_epoch seconds)
  in
  check "1970-01-01" 0;
  check "2000-02-29" 951782400;
  check "2026-11-05" 1793836800;
  check "2024-12-31" 1735603200;
  check "1999-12-31" 946598400

let test_model_codecs () =
  let sample =
    {
      (issue 7) with
      assignee = Some User.Cleo;
      labels = [ Label.Bug; Label.Docs ];
      description = "line 1\nline \"2\"";
    }
  in
  let codec = Remote.output issues_endpoint in
  let json = codec.Codec.encode [ sample ] in
  match codec.Codec.decode json with
  | Ok [ back ] ->
      Alcotest.(check bool) "round trip" true (back = sample)
  | Ok _ ->
      Alcotest.fail "wrong length"
  | Error message ->
      Alcotest.fail message

(* Live data *)

let test_data_apply () =
  let base = Linear_data.loaded issues in
  let one = [ issue 1 ] in
  let at stale =
    Linear_data.apply (offline one) (Linear_data.loaded one) |> fun t ->
    if stale then
      t
    else
      Linear_data.apply (ready 1 one) t
  in
  let check name expected actual =
    Alcotest.(check (pair (list int) bool))
      name
      ( List.map (fun (i : Issue.t) -> i.number) (Linear_data.issues expected),
        expected.stale
      )
      ( List.map (fun (i : Issue.t) -> i.number) (Linear_data.issues actual),
        actual.stale
      )
  in
  check "a pushed state replaces the loaded data" (at false)
    (Linear_data.apply (ready 3 one) base);
  check "a pushed state clears the stale flag" (at false)
    (Linear_data.apply (ready 3 one) (at true));
  check
    "a state with a lower seq replaces too: it comes from a new socket, the \
     socket already drops older states"
    (Linear_data.loaded [ issue 9 ])
    (Linear_data.apply (ready 1 [ issue 9 ]) (at false));
  check "offline keeps the data and sets stale" (at true)
    (Linear_data.apply (offline [ issue 9 ]) (at false));
  check "loading changes nothing" (at false)
    (Linear_data.apply { seq = 0; status = Status.Loading None } (at false));
  check "a failure keeps the data and sets stale" (at true)
    (Linear_data.apply
       {
         seq = 0;
         status = Status.Failed { error = Remote.Http 401; last = Some one };
       }
       (at false)
    )

(* List *)

let mount_list () =
  Sim.mount_with_props ~props:Linear_view.default Linear_list.component

let test_list_loading_then_ready () =
  let sim = mount_list () in
  has "loading" (Sim.html sim) "Loading issues";
  Alcotest.(check int) "one call running" 1 (List.length (Sim.running sim));
  Alcotest.(check int)
    "no subscription before the data" 0
    (List.length (Sim.subscriptions sim));
  Sim.answer sim issues_endpoint issues;
  let html = Sim.html sim in
  has "first" html "First";
  has "id" html "ENG-3";
  has "status" html "Todo";
  has "link" html "href=\"/issue/2\"";
  Alcotest.(check (list (pair string string)))
    "subscribed to the topic"
    [ ("issues", "all") ]
    (Sim.subscriptions sim);
  Alcotest.(check bool) "no offline note" false (contains html "Offline")

let index_of html needle =
  let n = String.length needle in
  let rec go i =
    if i + n > String.length html then
      Alcotest.failf "%S not found" needle
    else if String.sub html i n = needle then
      i
    else
      go (i + 1)
  in
  go 0

let test_list_newest_first () =
  let sim = mount_list () in
  Sim.answer sim issues_endpoint issues;
  let html = Sim.html sim in
  Alcotest.(check bool)
    "third before first" true
    (index_of html "Third" < index_of html "First")

let test_list_closed_issues_are_struck () =
  let sim = mount_list () in
  Sim.answer sim issues_endpoint
    [
      issue 1 ~title:"Open one";
      issue 2 ~title:"Done one" ~status:Issue_status.Done;
      issue 3 ~title:"Canceled one" ~status:Issue_status.Canceled;
    ];
  let html = Sim.html sim in
  has "open title has the plain class only" html
    "<span class=\"grow truncate\">Open one</span>";
  has "done is struck" html
    "<span class=\"grow truncate text-ui-muted line-through\">Done one</span>";
  has "canceled is struck" html
    "<span class=\"grow truncate text-ui-muted line-through\">Canceled \
     one</span>"

let test_list_live_updates () =
  let sim = mount_list () in
  Sim.answer sim issues_endpoint issues;
  Sim.publish sim issues_topic ~key:"all"
    (ready 2 (issues @ [ issue 4 ~title:"Fourth" ]));
  has "new issue appears" (Sim.html sim) "Fourth";
  Sim.publish sim issues_topic ~key:"all" (offline issues);
  has "offline note" (Sim.html sim) "Offline";
  has "old data stays" (Sim.html sim) "Fourth";
  Sim.publish sim issues_topic ~key:"all"
    (ready 1 (issues @ [ issue 5 ~title:"Fifth" ]));
  lacks "back online on a new socket whose topic starts again at seq 1"
    (Sim.html sim) "Offline";
  has "the state of the new socket replaces the data" (Sim.html sim) "Fifth"

let test_list_failure_and_retry () =
  let sim = mount_list () in
  Sim.answer_error sim issues_endpoint (Remote.Http 500);
  let html = Sim.html sim in
  has "error" html "The server answered 500";
  Alcotest.(check int) "no subscription" 0 (List.length (Sim.subscriptions sim));
  Sim.press sim "Try again";
  has "loading again" (Sim.html sim) "Loading issues";
  Sim.answer sim issues_endpoint issues;
  has "ready" (Sim.html sim) "First"

let test_list_unmount_stops () =
  let sim = mount_list () in
  Sim.answer sim issues_endpoint issues;
  Sim.unmount sim;
  Alcotest.(check int)
    "subscription stopped" 0
    (List.length (Sim.subscriptions sim))

let lost : Issues.t Topic.update =
  {
    seq = 0;
    status = Status.Failed { error = Remote.Http 403; last = Some issues };
  }

let test_list_reconnect_after_a_failure () =
  let sim = mount_list () in
  Sim.answer sim issues_endpoint issues;
  lacks "no reconnect while live" (Sim.html sim) "Reconnect";
  Sim.publish sim issues_topic ~key:"all" lost;
  has "offline note" (Sim.html sim) "Offline";
  has "the data stays" (Sim.html sim) "Third";
  Sim.clear_log sim;
  Sim.press sim "Reconnect";
  Alcotest.(check (list string))
    "the subscription stops, and starts again after a timer"
    [
      "stop 1.1/issue-list/ready/live:issues all";
      "start 1.1/issue-list/ready/after 0ms";
    ]
    (Sim.log sim);
  Sim.clear_log sim;
  Sim.fire sim "1.1/issue-list/ready/after 0ms";
  Alcotest.(check (list string))
    "the fired timer is gone; the subscription starts again"
    [ "start 1.1/issue-list/ready/live:issues all" ]
    (Sim.log sim);
  has "still offline until a state arrives" (Sim.html sim) "Offline";
  Sim.publish sim issues_topic ~key:"all" (ready 1 issues);
  lacks "live again" (Sim.html sim) "Offline"

(* Detail *)

let mount_detail number =
  Sim.mount_with_props ~props:number Linear_detail.component

let test_detail () =
  let sim = mount_detail 2 in
  has "loading" (Sim.html sim) "Loading issue";
  Sim.answer sim issues_endpoint issues;
  let html = Sim.html sim in
  has "id" html "ENG-2";
  has "title" html
    "<h1 class=\"mb-4 text-xl font-semibold\" \
     data-silica-route-focus=\"\">Second</h1>";
  has "no description" html "No description.";
  has "unassigned" html "Unassigned";
  has "date" html "1970-01-01";
  has "back link" html "href=\"/\"";
  Alcotest.(check (list (pair string string)))
    "live"
    [ ("issues", "all") ]
    (Sim.subscriptions sim);
  Alcotest.(check bool)
    "title action" true
    (List.mem "title ENG-2 Second" (Sim.log sim));
  Sim.publish sim issues_topic ~key:"all"
    (ready 2 [ issue 2 ~title:"Renamed"; issue 1 ]);
  has "live change" (Sim.html sim) "Renamed";
  lacks "old title gone" (Sim.html sim) ">Second<"

let test_detail_not_found () =
  let sim = mount_detail 99 in
  Sim.answer sim issues_endpoint issues;
  has "message" (Sim.html sim) "There is no issue ENG-99.";
  Alcotest.(check int) "no subscription" 0 (List.length (Sim.subscriptions sim))

let test_detail_reconnect_after_a_failure () =
  let sim = mount_detail 2 in
  Sim.answer sim issues_endpoint issues;
  Sim.answer sim comments_endpoint { Thread.viewer = User.Ada; comments = [] };
  lacks "no reconnect while live" (Sim.html sim) "Reconnect";
  Sim.press_id sim ~id:"status-picker-trigger";
  Sim.press_id sim ~id:(Silica_ui.Menu.item_id "status-picker" 3);
  let saving = Sim.call_inputs sim in
  Alcotest.(check int) "a change is saving" 1 (List.length saving);
  Sim.publish sim issues_topic ~key:"all" lost;
  has "offline note" (Sim.html sim) "Offline";
  Sim.clear_log sim;
  Sim.press sim "Reconnect";
  Alcotest.(check (list string))
    "the subscription stops, and starts again after a timer; the save goes on"
    [
      "stop 1.1/issue-detail/ready/live:issues all";
      "start 1.1/issue-detail/ready/after 0ms";
    ]
    (Sim.log sim);
  Sim.clear_log sim;
  Sim.fire sim "1.1/issue-detail/ready/after 0ms";
  Alcotest.(check (list string))
    "the fired timer is gone; the subscription starts again"
    [ "start 1.1/issue-detail/ready/live:issues all" ]
    (Sim.log sim);
  Alcotest.(check (list string))
    "the save was not sent again" saving (Sim.call_inputs sim);
  Sim.publish sim issues_topic ~key:"all" (ready 1 issues);
  lacks "live again" (Sim.html sim) "Offline"

let test_detail_failure_and_new_number () =
  let sim = Sim.mount_with_props ~props:1 Linear_detail.component in
  Sim.answer_error sim issues_endpoint (Remote.Network "down");
  has "error" (Sim.html sim) "The network failed";
  Sim.press sim "Try again";
  Sim.answer sim issues_endpoint issues;
  has "ready" (Sim.html sim) "First";
  Sim.set_props sim 3;
  lacks "no reload for the new number" (Sim.html sim) "Loading issue";
  has "the new number shows from the loaded issues" (Sim.html sim) "Third"

(* Create *)

let mount_create () =
  Sim.mount_with_props ~props:Linear_routes.list Linear_create.component

let open_create () =
  let sim = mount_create () in
  Sim.set_props sim { Linear_routes.list with create = true };
  sim

let reopen sim =
  Sim.set_props sim Linear_routes.list;
  Sim.set_props sim { Linear_routes.list with create = true }

let test_create_closed () =
  let sim = mount_create () in
  let html = Sim.page sim in
  has "trigger" html "New issue";
  has "trigger target" html "href=\"/?new=true\"";
  lacks "no dialog" html "role=\"dialog\"";
  Alcotest.(check int) "no sources" 0 (List.length (Sim.sources sim))

let test_create_open () =
  let sim = open_create () in
  let html = Sim.page sim in
  has "dialog" html "role=\"dialog\"";
  has "title field" html "aria-label=\"Title\"";
  has "status field" html "aria-label=\"Status\"";
  has "priority field" html "aria-label=\"Priority\"";
  Alcotest.(check (list string))
    "the dialog holds the trap, the scroll lock and the dismiss layer"
    [ "dismiss"; "focus_trap"; "scroll_lock" ]
    (List.sort compare (List.map snd (Sim.sources sim)));
  Alcotest.(check (option string))
    "labelled by its heading" (Some "create-issue-title")
    (Sim.attribute sim ~id:"create-issue-popup" "aria-labelledby")

let test_create_validation () =
  let sim = open_create () in
  Sim.submit sim;
  has "message" (Sim.page sim) "Title is required";
  Alcotest.check strings "no call" [] (Sim.call_inputs sim);
  Sim.input sim ~label:"Title" "   ";
  Sim.submit sim;
  has "blank title" (Sim.page sim) "Title is required";
  Sim.input sim ~label:"Title" "Real title";
  lacks "typing clears the message" (Sim.page sim) "Title is required"

let test_create_submit_and_close () =
  let sim = open_create () in
  Sim.input sim ~label:"Title" "  New thing  ";
  Sim.input sim ~label:"Description" "Some text";
  Sim.submit sim;
  Alcotest.(check int) "one call" 1 (List.length (Sim.call_inputs sim));
  has "busy" (Sim.page sim) "Creating...";
  has "fields disabled" (Sim.page sim) "disabled";
  let input = List.hd (Sim.call_inputs sim) in
  has "draft is sent trimmed" input "New thing";
  lacks "not the spaces" input "  New thing";
  has "description is sent" input "Some text";
  Sim.clear_log sim;
  Sim.answer sim create_endpoint (issue 31 ~title:"New thing");
  lacks "dialog closed" (Sim.page sim) "role=\"dialog\"";
  Alcotest.(check bool)
    "url replaced without the flag" true
    (List.mem "replace /" (Sim.log sim));
  Alcotest.(check bool)
    "focus returns to the trigger" true
    (List.mem "focus new-issue" (Sim.log sim));
  Alcotest.(check int) "sources stopped" 0 (List.length (Sim.sources sim))

let test_create_failure_and_retry () =
  let sim = open_create () in
  Sim.input sim ~label:"Title" "Will fail";
  Sim.submit sim;
  Sim.answer_error sim create_endpoint (Remote.Http 500);
  let html = Sim.page sim in
  has "message" html "Could not create the issue. The server answered 500";
  has "draft kept" html "value=\"Will fail\"";
  Sim.submit sim;
  Alcotest.(check int) "retry calls again" 1 (List.length (Sim.call_inputs sim));
  Sim.answer sim create_endpoint (issue 31);
  lacks "closed" (Sim.page sim) "role=\"dialog\"";
  let sim = open_create () in
  Sim.input sim ~label:"Title" "Edit after failure";
  Sim.submit sim;
  Sim.answer_error sim create_endpoint (Remote.Network "down");
  Sim.input sim ~label:"Title" "Edited";
  lacks "editing clears the failure" (Sim.page sim) "Could not create";
  has "edited value" (Sim.page sim) "value=\"Edited\""

let test_create_cancel () =
  let sim = open_create () in
  Sim.input sim ~label:"Title" "Draft";
  Sim.clear_log sim;
  Sim.press sim "Cancel";
  lacks "closed" (Sim.page sim) "role=\"dialog\"";
  Alcotest.(check bool) "replace" true (List.mem "replace /" (Sim.log sim));
  reopen sim;
  has "open again" (Sim.page sim) "role=\"dialog\"";
  lacks "a new dialog has no old draft" (Sim.page sim) "Draft"

let test_create_escape_and_outside () =
  let sim = open_create () in
  Alcotest.(check bool) "escape is bound" true (Sim.escape sim);
  lacks "escape closes" (Sim.page sim) "role=\"dialog\"";
  reopen sim;
  Sim.outside_press sim;
  lacks "outside press closes" (Sim.page sim) "role=\"dialog\""

let test_create_follows_url () =
  let sim = open_create () in
  Sim.clear_log sim;
  Sim.set_props sim Linear_routes.list;
  lacks "closed by the url" (Sim.page sim) "role=\"dialog\"";
  Alcotest.(check bool)
    "no second navigation" false
    (List.mem "replace /" (Sim.log sim));
  Alcotest.(check bool)
    "focus returns" true
    (List.mem "focus new-issue" (Sim.log sim))

let test_create_cannot_be_cancelled_while_sending () =
  let sim = open_create () in
  Sim.input sim ~label:"Title" "Slow";
  Sim.submit sim;
  Sim.clear_log sim;
  ignore (Sim.escape sim);
  Sim.outside_press sim;
  has "Escape and an outside press do not close it" (Sim.page sim) "Creating...";
  Alcotest.(check bool)
    "no navigation" false
    (List.mem "replace /" (Sim.log sim));
  Sim.set_props sim Linear_routes.list;
  has "the url does not cancel a call in flight" (Sim.page sim) "Creating..."

(* The app *)

let test_app_list_route () =
  let sim = sim_app "/" in
  answer_app sim;
  let html = Sim.html sim in
  has "header" html "Mini Linear";
  has "user" html "you are Ada";
  has "list" html "First";
  has "trigger" html "New issue";
  lacks "dialog is closed" (Sim.page sim) "role=\"dialog\""

let test_app_create_flag () =
  let sim = sim_app "/?new=true" in
  answer_app sim;
  has "dialog is open from the url" (Sim.page sim) "role=\"dialog\"";
  has "list is behind it" (Sim.page sim) "First";
  Sim.send sim (Silica.Router.Url_changed (Url.of_string "/"));
  lacks "closed when the flag leaves the url" (Sim.page sim) "role=\"dialog\""

let test_app_detail_route () =
  let sim = sim_app "/issue/2" in
  answer_app sim;
  let html = Sim.html sim in
  has "detail" html "ENG-2";
  has "title" html "Second";
  lacks "no list" html "ENG-1"

let test_app_navigation_keeps_header () =
  let sim = sim_app "/" in
  answer_app sim;
  Sim.send sim (Silica.Router.Url_changed (Url.of_string "/issue/1"));
  Alcotest.(check (list string))
    "only the detail asks, the user is not asked again; the theme keeps its \
     media source"
    [ "media" ]
    (List.map snd (Sim.sources sim));
  Alcotest.(check int)
    "one call or timer" 1
    (List.length (Sim.running sim) - List.length (Sim.sources sim));
  Sim.answer sim issues_endpoint issues;
  has "detail" (Sim.html sim) "ENG-1";
  has "user stays" (Sim.html sim) "you are Ada"

let test_app_not_found () =
  let sim = sim_app "/nowhere" in
  Sim.answer sim me_endpoint User.Ada;
  has "message" (Sim.html sim) "There is nothing at this address.";
  let routes_status =
    Linear_app.status
      (Router.parse Linear_routes.routes (Url.of_string "/nowhere"))
  in
  Alcotest.(check int) "404" 404 routes_status;
  Alcotest.(check int)
    "200" 200
    (Linear_app.status
       (Router.parse Linear_routes.routes (Url.of_string "/issue/1"))
    )

let test_routes_print () =
  let print route = Url.to_string (Router.print Linear_routes.routes route) in
  Alcotest.(check string)
    "list" "/"
    (print (Linear_routes.Issues Linear_routes.list));
  Alcotest.(check string)
    "create flag" "/?new=true"
    (print (Linear_routes.Issues { Linear_routes.list with create = true }));
  Alcotest.(check string) "issue" "/issue/12" (print (Linear_routes.Issue 12));
  Alcotest.(check bool)
    "a bad flag falls back to the default" true
    ( match Router.parse Linear_routes.routes (Url.of_string "/?new=maybe") with
    | Linear_routes.Issues q ->
        q = Linear_routes.list
    | _ ->
        false
    )

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica linear"
    [
      ( "store",
        [
          t "seed" test_store_seed;
          t "create" test_store_create;
          t "rejects bad drafts" test_store_rejects;
          t "users in turn" test_next_user;
          t "dates" test_dates;
          t "codecs" test_model_codecs;
        ]
      );
      ("data", [ t "apply" test_data_apply ]);
      ( "list",
        [
          t "loading then ready" test_list_loading_then_ready;
          t "newest first" test_list_newest_first;
          t "closed issues are struck" test_list_closed_issues_are_struck;
          t "live updates" test_list_live_updates;
          t "reconnect after a failure" test_list_reconnect_after_a_failure;
          t "failure and retry" test_list_failure_and_retry;
          t "unmount stops the subscription" test_list_unmount_stops;
        ]
      );
      ( "detail",
        [
          t "issue" test_detail;
          t "not found" test_detail_not_found;
          t "reconnect after a failure" test_detail_reconnect_after_a_failure;
          t "failure, retry, new number" test_detail_failure_and_new_number;
        ]
      );
      ( "create",
        [
          t "closed" test_create_closed;
          t "open" test_create_open;
          t "validation" test_create_validation;
          t "submit and close" test_create_submit_and_close;
          t "failure and retry" test_create_failure_and_retry;
          t "cancel" test_create_cancel;
          t "escape and outside press" test_create_escape_and_outside;
          t "follows the url" test_create_follows_url;
          t "no cancel while sending"
            test_create_cannot_be_cancelled_while_sending;
        ]
      );
      ( "app",
        [
          t "list route" test_app_list_route;
          t "create flag" test_app_create_flag;
          t "detail route" test_app_detail_route;
          t "navigation keeps the header" test_app_navigation_keeps_header;
          t "not found" test_app_not_found;
          t "routes print and parse" test_routes_print;
        ]
      );
    ]
