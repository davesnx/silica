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

let issue ?(title = "A title") ?(status = Issue_status.Todo)
    ?(priority = Priority.Medium) ?assignee ?(labels = []) ?(created_at = 0)
    ?(updated_at = 0) number : Issue.t =
  {
    number;
    title;
    description = "";
    status;
    priority;
    assignee;
    labels;
    created_at;
    updated_at;
    version = 1;
  }

let ready seq data : Issues.t Topic.update =
  { seq; status = Status.Ready { data; stale = false } }

let mixed =
  [
    issue 1 ~title:"Todo bug" ~labels:[ Label.Bug ] ~assignee:User.Ada;
    issue 2 ~title:"Done docs" ~status:Issue_status.Done ~labels:[ Label.Docs ]
      ~priority:Priority.Urgent;
    issue 3 ~title:"Todo feature" ~labels:[ Label.Feature ]
      ~priority:Priority.No_priority ~assignee:User.Ben;
    issue 4 ~title:"Backlog bug" ~status:Issue_status.Backlog
      ~labels:[ Label.Bug; Label.Chore ] ~priority:Priority.High;
  ]

let parse url =
  match Linear_routes.Router.parse Linear_routes.routes (Url.of_string url) with
  | Linear_routes.Router.At (Linear_routes.State.Issues, q) ->
      (q : Linear_routes.issues)
  | _ ->
      Alcotest.failf "%s is not the list route" url

let print (q : Linear_routes.issues) =
  Url.to_string
    (Linear_routes.Router.print Linear_routes.routes
       (Linear_routes.Router.At (Linear_routes.State.Issues, q))
    )

(* Filters and display options in the URL *)

let test_query_canonical () =
  let q =
    parse "/?label=docs.bug&order=updated&status=done.todo.todo&new=true"
  in
  Alcotest.(check string)
    "values sorted, keys in order, duplicates dropped"
    "/?new=true&status=todo.done&label=bug.docs&order=updated" (print q);
  Alcotest.(check string)
    "the default prints nothing" "/" (print Linear_routes.list);
  let q = parse "/?assignee=none.ada&priority=none.urgent&group=assignee" in
  Alcotest.(check string)
    "assignee and priority"
    "/?assignee=ada.none&priority=urgent.none&group=assignee" (print q)

let test_query_bad_values () =
  let q = parse "/?status=todo.nope&group=size&order=&label=bug" in
  Alcotest.(check string)
    "each bad value falls back to its default, good ones stay" "/?label=bug"
    (print q)

let titles view issues =
  List.concat_map
    (fun (_, members) -> List.map (fun (i : Issue.t) -> i.title) members)
    (Linear_view.groups view issues)

let with_filter filter = { Linear_view.default with filter }

let test_filter_rules () =
  let none = Linear_view.Filter.none in
  Alcotest.(check (list string))
    "is either of"
    [ "Backlog bug"; "Done docs" ]
    (titles
       {
         (with_filter
            { none with statuses = [ Issue_status.Done; Issue_status.Backlog ] }
         )
         with
         grouping = Linear_view.No_grouping;
       }
       mixed
    );
  Alcotest.(check (list string))
    "AND across fields" [ "Todo bug" ]
    (titles
       (with_filter
          { none with statuses = [ Issue_status.Todo ]; labels = [ Label.Bug ] }
       )
       mixed
    );
  Alcotest.(check (list string))
    "unassigned is a value"
    [ "Backlog bug"; "Done docs" ]
    (titles
       {
         (with_filter { none with assignees = [ None ] }) with
         grouping = Linear_view.No_grouping;
       }
       mixed
    );
  Alcotest.(check (list string))
    "a label filter matches any label of the issue" [ "Backlog bug" ]
    (titles (with_filter { none with labels = [ Label.Chore ] }) mixed)

let test_grouping_and_ordering () =
  let groups view =
    List.map
      (fun (g, members) ->
        ( Linear_view.group_label g,
          List.map (fun (i : Issue.t) -> i.number) members
        )
      )
      (Linear_view.groups view mixed)
  in
  Alcotest.(check (list (pair string (list int))))
    "by status, in status order, newest first, empty groups left out"
    [ ("Backlog", [ 4 ]); ("Todo", [ 3; 1 ]); ("Done", [ 2 ]) ]
    (groups Linear_view.default);
  Alcotest.(check (list (pair string (list int))))
    "by assignee, unassigned last"
    [ ("Ada", [ 1 ]); ("Ben", [ 3 ]); ("Unassigned", [ 4; 2 ]) ]
    (groups { Linear_view.default with grouping = Linear_view.By_assignee });
  Alcotest.(check (list (pair string (list int))))
    "no grouping, ordered by priority, no priority last"
    [ ("All issues", [ 2; 4; 1; 3 ]) ]
    (groups
       {
         Linear_view.default with
         grouping = Linear_view.No_grouping;
         ordering = Linear_view.Priority_order;
       }
    );
  let dated =
    [
      issue 1 ~created_at:30 ~updated_at:10;
      issue 2 ~created_at:20 ~updated_at:40;
      issue 3 ~created_at:10 ~updated_at:20;
    ]
  in
  let order ordering =
    List.concat_map
      (fun (_, members) -> List.map (fun (i : Issue.t) -> i.number) members)
      (Linear_view.groups
         {
           Linear_view.default with
           grouping = Linear_view.No_grouping;
           ordering;
         }
         dated
      )
  in
  Alcotest.(check (list int))
    "by created" [ 1; 2; 3 ]
    (order Linear_view.Created);
  Alcotest.(check (list int))
    "by updated" [ 2; 3; 1 ]
    (order Linear_view.Updated)

(* The list component *)

let mount_list view =
  let sim = Sim.mount ~props:(Linear_list.Props view) Linear_list.component in
  Sim.answer sim issues_endpoint mixed;
  sim

let test_list_shows_the_view () =
  let sim =
    mount_list
      (with_filter
         { Linear_view.Filter.none with statuses = [ Issue_status.Todo ] }
      )
  in
  let html = Sim.html sim in
  has "todo shows" html "Todo bug";
  lacks "done is filtered out" html "Done docs";
  has "the group heading" html "Todo";
  has "the filter count" html "Filter (1)"

let test_list_filter_goes_to_the_url () =
  let sim = mount_list Linear_view.default in
  Sim.press_id sim ~id:"issue-filter-trigger";
  Alcotest.(check bool)
    "the filter popup is open" true
    (Sim.exists sim ~id:"issue-filter-popup");
  Sim.clear_log sim;
  Sim.press_id sim ~id:"filter-status-done";
  Alcotest.(check (list string))
    "the URL is replaced with the filter"
    [ "replace /?status=done" ]
    (Sim.log sim);
  has "the list waits for the props" (Sim.html sim) "Todo bug";
  Sim.send sim
    (Linear_list.Props
       (with_filter
          { Linear_view.Filter.none with statuses = [ Issue_status.Done ] }
       )
    );
  lacks "filtered by the new props" (Sim.html sim) "Todo bug";
  has "done stays" (Sim.html sim) "Done docs";
  Alcotest.(check bool)
    "the popup stays open" true
    (Sim.exists sim ~id:"issue-filter-popup")

let test_list_view_survives_pushes () =
  let sim =
    mount_list
      (with_filter { Linear_view.Filter.none with labels = [ Label.Bug ] })
  in
  Sim.press_id sim ~id:"group-status-todo";
  lacks "the todo group is collapsed" (Sim.html sim) "Todo bug";
  Alcotest.(check (option string))
    "collapsed" (Some "false")
    (Sim.attribute sim ~id:"group-status-todo" "aria-expanded");
  Sim.publish sim issues_topic ~key:"all"
    (ready 2
       (mixed
       @ [
           issue 5 ~title:"New bug" ~status:Issue_status.Backlog
             ~labels:[ Label.Bug ];
           issue 6 ~title:"New docs" ~status:Issue_status.Backlog
             ~labels:[ Label.Docs ];
         ]
       )
    );
  let html = Sim.html sim in
  has "a pushed issue that matches shows" html "New bug";
  lacks "a pushed issue that does not match stays hidden" html "New docs";
  lacks "the collapsed group stays collapsed" html "Todo bug";
  has "the filter count stays" html "Filter (1)"

let replaces sim =
  List.filter
    (fun line -> String.starts_with ~prefix:"replace " line)
    (Sim.log sim)

let test_list_display_options () =
  let sim = mount_list Linear_view.default in
  Sim.press_id sim ~id:"issue-grouping-trigger";
  Sim.clear_log sim;
  Sim.press_id sim ~id:(Silica_ui.Select.option_id "issue-grouping" 1);
  Alcotest.(check (list string))
    "grouping goes to the URL"
    [ "replace /?group=assignee" ]
    (replaces sim);
  Sim.send sim
    (Linear_list.Props
       { Linear_view.default with grouping = Linear_view.By_assignee }
    );
  has "grouped by assignee" (Sim.html sim) "Unassigned";
  Sim.press_id sim ~id:"issue-ordering-trigger";
  Sim.clear_log sim;
  Sim.press_id sim ~id:(Silica_ui.Select.option_id "issue-ordering" 0);
  Alcotest.(check (list string))
    "ordering goes to the URL, the grouping stays"
    [ "replace /?group=assignee&order=priority" ]
    (replaces sim)

let test_list_filter_key () =
  let sim = mount_list Linear_view.default in
  Alcotest.(check bool)
    "F opens the filter" true
    (Sim.press_key sim (Key.make "f"));
  Alcotest.(check bool)
    "the filter popup is open" true
    (Sim.exists sim ~id:"issue-filter-popup")

let test_app_filter_keeps_the_list () =
  let sim = Sim.mount (Linear_app.app (Url.of_string "/?status=done")) in
  Sim.answer sim me_endpoint User.Ada;
  Sim.answer sim issues_endpoint mixed;
  lacks "filtered on the first render" (Sim.html sim) "Todo bug";
  has "done shows" (Sim.html sim) "Done docs";
  let before = Sim.running sim in
  Sim.send sim (Silica.Router.Url_changed (Url.of_string "/?status=todo"));
  Alcotest.(check (list string))
    "no new call, same list instance: the list stays" before (Sim.running sim);
  has "todo shows" (Sim.html sim) "Todo bug";
  lacks "done is gone" (Sim.html sim) "Done docs"

(* Inline edit with Optimistic *)

let v ?(version = 1) ?title ?status number =
  { (issue ?title ?status number) with version }

let shown data number =
  List.find (fun (i : Issue.t) -> i.number = number) (Linear_data.issues data)

let test_store_update () =
  let store = Linear_store.create () in
  let request change = { Change.number = 4; base = 1; change } in
  ( match
      Linear_store.update_issue store ~now:5000
        (request (Change.Status Issue_status.Done))
    with
  | Ok (Change.Saved issue) ->
      Alcotest.(check int) "version goes up" 2 issue.version;
      Alcotest.(check int) "updated_at is now" 5000 issue.updated_at;
      Alcotest.(check bool) "status" true (issue.status = Issue_status.Done)
  | _ ->
      Alcotest.fail "expected Saved"
  );
  ( match
      Linear_store.update_issue store ~now:6000
        (request (Change.Title "Late writer"))
    with
  | Ok (Change.Conflict current) ->
      Alcotest.(check int) "the current version" 2 current.version;
      Alcotest.(check bool)
        "nothing written" true
        (current.title <> "Late writer")
  | _ ->
      Alcotest.fail "expected Conflict: base 1 is old"
  );
  Alcotest.(check bool)
    "a blank title is refused" true
    (Linear_store.update_issue store ~now:1
       { Change.number = 4; base = 2; change = Change.Title "  " }
    = Error "Title is required"
    );
  Alcotest.(check bool)
    "an unknown issue is refused" true
    (Result.is_error
       (Linear_store.update_issue store ~now:1
          { Change.number = 99; base = 1; change = Change.Title "x" }
       )
    )

let two = [ v 1 ~title:"One"; v 2 ~title:"Two" ]

let test_data_success () =
  let data =
    Linear_data.start 1 (Change.Status Issue_status.Done)
      (Linear_data.loaded two)
  in
  Alcotest.(check bool)
    "shows at once" true
    ((shown data 1).status = Issue_status.Done);
  Alcotest.(check int) "one pending" 1 (Linear_data.pending data);
  let id, request = List.hd (Linear_data.requests data) in
  Alcotest.(check int) "base is the version seen" 1 request.base;
  let data, outcome =
    Linear_data.finish id
      (Ok (Change.Saved (v 1 ~version:2 ~title:"One" ~status:Issue_status.Done)))
      data
  in
  Alcotest.(check bool)
    "saved" true
    (outcome = Some (request, Linear_data.Saved));
  Alcotest.(check bool)
    "still done" true
    ((shown data 1).status = Issue_status.Done);
  Alcotest.(check int) "confirmed version" 2 (shown data 1).version;
  Alcotest.(check int) "nothing pending" 0 (Linear_data.pending data);
  Alcotest.(check int) "no call" 0 (List.length (Linear_data.requests data))

let test_data_failure_goes_back () =
  let data =
    Linear_data.start 1 (Change.Status Issue_status.Done)
      (Linear_data.loaded two)
  in
  let id, request = List.hd (Linear_data.requests data) in
  let data, outcome =
    Linear_data.finish id (Error (Remote.Network "down")) data
  in
  Alcotest.(check bool)
    "failed" true
    (outcome = Some (request, Linear_data.Failed (Remote.Network "down")));
  Alcotest.(check bool)
    "back to todo" true
    ((shown data 1).status = Issue_status.Todo)

let test_data_topic_first () =
  let data =
    Linear_data.start 1 (Change.Status Issue_status.Done)
      (Linear_data.loaded two)
  in
  let id, _ = List.hd (Linear_data.requests data) in
  let other =
    Linear_data.apply
      (ready 2 [ v 1 ~title:"One"; v 2 ~version:2 ~title:"Two!" ])
      data
  in
  Alcotest.(check int)
    "a push without this issue's change keeps it pending" 1
    (Linear_data.pending other);
  Alcotest.(check bool)
    "and shown" true
    ((shown other 1).status = Issue_status.Done);
  let data =
    Linear_data.apply
      (ready 3
         [
           v 1 ~version:2 ~title:"One" ~status:Issue_status.Done;
           v 2 ~title:"Two";
         ]
      )
      other
  in
  Alcotest.(check int) "the newer state settles it" 0 (Linear_data.pending data);
  Alcotest.(check int)
    "the call still runs" 1
    (List.length (Linear_data.requests data));
  let data, _ =
    Linear_data.finish id
      (Ok (Change.Saved (v 1 ~version:2 ~title:"One" ~status:Issue_status.Done)))
      data
  in
  Alcotest.(check bool)
    "the late answer changes nothing" true
    ((shown data 1).status = Issue_status.Done && (shown data 1).version = 2);
  Alcotest.(check int) "no call" 0 (List.length (Linear_data.requests data))

let test_data_conflict () =
  let data =
    Linear_data.start 1 (Change.Title "Mine") (Linear_data.loaded two)
  in
  let id, _ = List.hd (Linear_data.requests data) in
  let theirs = v 1 ~version:2 ~title:"Theirs" in
  let data, outcome =
    Linear_data.finish id (Ok (Change.Conflict theirs)) data
  in
  Alcotest.(check bool)
    "conflict" true
    ( match outcome with
    | Some (_, Linear_data.Conflict i) ->
        i = theirs
    | _ ->
        false
    );
  Alcotest.(check string) "the winner shows" "Theirs" (shown data 1).title

let mount_detail () =
  let sim = Sim.mount ~props:(Linear_detail.Props 1) Linear_detail.component in
  Sim.answer sim issues_endpoint two;
  Sim.answer sim comments_endpoint { Thread.viewer = User.Ada; comments = [] };
  sim

let pick_status sim position =
  Sim.press_id sim ~id:"status-picker-trigger";
  Sim.press_id sim ~id:(Silica_ui.Menu.item_id "status-picker" position)

let pending sim = Sim.attribute sim ~id:"issue-detail" "data-pending"

let test_detail_pick_success () =
  let sim = mount_detail () in
  pick_status sim 3;
  has "done at once" (Sim.html sim) "Status: Done";
  Alcotest.(check (option string)) "pending" (Some "1") (pending sim);
  has "the request" (String.concat "" (Sim.call_inputs sim)) "Done";
  Sim.answer sim update_endpoint
    (Change.Saved (v 1 ~version:2 ~title:"One" ~status:Issue_status.Done));
  has "still done" (Sim.html sim) "Status: Done";
  Alcotest.(check (option string)) "settled" (Some "0") (pending sim)

let test_detail_pick_failure () =
  let sim = mount_detail () in
  pick_status sim 3;
  Sim.answer_error sim update_endpoint (Remote.Network "down");
  has "back to todo" (Sim.html sim) "Status: Todo";
  has "a toast says so" (Sim.page sim) "Could not save the status"

let test_detail_pick_topic_first () =
  let sim = mount_detail () in
  pick_status sim 3;
  Sim.publish sim issues_topic ~key:"all"
    (ready 2
       [
         v 1 ~version:2 ~title:"One" ~status:Issue_status.Done; v 2 ~title:"Two";
       ]
    );
  Alcotest.(check (option string))
    "settled by the topic" (Some "0") (pending sim);
  Alcotest.(check int)
    "the call still runs" 1
    (List.length (Sim.call_inputs sim));
  Sim.answer sim update_endpoint
    (Change.Saved (v 1 ~version:2 ~title:"One" ~status:Issue_status.Done));
  has "done" (Sim.html sim) "Status: Done";
  Alcotest.(check int) "no call" 0 (List.length (Sim.call_inputs sim))

let test_detail_title_conflict () =
  let sim = mount_detail () in
  Sim.press_id sim ~id:"edit-title";
  Sim.input sim ~label:"Title" "Mine";
  Alcotest.(check bool)
    "Enter saves" true
    (Sim.key_down sim ~id:"title-input" Key.enter);
  has "shows at once" (Sim.html sim) ">Mine</h1>";
  has "base version" (String.concat "" (Sim.call_inputs sim)) "\"base\":1";
  Sim.answer sim update_endpoint
    (Change.Conflict (v 1 ~version:2 ~title:"Theirs"));
  Alcotest.(check (option string))
    "the draft is kept" (Some "Mine")
    (Sim.attribute sim ~id:"title-input" "value");
  has "the note names the new value" (Sim.html sim) "Theirs";
  Alcotest.(check bool)
    "save again" true
    (Sim.key_down sim ~id:"title-input" Key.enter);
  has "now on the new version"
    (String.concat "" (Sim.call_inputs sim))
    "\"base\":2"

let test_detail_escape_cancels () =
  let sim = mount_detail () in
  Sim.press_id sim ~id:"edit-description";
  Sim.input sim ~label:"Description" "Draft";
  Alcotest.(check bool)
    "Escape" true
    (Sim.key_down sim ~id:"description-input" Key.escape);
  Alcotest.(check bool)
    "editor closed" false
    (Sim.exists sim ~id:"description-input");
  Alcotest.(check (list string)) "no call" [] (Sim.call_inputs sim);
  Sim.press_id sim ~id:"edit-description";
  Sim.input sim ~label:"Description" "Saved text";
  Alcotest.(check bool)
    "Ctrl+Enter saves" true
    (Sim.key_down sim ~id:"description-input" (Key.make ~ctrl:true "Enter"));
  has "saved text shows" (Sim.html sim) "Saved text"

let test_detail_picker_keys () =
  let sim = mount_detail () in
  Alcotest.(check bool) "S" true (Sim.press_key sim (Key.make "s"));
  Alcotest.(check bool)
    "status menu open" true
    (Sim.exists sim ~id:"status-picker-popup");
  Alcotest.(check bool) "L" true (Sim.press_key sim (Key.make "l"));
  Alcotest.(check bool)
    "labels open" true
    (Sim.exists sim ~id:"labels-picker-popup");
  Sim.press_id sim ~id:"label-bug";
  has "labels change at once" (Sim.html sim) "bug";
  has "the request" (String.concat "" (Sim.call_inputs sim)) "Bug"

(* Comments *)

let comment ?(author = User.Ada) ?(edited = false) id body : Comment.t =
  { id; number = 1; author; body; created_at = 0; edited }

let test_store_comments () =
  let store = Linear_store.create () in
  let added =
    Linear_store.add_comment store ~now:10 ~author:User.Ada
      { New_comment.number = 1; body = "  First!  " }
  in
  ( match added with
  | Ok c ->
      Alcotest.(check string) "trimmed" "First!" c.body;
      Alcotest.(check bool) "author" true (c.author = User.Ada);
      Alcotest.(check bool) "not edited" false c.edited
  | Error m ->
      Alcotest.fail m
  );
  let id = match added with Ok c -> c.id | Error _ -> 0 in
  Alcotest.(check bool)
    "an empty comment is refused" true
    (Result.is_error
       (Linear_store.add_comment store ~now:10 ~author:User.Ada
          { New_comment.number = 1; body = " " }
       )
    );
  Alcotest.(check bool)
    "a comment on an unknown issue is refused" true
    (Result.is_error
       (Linear_store.add_comment store ~now:10 ~author:User.Ada
          { New_comment.number = 99; body = "x" }
       )
    );
  Alcotest.(check bool)
    "Ben cannot edit Ada's comment" true
    (Result.is_error
       (Linear_store.edit_comment store ~author:User.Ben
          { Comment_edit.id; body = "Mine now" }
       )
    );
  ( match
      Linear_store.edit_comment store ~author:User.Ada
        { Comment_edit.id; body = "First, edited" }
    with
  | Ok c ->
      Alcotest.(check string) "edited body" "First, edited" c.body;
      Alcotest.(check bool) "marked edited" true c.edited
  | Error m ->
      Alcotest.fail m
  );
  Alcotest.(check (list string))
    "the thread of the issue" [ "First, edited" ]
    (List.map (fun (c : Comment.t) -> c.body) (Linear_store.comments store 1))

let thread =
  {
    Thread.viewer = User.Ada;
    comments = [ comment 1 "Mine"; comment 2 ~author:User.Ben "From Ben" ];
  }

let mount_comments () =
  let sim =
    Sim.mount ~props:(Linear_comments.Props 1) Linear_comments.component
  in
  Sim.answer sim comments_endpoint thread;
  sim

let test_comments_show () =
  let sim = mount_comments () in
  has "mine" (Sim.html sim) "Mine";
  has "from Ben" (Sim.html sim) "From Ben";
  Alcotest.(check bool) "edit own" true (Sim.exists sim ~id:"comment-1-edit");
  Alcotest.(check bool) "not others" false (Sim.exists sim ~id:"comment-2-edit");
  Alcotest.(check (list (pair string string)))
    "live per issue"
    [ ("comments", "1") ]
    (Sim.subscriptions sim);
  Sim.publish sim comments_topic ~key:"1"
    {
      seq = 2;
      status =
        Status.Ready
          { data = thread.comments @ [ comment 3 "Third" ]; stale = false };
    };
  has "a pushed comment shows" (Sim.html sim) "Third"

let test_composer () =
  let sim = mount_comments () in
  Alcotest.(check (option string))
    "send is disabled while empty" (Some "")
    (Sim.attribute sim ~id:"comment-send" "disabled");
  Sim.input sim ~label:"Comment" "Hello";
  Alcotest.(check bool)
    "Ctrl+Enter sends" true
    (Sim.key_down sim ~id:"comment-input" (Key.make ~ctrl:true "Enter"));
  has "sending" (Sim.html sim) "Sending...";
  has "the body" (String.concat "" (Sim.call_inputs sim)) "Hello";
  Sim.answer_error sim add_comment_endpoint (Remote.Network "down");
  has "failed note" (Sim.html sim) "Could not send the comment";
  Alcotest.(check (option string))
    "the draft is kept" (Some "Hello")
    (Sim.attribute sim ~id:"comment-input" "value");
  Sim.press_id sim ~id:"comment-send";
  Sim.answer sim add_comment_endpoint (comment 3 "Hello");
  Alcotest.(check (option string))
    "empty again" (Some "")
    (Sim.attribute sim ~id:"comment-input" "value");
  Sim.input sim ~label:"Comment" "Draft";
  Alcotest.(check bool)
    "Escape clears" true
    (Sim.key_down sim ~id:"comment-input" Key.escape);
  Alcotest.(check (option string))
    "cleared" (Some "")
    (Sim.attribute sim ~id:"comment-input" "value")

let test_edit_own_comment () =
  let sim = mount_comments () in
  Sim.press_id sim ~id:"comment-1-edit";
  Sim.input sim ~label:"Edit comment" "Changed";
  Sim.press_id sim ~id:"comment-1-save";
  has "saving" (Sim.html sim) "Saving...";
  has "the edit" (String.concat "" (Sim.call_inputs sim)) "Changed";
  Sim.answer sim edit_comment_endpoint (comment 1 ~edited:true "Changed");
  has "the new body" (Sim.html sim) "Changed";
  has "edited mark" (Sim.html sim) "(edited)";
  Sim.press_id sim ~id:"comment-1-edit";
  Sim.input sim ~label:"Edit comment" "Again";
  Alcotest.(check bool)
    "Escape cancels" true
    (Sim.key_down sim ~id:"comment-1-input" Key.escape);
  lacks "not saved" (Sim.html sim) "Again";
  Sim.press_id sim ~id:"comment-1-edit";
  Sim.input sim ~label:"Edit comment" "Fails";
  Sim.press_id sim ~id:"comment-1-save";
  Sim.answer_error sim edit_comment_endpoint (Remote.Http 403);
  has "an error while editing" (Sim.html sim) "Could not save the comment";
  Alcotest.(check (option string))
    "the draft stays" (Some "Fails")
    (Sim.attribute sim ~id:"comment-1-input" "value")

(* Keyboard: row cursor, command menu, C *)

let cursor sim =
  List.filter_map
    (fun n ->
      match
        Sim.attribute sim ~id:("issue-row-" ^ string_of_int n) "data-cursor"
      with
      | Some _ ->
          Some n
      | None ->
          None
    )
    [ 1; 2; 3; 4 ]

let focuses sim =
  List.filter
    (fun line -> String.starts_with ~prefix:"focus " line)
    (Sim.log sim)

let test_list_cursor () =
  let sim = mount_list Linear_view.default in
  Alcotest.(check (list int)) "no cursor" [] (cursor sim);
  ignore (Sim.press_key sim (Key.make "j"));
  Alcotest.(check (list int)) "J: the first row (Backlog)" [ 4 ] (cursor sim);
  Alcotest.(check (list string))
    "and it has focus" [ "focus issue-row-4" ] (focuses sim);
  ignore (Sim.press_key sim (Key.make "j"));
  ignore (Sim.press_key sim Key.arrow_down);
  Alcotest.(check (list int))
    "J and ArrowDown: newest first in Todo" [ 1 ] (cursor sim);
  ignore (Sim.press_key sim (Key.make "k"));
  Alcotest.(check (list int)) "K goes up" [ 3 ] (cursor sim);
  Sim.press_id sim ~id:"group-status-todo";
  ignore (Sim.press_key sim Key.arrow_up);
  ignore (Sim.press_key sim (Key.make "j"));
  Alcotest.(check (list int)) "a collapsed group is skipped" [ 2 ] (cursor sim);
  ignore (Sim.press_key sim (Key.make "j"));
  Alcotest.(check (list int)) "the last row stays" [ 2 ] (cursor sim);
  Sim.focus sim ~id:"issue-row-4";
  Alcotest.(check (list int))
    "a row that gets focus is the cursor" [ 4 ] (cursor sim)

let run_command sim text =
  Sim.input sim ~label:"Command menu" text;
  ignore (Sim.key_down sim ~id:"command-menu-search-input" Key.enter)

let test_list_command_menu () =
  let sim = mount_list Linear_view.default in
  Alcotest.(check bool)
    "Ctrl+K" true
    (Sim.press_key sim (Key.make ~ctrl:true "k"));
  Alcotest.(check bool) "open" true (Sim.exists sim ~id:"command-menu-popup");
  lacks "no issue is focused: no set commands" (Sim.page sim) "Set status";
  run_command sim "ENG-3";
  Alcotest.(check bool)
    "ENG-3 opens the issue" true
    (List.mem "navigate /issue/3" (Sim.log sim));
  Alcotest.(check bool) "closed" false (Sim.exists sim ~id:"command-menu-popup");
  ignore (Sim.press_key sim (Key.make "j"));
  ignore (Sim.press_key sim (Key.make ~meta:true "k"));
  has "set commands for the focused issue" (Sim.page sim)
    "Set status: Done of ENG-4";
  run_command sim "status: done";
  has "the change shows at once" (Sim.html sim) "Done";
  has "the request" (String.concat "" (Sim.call_inputs sim)) "Done";
  Sim.answer_error sim update_endpoint (Remote.Network "down");
  has "a toast" (Sim.page sim) "Could not save the status";
  ignore (Sim.press_key sim (Key.make ~ctrl:true "k"));
  run_command sim "Create issue";
  Alcotest.(check bool)
    "create opens the modal" true
    (List.mem "navigate /?new=true" (Sim.log sim))

let test_list_c_key () =
  let sim =
    mount_list { Linear_view.default with grouping = Linear_view.No_grouping }
  in
  Sim.clear_log sim;
  ignore (Sim.press_key sim (Key.make "c"));
  Alcotest.(check (list string))
    "C opens the create modal and keeps the view"
    [ "navigate /?new=true&group=none" ]
    (List.filter
       (fun l -> String.starts_with ~prefix:"navigate" l)
       (Sim.log sim)
    )

let test_detail_command_menu () =
  let sim = mount_detail () in
  ignore (Sim.press_key sim (Key.make ~ctrl:true "k"));
  has "the shown issue is the focused one" (Sim.page sim)
    "Set priority: Urgent of ENG-1";
  run_command sim "priority: urgent";
  has "shows at once" (Sim.html sim) "Priority: Urgent";
  ignore (Sim.press_key sim (Key.make ~ctrl:true "k"));
  run_command sim "Go to list";
  Alcotest.(check bool)
    "go to the list" true
    (List.mem "navigate /" (Sim.log sim));
  Sim.clear_log sim;
  ignore (Sim.press_key sim (Key.make "c"));
  Alcotest.(check bool)
    "C opens the create modal" true
    (List.mem "navigate /?new=true" (Sim.log sim))

let test_create_ctrl_enter () =
  let sim =
    Sim.mount
      ~props:(Linear_create.Props { Linear_routes.list with create = true })
      Linear_create.component
  in
  Sim.input sim ~label:"Title" "From the keyboard";
  Alcotest.(check bool)
    "Ctrl+Enter in the description submits" true
    (Sim.key_down sim ~id:"new-issue-description" (Key.make ~ctrl:true "Enter"));
  has "sending" (Sim.page sim) "Creating..."

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica linear features"
    [
      ( "view",
        [
          t "canonical query" test_query_canonical;
          t "bad query values" test_query_bad_values;
          t "filter rules" test_filter_rules;
          t "grouping and ordering" test_grouping_and_ordering;
        ]
      );
      ( "list",
        [
          t "shows the view" test_list_shows_the_view;
          t "filter goes to the url" test_list_filter_goes_to_the_url;
          t "view survives pushes" test_list_view_survives_pushes;
          t "display options" test_list_display_options;
          t "F opens the filter" test_list_filter_key;
          t "app filter keeps the list" test_app_filter_keeps_the_list;
        ]
      );
      ( "edit",
        [
          t "store update and conflict" test_store_update;
          t "data: success" test_data_success;
          t "data: failure goes back" test_data_failure_goes_back;
          t "data: topic state first" test_data_topic_first;
          t "data: conflict" test_data_conflict;
          t "detail: pick, success" test_detail_pick_success;
          t "detail: pick, failure" test_detail_pick_failure;
          t "detail: pick, topic first" test_detail_pick_topic_first;
          t "detail: title conflict keeps the draft" test_detail_title_conflict;
          t "detail: Escape cancels, Ctrl+Enter saves"
            test_detail_escape_cancels;
          t "detail: S and L open pickers" test_detail_picker_keys;
        ]
      );
      ( "comments",
        [
          t "store" test_store_comments;
          t "list, own, live" test_comments_show;
          t "composer" test_composer;
          t "edit own comment" test_edit_own_comment;
        ]
      );
      ( "keyboard",
        [
          t "list: J, K, arrows move the row cursor" test_list_cursor;
          t "list: command menu" test_list_command_menu;
          t "list: C" test_list_c_key;
          t "detail: command menu and C" test_detail_command_menu;
          t "create: Ctrl+Enter submits" test_create_ctrl_enter;
        ]
      );
    ]
