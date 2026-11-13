open Silica
open Linear_model
module Memory = Silica_memory

let issue number title : Issue.t =
  {
    number;
    title;
    description = "";
    status = Issue_status.Todo;
    priority = Priority.Medium;
    assignee = None;
    labels = [];
    created_at = 0;
    updated_at = 0;
    version = 1;
  }

let issues = [ issue 1 "First"; issue 2 "Second" ]

let answer_requests requests =
  List.iter
    (fun request ->
      let reply endpoint value =
        Memory.reply request (Codec.to_string (Remote.output endpoint) value)
      in
      match Memory.path request with
      | "/api/me" ->
          reply me_endpoint User.Ada
      | "/api/issues" ->
          reply issues_endpoint issues
      | "/api/comments" ->
          reply comments_endpoint { Thread.viewer = User.Ada; comments = [] }
      | "/api/issues/create" ->
          ()
      | path ->
          Alcotest.failf "unexpected call %s" path
    )
    requests

let answer t = answer_requests (Memory.pending t)

let calls t path =
  List.length (List.filter (fun r -> Memory.path r = path) (Memory.requests t))

let has msg t needle =
  let html = Memory.html t in
  let n = String.length needle and h = String.length html in
  let rec go i = i + n <= h && (String.sub html i n = needle || go (i + 1)) in
  if not (go 0) then Alcotest.failf "%s: %S not found in %s" msg needle html

let to_detail_and_back t =
  Memory.press (Memory.by_id t "issue-row-2");
  answer t;
  Alcotest.(check string) "on the detail" "/issue/2" (Memory.url t);
  has "the detail shows" t "Second</h1>";
  Memory.back t;
  answer t;
  Alcotest.(check string) "back on the list" "/" (Memory.url t)

let test_user_call_runs_once () =
  let t = Memory.mount_app ~url:"/" Linear_app.app in
  answer t;
  has "the user shows" t "you are Ada";
  to_detail_and_back t;
  has "the user still shows" t "you are Ada";
  Alcotest.(check int) "one user call for the whole visit" 1 (calls t "/api/me");
  Alcotest.(check int)
    "the pages load again: list, detail, list" 3 (calls t "/api/issues")

let test_create_in_flight_survives () =
  let t = Memory.mount_app ~url:"/" Linear_app.app in
  answer t;
  Memory.press (Memory.by_id t "issue-row-2");
  answer t;
  Memory.press (Memory.by_role ~name:"Mini Linear" t Accessibility.Link);
  answer t;
  Memory.press (Memory.by_role ~name:"New issue" t Accessibility.Link);
  Alcotest.(check string) "the modal is open" "/?new=true" (Memory.url t);
  let dialog = Memory.by_role ~name:"New issue" t Accessibility.Dialog in
  let title =
    Memory.by_role ~within:dialog ~name:"Title" t Accessibility.Textbox
  in
  Memory.input title "Made during a visit";
  Memory.submit title;
  let create =
    match
      List.filter
        (fun r -> Memory.path r = "/api/issues/create")
        (Memory.pending t)
    with
    | [ request ] ->
        request
    | requests ->
        Alcotest.failf "%d create calls" (List.length requests)
  in
  Memory.back t;
  answer t;
  Memory.back t;
  answer t;
  Alcotest.(check string)
    "the back button reaches the detail behind the open dialog" "/issue/2"
    (Memory.url t);
  has "the detail shows" t "Second</h1>";
  Memory.forward t;
  answer t;
  Alcotest.(check string) "forward on the list" "/" (Memory.url t);
  let focused () =
    List.mem (Action.Focus Linear_refs.new_issue) (Memory.actions t)
  in
  Alcotest.(check bool) "no answer yet" false (focused ());
  Memory.reply create
    (Codec.to_string
       (Remote.output create_endpoint)
       (issue 3 "Made during a visit")
    );
  Alcotest.(check bool)
    "the create button that sent the call gets its answer and gives focus back"
    true (focused ())

let contains text needle =
  let n = String.length needle and h = String.length text in
  let rec go i = i + n <= h && (String.sub text i n = needle || go (i + 1)) in
  go 0

let test_trace_of_a_visit () =
  Linear_codecs.register ();
  let t = Memory.mount_app ~url:"/" Linear_app.app in
  answer t;
  Memory.press (Memory.by_role ~name:"New issue" t Accessibility.Link);
  let dialog = Memory.by_role ~name:"New issue" t Accessibility.Dialog in
  let title =
    Memory.by_role ~within:dialog ~name:"Title" t Accessibility.Textbox
  in
  Memory.input title "Traced";
  Memory.submit title;
  let trace = Memory.trace t in
  let text = Trace.to_text trace in
  if Sys.getenv_opt "SILICA_PRINT_TRACE" = Some "1" then print_string text;
  let transitions = trace.transitions in
  let first = List.hd transitions in
  Alcotest.(check string)
    "the root starts first" "started"
    ( match first.change with
    | Transition.Started ->
        "started"
    | Transition.Resumed | Unhandled | Kept | Entered | Ended ->
        "other"
    );
  Alcotest.(check string)
    "the root is the router" trace.header.root first.machine;
  Alcotest.(check (list int))
    "numbered from 1 with no gap"
    (List.init (List.length transitions) (fun index -> index + 1))
    (List.map (fun (record : Transition.t) -> record.seq) transitions);
  let has_cause name found =
    if not (List.exists found transitions) then
      Alcotest.failf "no %s in the trace:\n%s" name text
  in
  has_cause "answer of /api/issues with the issues" (fun record ->
      match[@warning "-4"] record.cause with
      | Transition.Answer { path = "/api/issues"; answer = Some answer; _ } ->
          contains (Silica_json.Json.to_string answer) "\"Second\""
      | _ ->
          false
  );
  has_cause "submit of the form" (fun record ->
      match[@warning "-4"] record.cause with
      | Transition.Element { handler = "submit"; element; _ } ->
          contains element "form"
      | _ ->
          false
  );
  has_cause "the create call, started by the parent of the dialog"
    (fun record ->
      match[@warning "-4"] record.cause with
      | Transition.Child { instance } ->
          contains instance ":create-issue-dialog"
          && List.exists
               (fun action -> contains action "start call")
               record.actions
      | _ ->
          false
  );
  has_cause "input of Traced in the Title field" (fun record ->
      match[@warning "-4"] record.cause with
      | Transition.Element
          { handler = "input"; role = Some "textbox"; value = Some "Traced"; _ }
        ->
          true
      | _ ->
          false
  );
  has_cause "the event and snapshot data of a machine with codecs"
    (fun record ->
      record.machine = "create-issue"
      && record.event = "title"
      && Option.map Silica_json.Json.to_string record.data
         = Some "[\"Title\",\"Traced\"]"
      && record.snapshot <> None
  );
  has_cause "the URL change to the dialog" (fun record ->
      record.cause = Transition.Url "/?new=true"
  );
  has_cause "a child instance with a path under the root" (fun record ->
      contains record.instance (first.instance ^ "/")
  );
  match Trace.of_jsonl (Trace.to_jsonl trace) with
  | Ok again ->
      List.iter2
        (fun (read : Transition.t) (written : Transition.t) ->
          if read <> written then
            Alcotest.failf "record %d reads back as %s" written.seq
              (Codec.to_string Transition.codec read)
        )
        again.transitions trace.transitions;
      Alcotest.(check bool)
        "the file reads back to the same trace" true (again = trace)
  | Error message ->
      Alcotest.fail message

let test_row_names_have_spaces () =
  let t = Memory.mount_app ~url:"/" Linear_app.app in
  answer t;
  let row = Memory.by_role ~name:"ENG-2 Second Todo" t Accessibility.Link in
  Alcotest.(check (option string))
    "the row link" (Some "issue-row-2")
    (Memory.attribute row "id")

let updates t =
  List.filter (fun r -> Memory.path r = "/api/issues/update") (Memory.pending t)

let answer_pages t =
  List.iter
    (fun request ->
      if Memory.path request <> "/api/issues/update" then
        answer_requests [ request ]
    )
    (Memory.pending t)

let run_command t text =
  Memory.key t (Key.make ~ctrl:true "k");
  let search = Memory.by_role ~name:"Command menu" t Accessibility.Combobox in
  Memory.focus search;
  Memory.input search text;
  Memory.key t Key.enter

let toasts t = Memory.all_by_role t Accessibility.Alertdialog

let toast_names t = List.map Memory.accessible_name (toasts t)

let test_save_fails_after_moving_on () =
  let t = Memory.mount_app ~url:"/issue/1" Linear_app.app in
  answer_pages t;
  Memory.press (Memory.by_role ~name:"Edit title" t Accessibility.Button);
  Memory.input (Memory.by_role ~name:"Title" t Accessibility.Textbox) "Renamed";
  Memory.press (Memory.by_role ~name:"Save" t Accessibility.Button);
  let save =
    match updates t with
    | [ request ] ->
        request
    | requests ->
        Alcotest.failf "%d save calls" (List.length requests)
  in
  run_command t "ENG-2";
  answer_pages t;
  Alcotest.(check string) "on the other issue" "/issue/2" (Memory.url t);
  has "the other issue shows" t "Second</h1>";
  Alcotest.(check int)
    "the save is still waiting for its answer" 1
    (List.length (updates t));
  Memory.network_error save "down";
  Alcotest.(check (list string))
    "a toast tells which save failed"
    [ "Could not save the title of ENG-1" ]
    (toast_names t)

let test_toast_keys_are_not_reused () =
  let t = Memory.mount_app ~url:"/issue/1" Linear_app.app in
  answer_pages t;
  let fail_next_save () =
    run_command t "priority: urgent";
    match updates t with
    | [ request ] ->
        Memory.network_error request "down"
    | requests ->
        Alcotest.failf "%d save calls" (List.length requests)
  in
  let close toast =
    Memory.press
      (Memory.by_role ~within:toast ~name:"Close" t Accessibility.Button)
  in
  fail_next_save ();
  fail_next_save ();
  Alcotest.(check int) "two toasts" 2 (List.length (toasts t));
  close (List.hd (toasts t));
  fail_next_save ();
  let ids =
    List.filter_map (fun toast -> Memory.attribute toast "id") (toasts t)
  in
  Alcotest.(check int)
    "the two shown toasts have two ids" 2
    (List.length (List.sort_uniq compare ids));
  close (List.nth (toasts t) 1);
  Alcotest.(check int)
    "closing the newest toast keeps the other one" 1
    (List.length (toasts t))

let () =
  Alcotest.run "linear_memory"
    [
      ( "layout",
        [
          Alcotest.test_case "the user call runs once across pages" `Quick
            test_user_call_runs_once;
          Alcotest.test_case "a create in flight survives a move between pages"
            `Quick test_create_in_flight_survives;
          Alcotest.test_case "the trace of a visit, from memory" `Quick
            test_trace_of_a_visit;
          Alcotest.test_case "a row link reads its parts apart" `Quick
            test_row_names_have_spaces;
        ]
      );
      ( "saves",
        [
          Alcotest.test_case
            "a save that fails after the user opened another issue is reported"
            `Quick test_save_fails_after_moving_on;
          Alcotest.test_case "a toast key is never used again" `Quick
            test_toast_keys_are_not_reused;
        ]
      );
    ]
