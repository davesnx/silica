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

let answer t =
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
    (Memory.pending t)

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
  Memory.press (Memory.by_id t "new-issue");
  Alcotest.(check string) "the modal is open" "/?new=true" (Memory.url t);
  Memory.input (Memory.by_id t "new-issue-title") "Made during a visit";
  Memory.submit (Memory.by_id t "new-issue-title");
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
  to_detail_and_back t;
  let focused () =
    List.mem (Cmd.Focus Linear_refs.new_issue) (Memory.commands t)
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

let () =
  Alcotest.run "linear_memory"
    [
      ( "layout",
        [
          Alcotest.test_case "the user call runs once across pages" `Quick
            test_user_call_runs_once;
          Alcotest.test_case "a create in flight survives a move between pages"
            `Quick test_create_in_flight_survives;
        ]
      );
    ]
