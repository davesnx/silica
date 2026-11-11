open Linear_model

type t = {
  lock : Mutex.t;
  mutable issues : Issue.t list;
  mutable comments : Comment.t list;
}

let base_time = 1793836800

let seed =
  let open Issue_status in
  let open Priority in
  [
    ("Set up the project", Done, High, Some User.Ada, [ Label.Chore ]);
    ("Write the build script", Done, Medium, Some User.Ben, [ Label.Chore ]);
    ( "Login page flashes on load",
      In_progress,
      Urgent,
      Some User.Ada,
      [ Label.Bug ]
    );
    ("Add an export to CSV", Todo, Medium, None, [ Label.Feature ]);
    ("Document the API", Backlog, Low, Some User.Cleo, [ Label.Docs ]);
    ("Search is slow on large lists", Todo, High, Some User.Ben, [ Label.Bug ]);
    ("Dark mode for the settings page", Backlog, Low, None, [ Label.Feature ]);
    ( "Fix the typo on the pricing page",
      Done,
      No_priority,
      Some User.Cleo,
      [ Label.Docs ]
    );
    ( "Upgrade the database driver",
      In_progress,
      Medium,
      Some User.Ben,
      [ Label.Chore ]
    );
    ("Users cannot reset a password", Todo, Urgent, Some User.Ada, [ Label.Bug ]);
    ("Add keyboard shortcuts", Backlog, Medium, None, [ Label.Feature ]);
    ("Remove the old billing code", Canceled, Low, None, [ Label.Chore ]);
    ( "Notifications arrive twice",
      In_progress,
      High,
      Some User.Cleo,
      [ Label.Bug ]
    );
    ("Write the onboarding guide", Todo, Medium, Some User.Cleo, [ Label.Docs ]);
    ( "Show the avatar in the header",
      Done,
      Low,
      Some User.Ben,
      [ Label.Feature ]
    );
    ( "Rate limit the public API",
      Backlog,
      High,
      None,
      [ Label.Feature; Label.Chore ]
    );
    ("Crash when the title is empty", Done, Urgent, Some User.Ada, [ Label.Bug ]);
    ("Clean up unused CSS", Backlog, No_priority, None, [ Label.Chore ]);
    ( "Add a changelog page",
      Todo,
      Low,
      Some User.Ben,
      [ Label.Docs; Label.Feature ]
    );
    ("Improve the empty state", Backlog, Medium, None, [ Label.Feature ]);
    ("Wrong date in the activity feed", Todo, High, Some User.Ada, [ Label.Bug ]);
    ("Try a new font", Canceled, No_priority, None, []);
    ("Cache the issue list", In_progress, Medium, Some User.Ben, [ Label.Chore ]);
    ("Describe the label colours", Backlog, Low, Some User.Cleo, [ Label.Docs ]);
    ("Drag to reorder is jumpy", Todo, Medium, None, [ Label.Bug ]);
    ("Support more than three users", Backlog, High, None, [ Label.Feature ]);
    ("Archive finished work", Todo, Low, Some User.Cleo, [ Label.Feature ]);
    ("Rename the Canceled status", Canceled, No_priority, None, []);
    ("Mobile layout overflows", Todo, High, Some User.Ada, [ Label.Bug ]);
    ( "Plan the first release",
      In_progress,
      Urgent,
      Some User.Cleo,
      [ Label.Chore; Label.Docs ]
    );
  ]

let seed_issues count =
  let seeds = Array.of_list seed in
  List.init count (fun index ->
      let title, status, priority, assignee, labels =
        seeds.(index mod Array.length seeds)
      in
      let title =
        if index < Array.length seeds then
          title
        else
          title ^ " " ^ string_of_int (index + 1)
      in
      let created_at = base_time + (index * 7200) in
      {
        Issue.number = index + 1;
        title;
        description =
          "Seed issue " ^ string_of_int (index + 1) ^ ".\n" ^ title ^ ".";
        status;
        priority;
        assignee;
        labels;
        created_at;
        updated_at = created_at + 3600;
        version = 1;
      }
  )

let create ?(count = List.length seed) () =
  { lock = Mutex.create (); issues = seed_issues count; comments = [] }

let locked t f =
  Mutex.lock t.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.lock) f

let issues t = locked t (fun () -> t.issues)

let create_issue t ~now draft =
  match Draft.check draft with
  | Error message ->
      Error message
  | Ok (draft : Draft.t) ->
      locked t (fun () ->
          let number =
            List.fold_left (fun m (i : Issue.t) -> max m i.number) 0 t.issues
            + 1
          in
          let issue =
            {
              Issue.number;
              title = draft.title;
              description = draft.description;
              status = draft.status;
              priority = draft.priority;
              assignee = None;
              labels = [];
              created_at = now;
              updated_at = now;
              version = 1;
            }
          in
          t.issues <- t.issues @ [ issue ];
          Ok issue
      )

let update_issue t ~now (request : Change.request) =
  match Change.check request.change with
  | Error message ->
      Error message
  | Ok change ->
      locked t (fun () ->
          match
            List.find_opt
              (fun (i : Issue.t) -> i.number = request.number)
              t.issues
          with
          | None ->
              Error ("There is no issue ENG-" ^ string_of_int request.number)
          | Some current when current.version <> request.base ->
              Ok (Change.Conflict current)
          | Some current ->
              let issue =
                {
                  (Change.apply current change) with
                  version = current.version + 1;
                  updated_at = now;
                }
              in
              t.issues <-
                List.map
                  (fun (i : Issue.t) ->
                    if i.number = issue.number then
                      issue
                    else
                      i
                  )
                  t.issues;
              Ok (Change.Saved issue)
      )

let comments_of t number =
  List.filter (fun (c : Comment.t) -> c.number = number) t.comments

let comments t number = locked t (fun () -> comments_of t number)

let add_comment t ~now ~author (input : New_comment.t) =
  match Comment.check input.body with
  | Error message ->
      Error message
  | Ok body ->
      locked t (fun () ->
          if
            not
              (List.exists
                 (fun (i : Issue.t) -> i.number = input.number)
                 t.issues
              )
          then
            Error ("There is no issue ENG-" ^ string_of_int input.number)
          else
            let comment =
              {
                Comment.id = List.length t.comments + 1;
                number = input.number;
                author;
                body;
                created_at = now;
                edited = false;
              }
            in
            t.comments <- t.comments @ [ comment ];
            Ok comment
      )

let edit_comment t ~author (input : Comment_edit.t) =
  match Comment.check input.body with
  | Error message ->
      Error message
  | Ok body ->
      locked t (fun () ->
          match
            List.find_opt (fun (c : Comment.t) -> c.id = input.id) t.comments
          with
          | None ->
              Error "There is no such comment"
          | Some c when c.author <> author ->
              Error "Only the author can edit a comment"
          | Some c ->
              let edited = { c with body; edited = true } in
              t.comments <-
                List.map
                  (fun (o : Comment.t) ->
                    if o.id = c.id then
                      edited
                    else
                      o
                  )
                  t.comments;
              Ok edited
      )

let save t =
  locked t (fun () ->
      Jsonkit.to_string
        (Jsonkit.declassify
           (`Assoc
              [
                ("issues", Issues.to_json t.issues);
                ("comments", Comments.to_json t.comments);
              ]
           )
        )
  )

let load t text =
  match Jsonkit.of_string text with
  | exception Jsonkit.Of_string_error message ->
      Error ("not JSON: " ^ message)
  | json -> (
      let field name =
        match Jsonkit.classify json with
        | `Assoc fields ->
            List.assoc_opt name fields
        | _ ->
            None
      in
      match (field "issues", field "comments") with
      | Some issues, Some comments -> (
          match (Issues.of_json issues, Comments.of_json comments) with
          | issues, comments ->
              locked t (fun () ->
                  t.issues <- issues;
                  t.comments <- comments
              );
              Ok ()
          | exception (Jsonkit.Of_json_error _ as error) ->
              Error (Printexc.to_string error)
        )
      | _ ->
          Error "no issues and comments"
    )

let next_user () =
  let lock = Mutex.create () in
  let count = ref 0 in
  fun () ->
    Mutex.lock lock;
    let user = List.nth User.all (!count mod List.length User.all) in
    incr count;
    Mutex.unlock lock;
    user
