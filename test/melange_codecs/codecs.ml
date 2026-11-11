open Silica
open Linear_model

let count = ref 0

let round_trip name codec value =
  match codec with
  | Error message ->
      failwith (name ^ ": " ^ message)
  | Ok codec -> (
      let text = Codec.to_string codec value in
      incr count;
      match Codec.of_string codec text with
      | Ok decoded when decoded = value ->
          ()
      | Ok _ ->
          failwith (name ^ ": " ^ text ^ " decodes to another value")
      | Error error ->
          failwith (name ^ ": " ^ text ^ " does not decode: " ^ error)
    )

let events name machine values =
  List.iter (round_trip name (Machine.event_codec machine)) values

let states name machine values =
  List.iter (round_trip name (Machine.state_codec machine)) values

let issue : Issue.t =
  {
    number = 1;
    title = "A title";
    description = "";
    status = Issue_status.Todo;
    priority = Priority.High;
    assignee = Some User.Ada;
    labels = [ Label.Bug ];
    created_at = 1;
    updated_at = 2;
    version = 3;
  }

let comment : Comment.t =
  {
    id = 1;
    number = 1;
    author = User.Ben;
    body = "Hi";
    created_at = 0;
    edited = false;
  }

let () =
  Linear_codecs.register ();
  Example_codecs.register ();
  events "me" Linear_me.machine
    [ Linear_me.Loaded (Ok User.Cleo); Loaded (Failed (Network "down")) ];
  states "me" Linear_me.machine [ Linear_me.Loading; Ready User.Ada; Failed ];
  events "list" Linear_list.machine
    [
      Linear_list.Loaded (Ok [ issue ]);
      Updated { seq = 2; status = Ready { data = [ issue ]; stale = true } };
      Filter_changed
        {
          statuses = [ Issue_status.Done ];
          assignees = [ None ];
          priorities = [];
          labels = [];
        };
      Toggle_group (Linear_view.Assignee (Some User.Ben));
      Saved (1, Ok (Change.Conflict issue));
      Cursor_previous;
    ];
  events "detail" Linear_detail.machine
    [
      Linear_detail.Edit Linear_detail.Title_field;
      Picker (Some Linear_pickers.Labels_picker);
      Pick (Change.Labels [ Label.Docs ]);
      Saved (2, Rejected "Status is locked");
      Keep_editing;
    ];
  events "create" Linear_create.machine
    [ Linear_create.Priority_picked Priority.Low; Created (Ok issue) ];
  states "create" Linear_create.machine
    [
      Linear_create.Closed;
      Failed { draft = Draft.empty; message = "Title is required" };
    ];
  events "comments" Linear_comments.machine
    [
      Linear_comments.Loaded (Ok { viewer = User.Ada; comments = [ comment ] });
      Updated { seq = 0; status = Loading None };
    ];
  states "comment" Linear_comment.machine
    [ Linear_comment.Editing { draft = "x"; error = None }; Edited comment ];
  events "composer" Linear_composer.machine
    [ Linear_composer.Typed "x"; Sent (Failed (Http 500)) ];
  events "todo" Example_todo.machine
    [ Example_todo.Draft "milk"; Add; Toggle 1 ];
  states "votes" Example_votes.machine
    [ Example_votes.Watching (Failed { error = Http 503; last = Some 2 }) ];
  events "vote" Example_vote_button.machine
    [ Remote_machine.Load (); Settled (Rejected "closed") ];
  print_endline
    ("codecs: " ^ string_of_int !count ^ " values round trip under Melange")
