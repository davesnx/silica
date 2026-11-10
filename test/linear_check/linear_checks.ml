open Silica
open Linear_model

let rec exists predicate n =
  predicate (Node.view n)
  ||
  match Node.view n with
  | Node.Element (_, _, children) | Node.List children ->
      List.exists (exists predicate) children
  | Node.Keyed (_, child) ->
      exists predicate child
  | Node.Portal (_, children) ->
      List.exists (exists predicate) children
  | Node.Boundary { body; _ } ->
      exists predicate body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      false

let rec text n =
  match Node.view n with
  | Node.Text t ->
      t
  | Node.Element (_, _, children) | Node.List children ->
      String.concat "" (List.map text children)
  | Node.Keyed (_, child) ->
      text child
  | Node.Portal (_, children) ->
      String.concat "" (List.map text children)
  | Node.Boundary { body; _ } ->
      text body
  | Node.Component _ | Node.Chunk _ ->
      ""

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let shows view needle = contains (text view) needle

let count_rows view =
  let rec go n =
    (match Node.view n with Node.Element ("li", _, _) -> 1 | _ -> 0)
    +
    match Node.view n with
    | Node.Element (_, _, children) | Node.List children ->
        List.fold_left (fun acc child -> acc + go child) 0 children
    | Node.Keyed (_, child) ->
        go child
    | _ ->
        0
  in
  go view

let has_dialog view =
  exists
    (function
      | Node.Element (_, attrs, _) ->
          List.exists
            (function Node.String ("role", "dialog") -> true | _ -> false)
            attrs
      | _ ->
          false
      )
    view

let issue ?(title = "A title") number : Issue.t =
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

let two = [ issue 1; issue 2 ]
let three = [ issue 1; issue 2; issue 3 ]
let issues = Check.answers issues_endpoint [ two ]
let topic = Check.states issues_topic [ two; three ]

let pp_status f : Issues.t Status.t -> unit = function
  | Status.Idle ->
      Format.fprintf f "idle"
  | Status.Loading _ ->
      Format.fprintf f "loading"
  | Status.Ready { data; stale } ->
      Format.fprintf f "%s %d"
        ( if stale then
            "stale"
          else
            "ready"
        )
        (List.length data)
  | Status.Failed _ ->
      Format.fprintf f "failed"

let pp_result f = function
  | Ok _ ->
      Format.fprintf f "ok"
  | Error (Remote.Http code) ->
      Format.fprintf f "http %d" code
  | Error (Remote.Network _) ->
      Format.fprintf f "network"
  | Error (Remote.Decode _) ->
      Format.fprintf f "decode"

let label pp event = Format.asprintf "%a" pp event

let pp_router_event f = function
  | Silica.Router.Url_changed u ->
      Format.fprintf f "url_changed %s" (Url.to_string u)
  | Silica.Router.Preload u ->
      Format.fprintf f "preload %s" (Url.to_string u)

let router_label = function
  | Silica.Router.Url_changed _ ->
      "url changed"
  | Silica.Router.Preload _ ->
      "preload"

let router () =
  let open Linear_routes in
  Check.app ~max_states:500_000 ~label:router_label
    ~inputs:(fun _ ->
      List.map
        (fun path -> Silica.Router.Url_changed (Url.of_string path))
        [
          "/";
          "/?new=true";
          "/?status=done";
          "/issue/1";
          "/issue/2";
          "/issue/9";
          "/nowhere";
        ]
    )
    ~routes:
      [
        Router.At (State.Issues, list);
        Router.At (State.Issues, { list with create = true });
        Router.At (State.Issue, 1);
        Router.At (State.Issue, 2);
        Router.At (State.Not_found, "/nowhere");
      ]
    ~parse:(Router.parse routes) ~print:(Router.print routes)
    ~is_not_found:(Router.is_not_found routes)
    ~url_event:(fun u -> Silica.Router.Url_changed u)
    Linear_app.app

let list_label = function
  | Linear_list.Props view ->
      if view = Linear_view.default then
        "props default"
      else
        "props filtered"
  | Linear_list.Loaded (Ok _) ->
      "loaded ok"
  | Linear_list.Loaded (Error _) ->
      "loaded error"
  | Linear_list.Updated _ ->
      "updated"
  | Linear_list.Retry ->
      "retry"
  | Linear_list.Reconnect ->
      "reconnect"
  | Linear_list.Filter_open b ->
      "filter open " ^ string_of_bool b
  | Linear_list.Filter_changed _ ->
      "filter changed"
  | Linear_list.Grouping_picked _ ->
      "grouping picked"
  | Linear_list.Ordering_picked _ ->
      "ordering picked"
  | Linear_list.Toggle_group _ ->
      "toggle group"
  | Linear_list.Cursor_next ->
      "cursor next"
  | Linear_list.Cursor_previous ->
      "cursor previous"
  | Linear_list.Row_focused _ ->
      "row focused"
  | Linear_list.Menu_open b ->
      "menu open " ^ string_of_bool b
  | Linear_list.Command _ ->
      "command"
  | Linear_list.Create_key ->
      "create key"
  | Linear_list.Saved (_, Ok (Change.Saved _)) ->
      "saved"
  | Linear_list.Saved (_, Ok (Change.Conflict _)) ->
      "conflict"
  | Linear_list.Saved (_, Error _) ->
      "save failed"
  | Linear_list.Toast_closed _ ->
      "toast closed"

let pp_list_event f = function
  | Linear_list.Loaded r ->
      Format.fprintf f "loaded %a" pp_result r
  | Linear_list.Updated { seq; status } ->
      Format.fprintf f "updated %d %a" seq pp_status status
  | Linear_list.Retry ->
      Format.fprintf f "retry"
  | Linear_list.Reconnect ->
      Format.fprintf f "reconnect"
  | event ->
      Format.pp_print_string f (list_label event)

let list () =
  let open Linear_list in
  let invariants =
    [
      {
        Check.name = "never shows loading and rows";
        holds =
          (fun _ view ->
            not (shows view "Loading issues" && count_rows view > 0)
          );
      };
      {
        Check.name = "a ready list shows one row for each issue";
        holds =
          (fun (M.At (state, data) : M.snapshot) view ->
            match state with
            | States.Ready ->
                count_rows view
                = List.length
                    (List.concat_map
                       (fun (group, members) ->
                         if List.mem group data.collapsed then
                           []
                         else
                           members
                       )
                       (Linear_view.groups data.view
                          (Linear_data.issues data.data)
                       )
                    )
            | States.Loading | States.Failed ->
                true
          );
      };
      {
        Check.name = "the offline note shows exactly when the data is stale";
        holds =
          (fun (M.At (state, data) : M.snapshot) view ->
            match state with
            | States.Ready ->
                shows view "Offline" = data.data.stale
            | States.Loading | States.Failed ->
                not (shows view "Offline")
          );
      };
    ]
  in
  let filtered =
    {
      Linear_view.default with
      filter = { Linear_view.Filter.none with statuses = [ Issue_status.Done ] };
    }
  in
  Check.component ~max_states:50_000 ~samples:[ issues; topic ]
    ~props:[ Props Linear_view.default; Props filtered ]
    ~within:(fun (M.At (state, data)) ->
      match state with
      | States.Ready ->
          data.data.next_id <= 2 && List.length data.toasts <= 1
      | States.Loading | States.Failed ->
          true
    )
    ~inputs:(fun (M.At (state, data)) ->
      match state with
      | States.Ready ->
          [
            Filter_open false;
            Filter_changed filtered.filter;
            Grouping_picked "assignee";
            Ordering_picked "priority";
            Menu_open false;
            Command "create";
            Command "open-1";
            Command "status-done";
          ]
          @ List.concat_map
              (fun (id, (r : Linear_data.request)) ->
                [
                  Saved (id, Error (Remote.Network "down"));
                  Saved
                    ( id,
                      Ok
                        (Change.Conflict
                           { (issue r.number) with version = r.base + 1 }
                        )
                    );
                ]
              )
              (Linear_data.requests data.data)
          @ List.map
              (fun (t : Silica_ui.Toast.toast) -> Toast_closed t.key)
              data.toasts
      | States.Loading | States.Failed ->
          []
    )
    ~label:list_label ~invariants Linear_list.component

let detail_label = function
  | Linear_detail.Props _ ->
      "props"
  | Linear_detail.Loaded (Ok _) ->
      "loaded ok"
  | Linear_detail.Loaded (Error _) ->
      "loaded error"
  | Linear_detail.Updated _ ->
      "updated"
  | Linear_detail.Retry ->
      "retry"
  | Linear_detail.Reconnect ->
      "reconnect"
  | Linear_detail.Edit _ ->
      "edit"
  | Linear_detail.Draft_changed _ ->
      "draft"
  | Linear_detail.Save ->
      "save"
  | Linear_detail.Cancel_edit ->
      "cancel edit"
  | Linear_detail.Picker (Some _) ->
      "picker open"
  | Linear_detail.Picker None ->
      "picker closed"
  | Linear_detail.Pick _ ->
      "pick"
  | Linear_detail.Saved (_, Ok (Change.Saved _)) ->
      "saved"
  | Linear_detail.Saved (_, Ok (Change.Conflict _)) ->
      "conflict"
  | Linear_detail.Saved (_, Error _) ->
      "save failed"
  | Linear_detail.Toast_closed _ ->
      "toast closed"
  | Linear_detail.Menu_open b ->
      "menu open " ^ string_of_bool b
  | Linear_detail.Command _ ->
      "command"
  | Linear_detail.Create_key ->
      "create key"
  | Linear_detail.Keep_editing ->
      "keep editing"

let pp_detail_event f = function
  | Linear_detail.Props n ->
      Format.fprintf f "props %d" n
  | Linear_detail.Loaded r ->
      Format.fprintf f "loaded %a" pp_result r
  | Linear_detail.Updated { seq; status } ->
      Format.fprintf f "updated %d %a" seq pp_status status
  | Linear_detail.Retry ->
      Format.fprintf f "retry"
  | Linear_detail.Reconnect ->
      Format.fprintf f "reconnect"
  | event ->
      Format.pp_print_string f (detail_label event)

let detail_inputs (Linear_detail.M.At (state, data) : Linear_detail.M.snapshot)
    =
  let open Linear_detail in
  match state with
  | States.Ready ->
      let current number = Option.get (Linear_data.find number data.data) in
      [
        Draft_changed "x";
        Draft_changed "";
        Pick (Change.Status Issue_status.Done);
        Picker (Some Linear_pickers.Status_picker);
        Picker None;
        Menu_open false;
        Command "priority-urgent";
        Keep_editing;
      ]
      @ List.concat_map
          (fun (id, (r : Linear_data.request)) ->
            let issue = current r.number in
            [
              Saved
                ( id,
                  Ok
                    (Change.Saved
                       { issue with version = r.base + 1; title = "saved" }
                    )
                );
              Saved
                ( id,
                  Ok
                    (Change.Conflict
                       { issue with version = r.base + 1; title = "theirs" }
                    )
                );
              Saved (id, Error (Remote.Network "down"));
            ]
          )
          (Linear_data.requests data.data)
      @ List.map
          (fun (t : Silica_ui.Toast.toast) -> Toast_closed t.key)
          data.toasts
  | States.Idle | States.Loading | States.Not_found | States.Failed ->
      []

let detail_within (Linear_detail.M.At (state, data) : Linear_detail.M.snapshot)
    =
  match state with
  | Linear_detail.States.Ready ->
      data.data.next_id <= 2 && List.length data.toasts <= 1
  | _ ->
      true

let detail ~props () =
  let open Linear_detail in
  let invariants =
    [
      {
        Check.name = "ready shows the title of the issue";
        holds =
          (fun (M.At (state, data) : M.snapshot) view ->
            match state with
            | States.Ready -> (
                match Linear_data.find data.number data.data with
                | Some _ when Option.is_some data.editor ->
                    true
                | Some issue ->
                    shows view issue.title
                | None ->
                    shows view "There is no issue"
              )
            | _ ->
                true
          );
      };
      {
        Check.name = "never shows loading and an issue";
        holds =
          (fun _ view ->
            not (shows view "Loading issue" && shows view "No description.")
          );
      };
      {
        Check.name = "every pending change has a call in flight";
        holds =
          (fun (M.At (state, data) : M.snapshot) _ ->
            match state with
            | States.Ready ->
                Linear_data.pending data.data
                <= List.length (Linear_data.requests data.data)
            | _ ->
                true
          );
      };
    ]
  in
  Check.component ~max_states:400_000 ~props ~within:detail_within
    ~inputs:detail_inputs ~samples:[ issues; topic ] ~label:detail_label
    ~invariants Linear_detail.component

let unsaved_label = function
  | Linear_unsaved.Attempt (Nav.Push _) ->
      "attempt push"
  | Linear_unsaved.Attempt (Nav.Replace _) ->
      "attempt replace"
  | Linear_unsaved.Attempt (Nav.Traverse _) ->
      "attempt traverse"
  | Linear_unsaved.Discard ->
      "discard"
  | Linear_unsaved.Keep ->
      "keep"

let pp_unsaved_event f event = Format.pp_print_string f (unsaved_label event)

let unsaved () =
  let open Linear_unsaved in
  let invariants =
    [
      {
        Check.name = "the confirm shows only while a navigation waits";
        holds =
          (fun (M.At (state, _) : M.snapshot) view ->
            M.same state States.Asking = shows view "unsaved changes"
          );
      };
      {
        Check.name = "the block is held in every state";
        holds = (fun snapshot _ -> Machine.held machine snapshot <> []);
      };
    ]
  in
  Check.component ~label:unsaved_label ~invariants Linear_unsaved.component

let pp_create_event f = function
  | Linear_create.Props { create; view } ->
      Format.fprintf f "props %b%s" create
        ( if view = Linear_view.default then
            ""
          else
            " filtered"
        )
  | Linear_create.Title s ->
      Format.fprintf f "title %S" s
  | Linear_create.Description s ->
      Format.fprintf f "description %S" s
  | Linear_create.Status_picked s ->
      Format.fprintf f "status %s" (Issue_status.label s)
  | Linear_create.Priority_picked p ->
      Format.fprintf f "priority %s" (Priority.label p)
  | Linear_create.Submit ->
      Format.fprintf f "submit"
  | Linear_create.Cancel ->
      Format.fprintf f "cancel"
  | Linear_create.Created r ->
      Format.fprintf f "created %a" pp_result r

let create_label = function
  | Linear_create.Props _ ->
      "props"
  | Linear_create.Title _ ->
      "title"
  | Linear_create.Description _ ->
      "description"
  | Linear_create.Status_picked _ ->
      "status"
  | Linear_create.Priority_picked _ ->
      "priority"
  | Linear_create.Submit ->
      "submit"
  | Linear_create.Cancel ->
      "cancel"
  | Linear_create.Created (Ok _) ->
      "created ok"
  | Linear_create.Created (Error _) ->
      "created error"

let rec requests : type s e. (s, e) Component.t -> _ Node.t -> e list =
 fun component n ->
  match Node.view n with
  | Node.Component child -> (
      match
        Type.Id.provably_equal
          (Machine.type_id child.component.machine)
          (Machine.type_id component.machine)
      with
      | Some Type.Equal ->
          Option.to_list child.props
      | None ->
          []
    )
  | Node.Element (_, _, children)
  | Node.List children
  | Node.Portal (_, children) ->
      List.concat_map (requests component) children
  | Node.Keyed (_, child) ->
      requests component child
  | Node.Boundary { body; _ } ->
      requests component body
  | Node.Chunk chunk ->
      requests component (Node.chunk_view chunk)
  | Node.Text _ ->
      []

let dialog_request view =
  match requests Linear_create.Create_dialog.component view with
  | [ Linear_create.Create_dialog.Props props ] ->
      props
  | _ ->
      failwith "the create view has not exactly one dialog with props"

let create_inputs (Linear_create.M.At (state, _) : Linear_create.M.snapshot) =
  let open Linear_create in
  match state with
  | States.Closed | States.Submitting ->
      []
  | States.Editing | States.Failed ->
      [
        Title "";
        Title "x";
        Description "d";
        Status_picked Issue_status.Done;
        Priority_picked Priority.Urgent;
        Submit;
        Cancel;
      ]

let create_invariants =
  let open Linear_create in
  [
    {
      Check.name =
        "the dialog is asked open exactly when the machine is not closed";
      holds =
        (fun (M.At (state, _) : M.snapshot) view ->
          (dialog_request view).open_
          = Some
              ( match state with
              | States.Closed ->
                  false
              | States.Editing | States.Submitting | States.Failed ->
                  true
              )
        );
    };
    {
      Check.name = "the dialog cannot be dismissed while sending";
      holds =
        (fun (M.At (state, _) : M.snapshot) view ->
          let request = dialog_request view in
          match state with
          | States.Submitting ->
              (not request.dismissible) && request.content.busy
          | _ ->
              request.dismissible && not request.content.busy
        );
    };
    {
      Check.name = "only a draft with a title is ever sent";
      holds =
        (fun (M.At (state, data) : M.snapshot) _ ->
          match state with
          | States.Submitting ->
              let _, (draft : Draft.t) = data in
              String.trim draft.title <> ""
          | _ ->
              true
        );
    };
    {
      Check.name = "the message of a failed send is shown";
      holds =
        (fun (M.At (state, _) : M.snapshot) view ->
          let message = (dialog_request view).content.message in
          match state with
          | States.Failed ->
              Option.fold ~none:false
                ~some:(fun m -> contains m "Could not create the issue")
                message
          | _ ->
              true
        );
    };
    {
      Check.name = "the title error shows only in editing";
      holds =
        (fun (M.At (state, form) : M.snapshot) view ->
          let message = (dialog_request view).content.message in
          match state with
          | States.Editing ->
              message = form.error
          | _ ->
              message <> Some "Title is required"
        );
    };
  ]

let create ?(invariants = []) ~props () =
  Check.component ~max_states:100_000 ~props
    ~samples:[ Check.answers create_endpoint [ issue 3 ] ]
    ~inputs:create_inputs ~label:create_label
    ~invariants:(create_invariants @ invariants)
    Linear_create.component

let dialog_label : Linear_create.Create_dialog.event -> string = function
  | Props { open_ = Some true; dismissible = true; _ } ->
      "props open"
  | Props { open_ = Some true; dismissible = false; _ } ->
      "props busy"
  | Props _ ->
      "props closed"
  | Open_request _ ->
      "open"
  | Close_request reason ->
      "close " ^ Silica_ui.Popup.reason_name reason
  | Content event ->
      create_label event

let pp_dialog_event f event = Format.pp_print_string f (dialog_label event)

let create_dialog ~props () =
  Check.component ~props ~label:dialog_label
    Linear_create.Create_dialog.component

let pp_me_event f = function
  | Linear_me.Loaded r ->
      Format.fprintf f "loaded %a" pp_result r

let me_label = function
  | Linear_me.Loaded (Ok _) ->
      "loaded ok"
  | Linear_me.Loaded (Error _) ->
      "loaded error"

let me () =
  Check.component
    ~samples:[ Check.answers me_endpoint [ User.Ada ] ]
    ~label:me_label Linear_me.component

let a_comment ?(author = User.Ada) id body : Comment.t =
  { id; number = 1; author; body; created_at = 0; edited = false }

let comments_label = function
  | Linear_comments.Props _ ->
      "props"
  | Linear_comments.Loaded (Ok _) ->
      "loaded ok"
  | Linear_comments.Loaded (Error _) ->
      "loaded error"
  | Linear_comments.Updated _ ->
      "updated"
  | Linear_comments.Retry ->
      "retry"

let pp_comments_event f event = Format.pp_print_string f (comments_label event)

let comments () =
  let thread =
    {
      Thread.viewer = User.Ada;
      comments = [ a_comment 1 "Mine"; a_comment ~author:User.Ben 2 "Ben's" ];
    }
  in
  let invariants =
    [
      {
        Check.name = "ready shows every comment and one composer";
        holds =
          (fun (Linear_comments.M.At (state, data) : Linear_comments.M.snapshot)
            view
          ->
            match state with
            | Linear_comments.States.Ready ->
                let rec count n =
                  match Node.view n with
                  | Node.Component _ ->
                      1
                  | Node.Element (_, _, children) | Node.List children ->
                      List.fold_left (fun acc c -> acc + count c) 0 children
                  | Node.Keyed (_, child) ->
                      count child
                  | _ ->
                      0
                in
                count view = List.length data.comments + 1
            | _ ->
                true
          );
      };
    ]
  in
  Check.component
    ~props:[ Linear_comments.Props 1; Linear_comments.Props 2 ]
    ~samples:
      [
        Check.answers comments_endpoint [ thread ];
        Check.states comments_topic
          [ thread.comments; thread.comments @ [ a_comment 3 "New" ] ];
      ]
    ~label:comments_label ~invariants Linear_comments.component

let composer_label = function
  | Linear_composer.Props _ ->
      "props"
  | Linear_composer.Typed "" ->
      "typed empty"
  | Linear_composer.Typed _ ->
      "typed"
  | Linear_composer.Send ->
      "send"
  | Linear_composer.Clear ->
      "clear"
  | Linear_composer.Sent (Ok _) ->
      "sent ok"
  | Linear_composer.Sent (Error _) ->
      "sent error"

let pp_composer_event f event = Format.pp_print_string f (composer_label event)

let composer () =
  let invariants =
    [
      {
        Check.name = "only a comment with text is ever sent";
        holds =
          (fun (Linear_composer.M.At (state, data) : Linear_composer.M.snapshot)
            _
          ->
            match state with
            | Linear_composer.States.Sending ->
                String.trim (snd data) <> ""
            | _ ->
                true
          );
      };
    ]
  in
  Check.component
    ~props:[ Linear_composer.Props 1; Linear_composer.Props 2 ]
    ~inputs:(fun _ -> [ Linear_composer.Typed "x"; Linear_composer.Typed " " ])
    ~samples:[ Check.answers add_comment_endpoint [ a_comment 3 "x" ] ]
    ~label:composer_label ~invariants Linear_composer.component

let comment_label = function
  | Linear_comment.Props { own = true; _ } ->
      "props own"
  | Linear_comment.Props { own = false; _ } ->
      "props other"
  | Linear_comment.Edit ->
      "edit"
  | Linear_comment.Typed _ ->
      "typed"
  | Linear_comment.Save ->
      "save"
  | Linear_comment.Cancel ->
      "cancel"
  | Linear_comment.Saved (Ok _) ->
      "saved ok"
  | Linear_comment.Saved (Error _) ->
      "saved error"

let pp_comment_event f event = Format.pp_print_string f (comment_label event)

let comment () =
  let mine = a_comment 1 "Mine" in
  let invariants =
    [
      {
        Check.name = "only the author can edit";
        holds =
          (fun (Linear_comment.M.At (state, data) : Linear_comment.M.snapshot)
            _
          ->
            match state with
            | Linear_comment.States.Editing ->
                data.props.own
            | Linear_comment.States.Saving ->
                (fst data).own
            | _ ->
                true
          );
      };
    ]
  in
  Check.component
    ~props:
      [
        Linear_comment.Props { comment = mine; own = true };
        Linear_comment.Props { comment = mine; own = false };
      ]
    ~inputs:(fun _ ->
      [ Linear_comment.Edit; Linear_comment.Typed "x"; Linear_comment.Typed "" ]
    )
    ~samples:
      [ Check.answers edit_comment_endpoint [ { mine with body = "x" } ] ]
    ~label:comment_label ~invariants Linear_comment.component
