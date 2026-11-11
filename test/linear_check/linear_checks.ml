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

let pp_result f : (_, string) Remote.answer -> unit = function
  | Ok _ ->
      Format.fprintf f "ok"
  | Rejected message ->
      Format.fprintf f "rejected %S" message
  | Failed (Remote.Http code) ->
      Format.fprintf f "http %d" code
  | Failed (Remote.Network _) ->
      Format.fprintf f "network"
  | Failed (Remote.Decode _) ->
      Format.fprintf f "decode"

let label pp event = Format.asprintf "%a" pp event

let rec children : type p s e. (p, s, e) Component.t -> _ Node.t -> p list =
 fun component n ->
  match Node.view n with
  | Node.Component child -> (
      match
        Type.Id.provably_equal
          (Machine.type_id child.component.machine)
          (Machine.type_id component.machine)
      with
      | Some Type.Equal ->
          [ child.props ]
      | None ->
          []
    )
  | Node.Element (_, _, nodes) | Node.List nodes | Node.Portal (_, nodes) ->
      List.concat_map (children component) nodes
  | Node.Keyed (_, child) ->
      children component child
  | Node.Boundary { body; _ } ->
      children component body
  | Node.Chunk chunk ->
      children component (Node.chunk_view chunk)
  | Node.Text _ ->
      []

let saves =
  Check.answers ~rejected:[ "Status is locked" ] update_endpoint
    [
      Change.Saved { (issue 1) with version = 2; title = "saved" };
      Change.Conflict { (issue 1) with version = 2; title = "theirs" };
    ]

let pp_router_event f = function
  | Silica.Router.Url_changed u ->
      Format.fprintf f "url_changed %s" (Url.to_string u)
  | Silica.Router.Preload u ->
      Format.fprintf f "preload %s" (Url.to_string u)

let router () =
  let open Linear_routes in
  Check.app ~max_states:500_000
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
        Issues list;
        Issues { list with create = true };
        Issue 1;
        Issue 2;
        Not_found "/nowhere";
      ]
    ~parse:(Router.parse routes) ~print:(Router.print routes)
    ~is_not_found:(Router.is_not_found routes)
    ~url_event:(fun u -> Silica.Router.Url_changed u)
    Linear_app.app

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
      Format.pp_print_string f (Machine.event_name Linear_list.machine event)

let shown_view view =
  let value id =
    List.find_map
      (fun (props : Silica_ui.Select.props) ->
        if props.id = id then
          props.value
        else
          None
      )
      (children Silica_ui.Select.component view)
  in
  let filter =
    match children Linear_filter.popover.component view with
    | [ props ] ->
        props.content
    | _ ->
        failwith "the list view has not exactly one filter"
  in
  let pick names id =
    Option.bind (value id) (fun name -> List.assoc_opt name names)
  in
  match
    ( pick Linear_view.grouping_names "issue-grouping",
      pick Linear_view.ordering_names "issue-ordering"
    )
  with
  | Some grouping, Some ordering ->
      { Linear_view.filter; grouping; ordering }
  | _ ->
      failwith "the list view does not show its grouping and ordering"

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
          (fun state view ->
            match state with
            | Ready r ->
                count_rows view
                = List.length
                    (List.concat_map
                       (fun (group, members) ->
                         if List.mem group r.collapsed then
                           []
                         else
                           members
                       )
                       (Linear_view.groups (shown_view view)
                          (Linear_data.issues r.data)
                       )
                    )
            | Loading | Failed _ ->
                true
          );
      };
      {
        Check.name = "the offline note shows exactly when the data is stale";
        holds =
          (fun state view ->
            match state with
            | Ready r ->
                shows view "Offline" = r.data.stale
            | Loading | Failed _ ->
                not (shows view "Offline")
          );
      };
      {
        Check.name = "every pending change has a call in flight";
        holds =
          (fun state _ ->
            match state with
            | Ready r ->
                Linear_data.pending r.data
                <= List.length (Linear_data.requests r.data)
            | Loading | Failed _ ->
                true
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
  Check.component ~max_states:50_000 ~samples:[ issues; topic; saves ]
    ~props:[ Linear_view.default; filtered ]
    ~within:(function
      | Ready r ->
          r.data.next_id <= 2 && List.length r.toasts <= 1
      | Loading | Failed _ ->
          true
      )
    ~inputs:(function
      | Ready r ->
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
          @ List.map
              (fun (t : Silica_ui.Toast.toast) -> Toast_closed t.key)
              r.toasts
      | Loading | Failed _ ->
          []
      )
    ~invariants Linear_list.component

let pp_detail_event f = function
  | Linear_detail.Loaded r ->
      Format.fprintf f "loaded %a" pp_result r
  | Linear_detail.Updated { seq; status } ->
      Format.fprintf f "updated %d %a" seq pp_status status
  | Linear_detail.Retry ->
      Format.fprintf f "retry"
  | Linear_detail.Reconnect ->
      Format.fprintf f "reconnect"
  | event ->
      Format.pp_print_string f (Machine.event_name Linear_detail.machine event)

let detail_inputs (state : Linear_detail.state) : Linear_detail.event list =
  let open Linear_detail in
  match state with
  | Ready state ->
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
      @ List.map
          (fun (t : Silica_ui.Toast.toast) -> Toast_closed t.key)
          state.toasts
  | Loading | Not_found | Failed _ ->
      []

let detail_within : Linear_detail.state -> bool = function
  | Ready state ->
      state.data.next_id <= 2 && List.length state.toasts <= 1
  | Loading | Not_found | Failed _ ->
      true

let detail ~props () =
  let open Linear_detail in
  let invariants =
    [
      {
        Check.name = "ready shows the title of the issue";
        holds =
          (fun state view ->
            match state with
            | Ready data -> (
                match Linear_data.find data.number data.data with
                | Some _ when Option.is_some data.editor ->
                    true
                | Some issue ->
                    shows view issue.title
                | None ->
                    shows view "There is no issue"
              )
            | Loading | Not_found | Failed _ ->
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
          (fun state _ ->
            match state with
            | Ready data ->
                Linear_data.pending data.data
                <= List.length (Linear_data.requests data.data)
            | Loading | Not_found | Failed _ ->
                true
          );
      };
    ]
  in
  Check.component ~max_states:400_000 ~props ~within:detail_within
    ~inputs:detail_inputs ~samples:[ issues; topic; saves ] ~invariants
    Linear_detail.component

let pp_unsaved_event f event =
  Format.pp_print_string f (Machine.event_name Linear_unsaved.machine event)

let unsaved () =
  let open Linear_unsaved in
  let invariants =
    [
      {
        Check.name = "the confirm shows only while a navigation waits";
        holds =
          (fun state view ->
            (match state with Asking _ -> true | Held -> false)
            = shows view "unsaved changes"
          );
      };
      {
        Check.name = "the block is held in every state";
        holds = (fun state _ -> Machine.held machine () state <> []);
      };
    ]
  in
  Check.component ~invariants Linear_unsaved.component

let pp_create_props f ({ create; view } : Linear_routes.issues) =
  Format.fprintf f "props %b%s" create
    ( if view = Linear_view.default then
        ""
      else
        " filtered"
    )

let pp_create_event f = function
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

let dialog_request view =
  match children Linear_create.dialog_component.component view with
  | [ props ] ->
      props
  | _ ->
      failwith "the create view has not exactly one dialog"

let create_inputs : Linear_create.state -> Linear_create.event list = function
  | Closed | Submitting _ ->
      []
  | Editing _ | Failed _ ->
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
        (fun state view ->
          (dialog_request view).open_
          = Some
              ( match state with
              | Closed ->
                  false
              | Editing _ | Submitting _ | Failed _ ->
                  true
              )
        );
    };
    {
      Check.name = "the dialog cannot be dismissed while sending";
      holds =
        (fun state view ->
          let request = dialog_request view in
          match state with
          | Submitting _ ->
              (not request.dismissible) && request.content.busy
          | Closed | Editing _ | Failed _ ->
              request.dismissible && not request.content.busy
        );
    };
    {
      Check.name = "only a draft with a title is ever sent";
      holds =
        (fun state _ ->
          match state with
          | Submitting draft ->
              String.trim draft.title <> ""
          | Closed | Editing _ | Failed _ ->
              true
        );
    };
    {
      Check.name = "the message of a failed send is shown";
      holds =
        (fun state view ->
          let message = (dialog_request view).content.message in
          match state with
          | Failed _ ->
              Option.fold ~none:false
                ~some:(fun m -> contains m "Could not create the issue")
                message
          | Closed | Editing _ | Submitting _ ->
              true
        );
    };
    {
      Check.name = "the title error shows only in editing";
      holds =
        (fun state view ->
          let message = (dialog_request view).content.message in
          match state with
          | Editing form ->
              message = form.error
          | Closed | Submitting _ | Failed _ ->
              message <> Some "Title is required"
        );
    };
  ]

let create ?(invariants = []) ~props () =
  Check.component ~max_states:100_000 ~props
    ~samples:
      [
        Check.answers ~rejected:[ "Title is too long" ] create_endpoint
          [ issue 3 ];
      ]
    ~inputs:create_inputs
    ~invariants:(create_invariants @ invariants)
    Linear_create.component

let dialog_props_label :
    (Linear_create.content, Linear_create.event) Silica_ui.Dialog.props ->
    string = function
  | { open_ = Some true; dismissible = true; _ } ->
      "props open"
  | { open_ = Some true; dismissible = false; _ } ->
      "props busy"
  | _ ->
      "props closed"

let pp_dialog_event f event =
  Format.pp_print_string f
    (Machine.event_name Linear_create.dialog_component.component.machine event)

let create_dialog ~props () =
  Check.component ~props Linear_create.dialog_component.component

let pp_me_event f = function
  | Linear_me.Loaded r ->
      Format.fprintf f "loaded %a" pp_result r

let me () =
  Check.component
    ~samples:[ Check.answers me_endpoint [ User.Ada ] ]
    Linear_me.component

let a_comment ?(author = User.Ada) id body : Comment.t =
  { id; number = 1; author; body; created_at = 0; edited = false }

let pp_comments_event f event =
  Format.pp_print_string f (Machine.event_name Linear_comments.machine event)

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
          (fun (state : Linear_comments.state) view ->
            match state with
            | Ready data ->
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
            | Loading | Failed ->
                true
          );
      };
    ]
  in
  Check.component ~props:[ 1; 2 ]
    ~samples:
      [
        Check.answers comments_endpoint [ thread ];
        Check.states comments_topic
          [ thread.comments; thread.comments @ [ a_comment 3 "New" ] ];
      ]
    ~invariants Linear_comments.component

let pp_composer_event f event =
  Format.pp_print_string f (Machine.event_name Linear_composer.machine event)

let composer () =
  let invariants =
    [
      {
        Check.name = "only a comment with text is ever sent";
        holds =
          (fun (state : Linear_composer.state) _ ->
            match state with
            | Sending body ->
                String.trim body <> ""
            | Empty | Drafting _ | Failed _ ->
                true
          );
      };
    ]
  in
  Check.component ~props:[ 1; 2 ]
    ~inputs:(fun _ -> [ Linear_composer.Typed "x"; Linear_composer.Typed " " ])
    ~samples:
      [
        Check.answers
          ~rejected:[ "The comment is too long" ]
          add_comment_endpoint
          [ a_comment 3 "x" ];
      ]
    ~invariants Linear_composer.component

let pp_comment_event f event =
  Format.pp_print_string f (Machine.event_name Linear_comment.machine event)

let mine = a_comment 1 "Mine"

let comment ?(props = [ (mine, true); (mine, false) ]) () =
  Check.component
    ~props:
      (List.map (fun (comment, own) -> { Linear_comment.comment; own }) props)
    ~inputs:(fun _ ->
      [ Linear_comment.Edit; Linear_comment.Typed "x"; Linear_comment.Typed "" ]
    )
    ~samples:
      [
        Check.answers
          ~rejected:[ "Only the author can edit" ]
          edit_comment_endpoint
          [ { mine with body = "x"; edited = true } ];
      ]
    Linear_comment.component
