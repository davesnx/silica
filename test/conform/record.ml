open Silica
open Linear_model
module Memory = Silica_memory

let title = "Quarterly zebra migration"
let body = "Purple elephant memo"

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

let reply request endpoint value =
  Memory.reply request (Codec.to_string (Remote.output endpoint) value)

let rec answer t =
  let pending = Memory.pending t in
  List.iter
    (fun request ->
      match Memory.path request with
      | "/api/me" ->
          reply request me_endpoint User.Ada
      | "/api/issues" ->
          reply request issues_endpoint issues
      | "/api/comments" ->
          reply request comments_endpoint
            { Thread.viewer = User.Ada; comments = [] }
      | "/api/issues/create" ->
          reply request create_endpoint (issue 3 title)
      | "/api/comments/add" ->
          reply request add_comment_endpoint
            {
              Comment.id = 1;
              number = 2;
              author = User.Ada;
              body;
              created_at = 0;
              edited = false;
            }
      | path ->
          failwith ("unexpected call " ^ path)
    )
    pending;
  if pending <> [] then answer t

let visit ~create () =
  Linear_codecs.register ();
  let t = Memory.mount_app ~url:"/" Linear_app.app in
  answer t;
  if create then (
    Memory.press (Memory.by_role ~name:"New issue" t Accessibility.Link);
    let dialog = Memory.by_role ~name:"New issue" t Accessibility.Dialog in
    let field =
      Memory.by_role ~within:dialog ~name:"Title" t Accessibility.Textbox
    in
    Memory.input field title;
    Memory.submit field;
    answer t
  );
  Memory.press (Memory.by_id t "issue-row-2");
  answer t;
  Memory.input (Memory.by_id t "comment-input") body;
  Memory.press (Memory.by_id t "comment-send");
  answer t;
  Memory.trace t
