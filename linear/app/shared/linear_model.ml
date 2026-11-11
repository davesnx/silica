open Jsonkit.Primitives

module Issue_status = struct
  type t = Backlog | Todo | In_progress | Done | Canceled [@@deriving json]

  let all = [ Backlog; Todo; In_progress; Done; Canceled ]

  let label = function
    | Backlog ->
        "Backlog"
    | Todo ->
        "Todo"
    | In_progress ->
        "In Progress"
    | Done ->
        "Done"
    | Canceled ->
        "Canceled"

  let of_label text = List.find_opt (fun t -> label t = text) all
  let is_closed = function
    | Done | Canceled ->
        true
    | Backlog | Todo | In_progress ->
        false
end

module Priority = struct
  type t = No_priority | Urgent | High | Medium | Low [@@deriving json]

  let all = [ No_priority; Urgent; High; Medium; Low ]

  let label = function
    | No_priority ->
        "No priority"
    | Urgent ->
        "Urgent"
    | High ->
        "High"
    | Medium ->
        "Medium"
    | Low ->
        "Low"

  let of_label text = List.find_opt (fun t -> label t = text) all
end

module User = struct
  type t = Ada | Ben | Cleo [@@deriving json]

  let all = [ Ada; Ben; Cleo ]

  let name = function Ada -> "Ada" | Ben -> "Ben" | Cleo -> "Cleo"
end

module Label = struct
  type t = Bug | Feature | Docs | Chore [@@deriving json]

  let label = function
    | Bug ->
        "bug"
    | Feature ->
        "feature"
    | Docs ->
        "docs"
    | Chore ->
        "chore"
end

module Issue = struct
  type t = {
    number : int;
    title : string;
    description : string;
    status : Issue_status.t;
    priority : Priority.t;
    assignee : User.t option;
    labels : Label.t list;
    created_at : int;
    updated_at : int;
    version : int;
  }
  [@@deriving json]

  let identifier t = "ENG-" ^ string_of_int t.number
end

module Issues = struct
  type t = Issue.t list [@@deriving json]
end

module Draft = struct
  type t = {
    title : string;
    description : string;
    status : Issue_status.t;
    priority : Priority.t;
  }
  [@@deriving json]

  let empty =
    {
      title = "";
      description = "";
      status = Issue_status.Backlog;
      priority = Priority.No_priority;
    }

  let max_title = 200
  let max_description = 10_000

  let check t =
    let title = String.trim t.title in
    if title = "" then
      Error "Title is required"
    else if String.length title > max_title then
      Error "Title is too long"
    else if String.length t.description > max_description then
      Error "Description is too long"
    else
      Ok { t with title }
end

let me_endpoint =
  Silica.Remote.declare ~rejected:Silica.Codec.string ~path:"/api/me"
    ~input:Silica.Codec.unit
    ~output:(Silica_jsonkit.codec ~to_json:User.to_json ~of_json:User.of_json)

let issues_endpoint =
  Silica.Remote.declare ~rejected:Silica.Codec.string ~path:"/api/issues"
    ~input:Silica.Codec.unit
    ~output:
      (Silica_jsonkit.codec ~to_json:Issues.to_json ~of_json:Issues.of_json)

let create_endpoint =
  Silica.Remote.declare ~rejected:Silica.Codec.string ~path:"/api/issues/create"
    ~input:(Silica_jsonkit.codec ~to_json:Draft.to_json ~of_json:Draft.of_json)
    ~output:(Silica_jsonkit.codec ~to_json:Issue.to_json ~of_json:Issue.of_json)

let issues_key = "all"

let issues_topic =
  Silica.Topic.declare ~name:"issues"
    ~state:(Silica_jsonkit.codec ~to_json:Issues.to_json ~of_json:Issues.of_json)

module Change = struct
  type t =
    | Title of string
    | Description of string
    | Status of Issue_status.t
    | Priority of Priority.t
    | Assignee of User.t option
    | Labels of Label.t list
  [@@deriving json]

  type request = { number : int; base : int; change : t } [@@deriving json]
  type answer = Saved of Issue.t | Conflict of Issue.t [@@deriving json]

  let apply (issue : Issue.t) = function
    | Title title ->
        { issue with title }
    | Description description ->
        { issue with description }
    | Status status ->
        { issue with status }
    | Priority priority ->
        { issue with priority }
    | Assignee assignee ->
        { issue with assignee }
    | Labels labels ->
        { issue with labels }

  let check = function
    | Title title ->
        let title = String.trim title in
        if title = "" then
          Error "Title is required"
        else if String.length title > Draft.max_title then
          Error "Title is too long"
        else
          Ok (Title title)
    | Description description ->
        if String.length description > Draft.max_description then
          Error "Description is too long"
        else
          Ok (Description description)
    | (Status _ | Priority _ | Assignee _ | Labels _) as change ->
        Ok change

  let field = function
    | Title _ ->
        "title"
    | Description _ ->
        "description"
    | Status _ ->
        "status"
    | Priority _ ->
        "priority"
    | Assignee _ ->
        "assignee"
    | Labels _ ->
        "labels"
end

let update_endpoint =
  Silica.Remote.declare ~rejected:Silica.Codec.string ~path:"/api/issues/update"
    ~input:
      (Silica_jsonkit.codec ~to_json:Change.request_to_json
         ~of_json:Change.request_of_json
      )
    ~output:
      (Silica_jsonkit.codec ~to_json:Change.answer_to_json
         ~of_json:Change.answer_of_json
      )

module Comment = struct
  type t = {
    id : int;
    number : int;
    author : User.t;
    body : string;
    created_at : int;
    edited : bool;
  }
  [@@deriving json]

  let max_body = 10_000

  let check body =
    let body = String.trim body in
    if body = "" then
      Error "The comment is empty"
    else if String.length body > max_body then
      Error "The comment is too long"
    else
      Ok body
end

module Comments = struct
  type t = Comment.t list [@@deriving json]
end

module Thread = struct
  type t = { viewer : User.t; comments : Comment.t list } [@@deriving json]
end

module New_comment = struct
  type t = { number : int; body : string } [@@deriving json]
end

module Comment_edit = struct
  type t = { id : int; body : string } [@@deriving json]
end

let comment_codec =
  Silica_jsonkit.codec ~to_json:Comment.to_json ~of_json:Comment.of_json

let comments_endpoint =
  Silica.Remote.declare ~rejected:Silica.Codec.string ~path:"/api/comments"
    ~input:Silica.Codec.int
    ~output:
      (Silica_jsonkit.codec ~to_json:Thread.to_json ~of_json:Thread.of_json)

let add_comment_endpoint =
  Silica.Remote.declare ~rejected:Silica.Codec.string ~path:"/api/comments/add"
    ~input:
      (Silica_jsonkit.codec ~to_json:New_comment.to_json
         ~of_json:New_comment.of_json
      )
    ~output:comment_codec

let edit_comment_endpoint =
  Silica.Remote.declare ~rejected:Silica.Codec.string ~path:"/api/comments/edit"
    ~input:
      (Silica_jsonkit.codec ~to_json:Comment_edit.to_json
         ~of_json:Comment_edit.of_json
      )
    ~output:comment_codec

let comments_topic =
  Silica.Topic.declare ~name:"comments"
    ~state:
      (Silica_jsonkit.codec ~to_json:Comments.to_json ~of_json:Comments.of_json)
