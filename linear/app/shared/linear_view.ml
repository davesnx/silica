open Jsonkit.Primitives
open Linear_model

module Filter = struct
  type t = {
    statuses : Issue_status.t list;
    assignees : User.t option list;
    priorities : Priority.t list;
    labels : Label.t list;
  }

  let none = { statuses = []; assignees = []; priorities = []; labels = [] }

  let either values value = values = [] || List.mem value values

  let matches t (issue : Issue.t) =
    either t.statuses issue.status
    && either t.assignees issue.assignee
    && either t.priorities issue.priority
    && (t.labels = [] || List.exists (fun l -> List.mem l t.labels) issue.labels)

  let count t =
    List.length t.statuses + List.length t.assignees + List.length t.priorities
    + List.length t.labels
end

type filter = Filter.t = {
  statuses : Issue_status.t list;
  assignees : User.t option list;
  priorities : Priority.t list;
  labels : Label.t list;
}
[@@deriving json]

type grouping = By_status | By_assignee | By_priority | No_grouping
type ordering = Priority_order | Updated | Created
type t = { filter : Filter.t; grouping : grouping; ordering : ordering }

let default = { filter = Filter.none; grouping = By_status; ordering = Created }

type group =
  | Status of Issue_status.t
  | Assignee of User.t option
  | Priority of Priority.t
  | All
[@@deriving json]

let status_names =
  Issue_status.
    [
      ("backlog", Backlog);
      ("todo", Todo);
      ("in_progress", In_progress);
      ("done", Done);
      ("canceled", Canceled);
    ]

let priority_names =
  Priority.
    [
      ("urgent", Urgent);
      ("high", High);
      ("medium", Medium);
      ("low", Low);
      ("none", No_priority);
    ]

let assignee_names =
  [
    ("ada", Some User.Ada);
    ("ben", Some User.Ben);
    ("cleo", Some User.Cleo);
    ("none", None);
  ]

let label_names =
  Label.[ ("bug", Bug); ("feature", Feature); ("docs", Docs); ("chore", Chore) ]

let grouping_names =
  [
    ("status", By_status);
    ("assignee", By_assignee);
    ("priority", By_priority);
    ("none", No_grouping);
  ]

let ordering_names =
  [ ("priority", Priority_order); ("updated", Updated); ("created", Created) ]

let name_of names value = fst (List.find (fun (_, v) -> v = value) names)

let canonical names values =
  List.filter_map
    (fun (_, v) ->
      if List.mem v values then
        Some v
      else
        None
    )
    names

let list_query key names =
  Silica.Route.query ~name:key ~default:[]
    ~parse:(fun text ->
      let parts = String.split_on_char '.' text in
      let values = List.map (fun part -> List.assoc_opt part names) parts in
      if List.mem None values then
        None
      else
        Some (canonical names (List.filter_map Fun.id values))
    )
    ~print:(fun values ->
      String.concat "." (List.map (name_of names) (canonical names values))
    )

let query =
  Silica.Route.(
    map_query
      (fun ((((statuses, assignees), priorities), labels), (grouping, ordering))
         ->
        {
          filter = { Filter.statuses; assignees; priorities; labels };
          grouping;
          ordering;
        }
      )
      (fun { filter; grouping; ordering } ->
        ( ( ((filter.statuses, filter.assignees), filter.priorities),
            filter.labels
          ),
          (grouping, ordering)
        )
      )
      (list_query "status" status_names
      &? list_query "assignee" assignee_names
      &? list_query "priority" priority_names
      &? list_query "label" label_names
      &? (choice_query "group" ~default:By_status grouping_names
         &? choice_query "order" ~default:Created ordering_names
         )
      )
  )

let rank names value =
  let rec go i = function
    | [] ->
        i
    | (_, v) :: rest ->
        if v = value then
          i
        else
          go (i + 1) rest
  in
  go 0 names

let compare_by ordering (a : Issue.t) (b : Issue.t) =
  let newest = compare b.number a.number in
  let first =
    match ordering with
    | Priority_order ->
        compare (rank priority_names a.priority) (rank priority_names b.priority)
    | Updated ->
        compare b.updated_at a.updated_at
    | Created ->
        compare b.created_at a.created_at
  in
  if first = 0 then
    newest
  else
    first

let group_names grouping =
  match grouping with
  | By_status ->
      List.map (fun (_, s) -> Status s) status_names
  | By_assignee ->
      List.map (fun (_, a) -> Assignee a) assignee_names
  | By_priority ->
      List.map (fun (_, p) -> Priority p) priority_names
  | No_grouping ->
      [ All ]

let in_group (issue : Issue.t) = function
  | Status s ->
      issue.status = s
  | Assignee a ->
      issue.assignee = a
  | Priority p ->
      issue.priority = p
  | All ->
      true

let groups t issues =
  let shown =
    List.sort (compare_by t.ordering)
      (List.filter (Filter.matches t.filter) issues)
  in
  List.filter_map
    (fun group ->
      match List.filter (fun issue -> in_group issue group) shown with
      | [] ->
          None
      | members ->
          Some (group, members)
    )
    (group_names t.grouping)

let group_label = function
  | Status s ->
      Issue_status.label s
  | Assignee (Some user) ->
      User.name user
  | Assignee None ->
      "Unassigned"
  | Priority p ->
      Priority.label p
  | All ->
      "All issues"

let group_key = function
  | Status s ->
      "status-" ^ name_of status_names s
  | Assignee a ->
      "assignee-" ^ name_of assignee_names a
  | Priority p ->
      "priority-" ^ name_of priority_names p
  | All ->
      "all"
