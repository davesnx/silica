open Silica
open Linear_model
module Filter = Linear_view.Filter

let toggle values value on =
  if on then
    value :: values
  else
    List.filter (fun v -> v <> value) values

let box ~id ~label ~checked set =
  Silica_ui.Checkbox.make ~on_checked_change:set
    (Silica_ui.Checkbox.props ~id ~label ~checked
       ~classes:Silica_ui.Style.checkbox ()
    )

let field legend boxes =
  Node.element "fieldset"
    [ Attr.string "class" "flex flex-col gap-1" ]
    (Node.element "legend"
       [ Attr.string "class" "mb-1 text-xs font-semibold text-ui-muted" ]
       [ Node.text legend ]
    :: boxes
    )

let group legend key names label (get : Filter.t -> 'a list) set
    (filter : Filter.t) =
  field legend
    (List.map
       (fun (name, value) ->
         box
           ~id:("filter-" ^ key ^ "-" ^ name)
           ~label:(label value)
           ~checked:(List.mem value (get filter))
           (fun on -> set filter (toggle (get filter) value on))
       )
       names
    )

let content (filter : Filter.t) : Filter.t Node.t =
  Node.element "div"
    [ Attr.string "class" "grid grid-cols-2 gap-4" ]
    [
      group "Status" "status" Linear_view.status_names Issue_status.label
        (fun f -> f.statuses)
        (fun f statuses -> { f with statuses })
        filter;
      group "Assignee" "assignee" Linear_view.assignee_names
        (function Some user -> User.name user | None -> "Unassigned")
        (fun f -> f.assignees)
        (fun f assignees -> { f with assignees })
        filter;
      group "Priority" "priority" Linear_view.priority_names Priority.label
        (fun f -> f.priorities)
        (fun f priorities -> { f with priorities })
        filter;
      group "Label" "label" Linear_view.label_names Label.label
        (fun f -> f.labels)
        (fun f labels -> { f with labels })
        filter;
    ]

let popover =
  Silica_ui.Popover.component ~id:"issue-filter-content"
    ~closes:(fun (_ : Filter.t) -> false)
    content

let trigger_label filter =
  match Filter.count filter with
  | 0 ->
      "Filter"
  | n ->
      "Filter (" ^ string_of_int n ^ ")"

let make ~open_ ~on_open_change ~on_change filter =
  Silica_ui.Popover.make ~on_open_change ~on_content:on_change popover
    (Silica_ui.Popover.props ~id:"issue-filter" ~trigger:(trigger_label filter)
       ~title:"Filter" ~close:"Done" ~open_ ~classes:Silica_ui.Style.popover
       filter
    )
