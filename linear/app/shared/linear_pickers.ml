open Silica
open Linear_model

type picker =
  | Status_picker
  | Priority_picker
  | Assignee_picker
  | Labels_picker

let assignee_label = function
  | Some user ->
      User.name user
  | None ->
      "Unassigned"

let menu ~id ~name ~current ~open_ ~on_open_change ~on_pick names label =
  Silica_ui.Menu.make
    ~on_open_change:(fun opened _ -> on_open_change opened)
    ~on_select:(fun value -> on_pick (List.assoc value names))
    (Silica_ui.Menu.props ~id
       ~label:(name ^ ": " ^ label current)
       ~open_ ~classes:Silica_ui.Style.menu
       (List.map
          (fun (value, v) ->
            { Silica_ui.Menu.value; label = label v; disabled = false }
          )
          names
       )
    )

let labels_content (labels : Label.t list) : Label.t list Node.t =
  Node.element "div"
    [ Attr.string "class" "flex flex-col gap-1" ]
    (List.map
       (fun (name, label) ->
         Silica_ui.Checkbox.make
           ~on_checked_change:(fun on ->
             if on then
               Linear_view.canonical Linear_view.label_names (label :: labels)
             else
               List.filter (fun l -> l <> label) labels
           )
           (Silica_ui.Checkbox.props ~id:("label-" ^ name)
              ~label:(Label.label label) ~checked:(List.mem label labels)
              ~classes:Silica_ui.Style.checkbox ()
           )
       )
       Linear_view.label_names
    )

let labels_popover =
  Silica_ui.Popover.component ~id:"labels-picker-content"
    ~closes:(fun (_ : Label.t list) -> false)
    labels_content

let labels_text = function
  | [] ->
      "None"
  | labels ->
      String.concat ", " (List.map Label.label labels)

let view ~(issue : Issue.t) ~(picker : picker option) ~on_open ~on_pick =
  let opened p = picker = Some p in
  let open_change p opened =
    if opened then
      on_open (Some p)
    else
      on_open None
  in
  [
    ( "Status",
      menu ~id:"status-picker" ~name:"Status" ~current:issue.status
        ~open_:(opened Status_picker)
        ~on_open_change:(open_change Status_picker)
        ~on_pick:(fun s -> on_pick (Change.Status s))
        Linear_view.status_names Issue_status.label
    );
    ( "Priority",
      menu ~id:"priority-picker" ~name:"Priority" ~current:issue.priority
        ~open_:(opened Priority_picker)
        ~on_open_change:(open_change Priority_picker)
        ~on_pick:(fun p -> on_pick (Change.Priority p))
        Linear_view.priority_names Priority.label
    );
    ( "Assignee",
      menu ~id:"assignee-picker" ~name:"Assignee" ~current:issue.assignee
        ~open_:(opened Assignee_picker)
        ~on_open_change:(open_change Assignee_picker)
        ~on_pick:(fun a -> on_pick (Change.Assignee a))
        Linear_view.assignee_names assignee_label
    );
    ( "Labels",
      Silica_ui.Popover.make
        ~on_open_change:(fun opened _ -> open_change Labels_picker opened)
        ~on_content:(fun labels -> on_pick (Change.Labels labels))
        labels_popover
        (Silica_ui.Popover.props ~id:"labels-picker"
           ~trigger:("Labels: " ^ labels_text issue.labels)
           ~title:"Labels" ~close:"Done" ~open_:(opened Labels_picker)
           ~classes:Silica_ui.Style.popover issue.labels
        )
    );
  ]
