open Linear_model

type command = Create | Go_list | Open of int | Set of Change.t

let item = Silica_ui.Combobox.item

let settings names prefix label make =
  List.map
    (fun (name, value) ->
      ( item (prefix ^ "-" ^ name) (label value),
        (prefix ^ "-" ^ name, Set (make value))
      )
    )
    names

let entries ~(issues : Issue.t list) ~(focused : Issue.t option) =
  let fixed =
    [
      (item "create" "Create issue", ("create", Create));
      (item "list" "Go to list", ("list", Go_list));
    ]
  in
  let changes =
    match focused with
    | None ->
        []
    | Some issue ->
        let on = " of " ^ Issue.identifier issue in
        settings Linear_view.status_names "status"
          (fun s -> "Set status: " ^ Issue_status.label s ^ on)
          (fun s -> Change.Status s)
        @ settings Linear_view.priority_names "priority"
            (fun p -> "Set priority: " ^ Priority.label p ^ on)
            (fun p -> Change.Priority p)
        @ settings Linear_view.assignee_names "assignee"
            (fun a -> "Set assignee: " ^ Linear_pickers.assignee_label a ^ on)
            (fun a -> Change.Assignee a)
  in
  let open_issue (issue : Issue.t) =
    let value = "open-" ^ string_of_int issue.number in
    ( item value (Issue.identifier issue ^ " " ^ issue.title),
      (value, Open issue.number)
    )
  in
  fixed @ changes
  @ List.map open_issue
      (List.sort
         (fun (a : Issue.t) (b : Issue.t) -> compare a.number b.number)
         issues
      )

let parse ~issues ~focused value =
  List.assoc_opt value (List.map snd (entries ~issues ~focused))

let make ~open_ ~on_open_change ~on_run ~issues ~focused =
  Silica_ui.Command_menu.make
    ~on_open_change:(fun opened _ -> on_open_change opened)
    ~on_run
    (Silica_ui.Command_menu.props ~id:"command-menu" ~title:"Command menu"
       ~placeholder:"Type a command or search issues" ~open_
       ~classes:Silica_ui.Style.command_menu
       (List.map fst (entries ~issues ~focused))
    )

let keys open_menu =
  [
    (Silica.Key.make ~ctrl:true "k", open_menu);
    (Silica.Key.make ~meta:true "k", open_menu);
  ]
