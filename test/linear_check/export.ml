open Silica

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let export name = function
  | Check.Ok { spec; edges; _ } ->
      write (spec.Spec.id ^ ".qnt") (Spec.to_quint ~labels:edges spec);
      write (spec.Spec.id ^ ".dot") (Spec.to_dot ~labels:edges spec)
  | _ ->
      failwith ("the check of " ^ name ^ " did not pass")

let children_of name = function
  | Check.Ok { children; _ } ->
      children
  | _ ->
      failwith ("the check of " ^ name ^ " did not pass")

let () =
  let open Linear_checks in
  let router_report = router () in
  let router_children = children_of "router" router_report in
  export "router" router_report;
  export "issue-list" (list ());
  export "issue-detail"
    (detail ~props:(Check.props router_children Linear_detail.component) ());
  export "unsaved" (unsaved ());
  let create_report =
    create ~props:(Check.props router_children Linear_create.component) ()
  in
  export "create-issue" create_report;
  export "create-issue-dialog"
    (create_dialog
       ~props:
         (Check.props
            (children_of "create-issue" create_report)
            Linear_create.dialog_component.component
         )
       ()
    );
  export "me" (me ());
  export "comments" (comments ());
  export "comment-composer" (composer ());
  export "comment" (comment ())
