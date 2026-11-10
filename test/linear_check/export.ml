open Silica

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let export spec = function
  | Check.Ok { edges; _ } ->
      write (spec.Spec.id ^ ".qnt") (Spec.to_quint ~labels:edges spec);
      write (spec.Spec.id ^ ".dot") (Spec.to_dot ~labels:edges spec)
  | _ ->
      failwith ("the check of " ^ spec.Spec.id ^ " did not pass")

let () =
  let open Linear_checks in
  let router_report = router () in
  let children =
    match router_report with
    | Check.Ok { children; _ } ->
        children
    | _ ->
        failwith "the check of the router did not pass"
  in
  export
    (Machine.spec (Linear_app.app (Url.of_string "/")).machine)
    router_report;
  export (Machine.spec Linear_list.machine) (list ());
  export
    (Machine.spec Linear_detail.machine)
    (detail ~props:(Check.props children Linear_detail.component) ());
  export (Machine.spec Linear_unsaved.machine) (unsaved ());
  export
    (Machine.spec Linear_create.machine)
    (create ~props:(Check.props children Linear_create.component) ());
  let create_report =
    create ~props:(Check.props children Linear_create.component) ()
  in
  let create_children =
    match create_report with
    | Check.Ok { children; _ } ->
        children
    | _ ->
        failwith "the check of create-issue did not pass"
  in
  export
    (Machine.spec Linear_create.Create_dialog.machine)
    (create_dialog
       ~props:(Check.props create_children Linear_create.Create_dialog.component)
       ()
    );
  export (Machine.spec Linear_me.machine) (me ());
  export (Machine.spec Linear_comments.machine) (comments ());
  export (Machine.spec Linear_composer.machine) (composer ());
  export (Machine.spec Linear_comment.machine) (comment ())
