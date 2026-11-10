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
  let open Example_checks in
  let router_report = router () in
  let pages = children router_report in
  export
    (Machine.spec (Example_app.app (Url.of_string "/")).machine)
    router_report;
  export (Machine.spec Example_home.machine) (home ());
  export (Machine.spec Example_todo.machine) (todo ());
  export
    (Machine.spec Example_item.machine)
    (item ~props:(Check.props pages Example_item.component) ());
  export (Machine.spec Example_about.machine) (about ());
  export
    (Machine.spec Example_not_found.machine)
    (not_found ~props:(Check.props pages Example_not_found.component) ());
  export (Machine.spec Example_votes.machine) (votes ());
  export (Machine.spec Example_vote_button.machine) (vote ());
  export (Machine.spec Example_counter.machine) (counter_bounded ())
