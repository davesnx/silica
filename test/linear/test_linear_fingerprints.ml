open Silica

type entry = Entry : string * ('p, 's, 'e) Machine.t -> entry

let machines =
  [
    Entry ("issue-list", Linear_list.machine);
    Entry ("issue-detail", Linear_detail.machine);
    Entry ("create-issue", Linear_create.machine);
    Entry ("comments", Linear_comments.machine);
    Entry ("comment", Linear_comment.machine);
    Entry ("comment-composer", Linear_composer.machine);
    Entry ("me", Linear_me.machine);
    Entry ("unsaved", Linear_unsaved.machine);
    Entry ("toast", Silica_ui.Toast.machine);
    Entry ("select", Silica_ui.Select.machine);
    Entry ("menu", Silica_ui.Menu.machine);
    Entry ("checkbox", Silica_ui.Checkbox.machine);
    Entry ("combobox", Silica_ui.Combobox.machine);
    Entry ("theme", Silica_ui.Theme.machine);
  ]

let status (Entry (name, machine)) =
  ( name,
    match Option.map Fingerprint.digest (Machine.fingerprint machine) with
    | Some (Ok _) ->
        "kept"
    | Some (Error reason) ->
        reason
    | None ->
        "no fingerprint"
  )

let () =
  Alcotest.run "linear fingerprints"
    [
      ( "fingerprints",
        [
          Alcotest.test_case
            "every machine of mini Linear has a fingerprint, so a view edit \
             keeps its state"
            `Quick (fun () ->
              Alcotest.(check (list (pair string string)))
                "status"
                (List.map (fun (Entry (name, _)) -> (name, "kept")) machines)
                (List.map status machines)
          );
        ]
      );
    ]
