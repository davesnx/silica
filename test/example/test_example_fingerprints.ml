open Silica

type entry = Entry : string * ('p, 's, 'e) Machine.t -> entry

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
  Alcotest.run "example fingerprints"
    [
      ( "fingerprints",
        [
          Alcotest.test_case "which machines of the example a view edit keeps"
            `Quick (fun () ->
              Alcotest.(check (list (pair string string)))
                "status"
                [
                  ("home", "kept");
                  ("about", "kept");
                  ("edit", "kept");
                  ("item", "kept");
                  ("not_found", "kept");
                  ("todo", "kept");
                  ("votes", "kept");
                  ("vote", "no fingerprint");
                  ("counter", "no fingerprint");
                ]
                (List.map status
                   [
                     Entry ("home", Example_home.machine);
                     Entry ("about", Example_about.machine);
                     Entry ("edit", Example_edit.machine);
                     Entry ("item", Example_item.machine);
                     Entry ("not_found", Example_not_found.machine);
                     Entry ("todo", Example_todo.machine);
                     Entry ("votes", Example_votes.machine);
                     Entry ("vote", Example_vote_button.machine);
                     Entry ("counter", Example_counter.machine);
                   ]
                )
          );
        ]
      );
    ]
