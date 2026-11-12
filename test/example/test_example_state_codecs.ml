open Silica

type entry = Entry : string * ('p, 's, 'e) Machine.t -> entry

let status (Entry (name, machine)) =
  ( name,
    match Machine.state_codec machine with
    | Ok _ ->
        "kept"
    | Error _ ->
        "no state codec"
  )

let () =
  Example_codecs.register ();
  Alcotest.run "example state codecs"
    [
      ( "state codecs",
        [
          Alcotest.test_case
            "which machines of the example keep their state at a hot reload"
            `Quick (fun () ->
              Alcotest.(check (list (pair string string)))
                "status"
                [
                  ("about", "kept");
                  ("item", "kept");
                  ("todo", "kept");
                  ("votes", "kept");
                  ("vote", "kept");
                  ("counter", "kept");
                  ("home", "no state codec");
                  ("not_found", "no state codec");
                  ("edit", "no state codec");
                ]
                (List.map status
                   [
                     Entry ("about", Example_about.machine);
                     Entry ("item", Example_item.machine);
                     Entry ("todo", Example_todo.machine);
                     Entry ("votes", Example_votes.machine);
                     Entry ("vote", Example_vote_button.machine);
                     Entry ("counter", Example_counter.machine);
                     Entry ("home", Example_home.machine);
                     Entry ("not_found", Example_not_found.machine);
                     Entry ("edit", Example_edit.machine);
                   ]
                )
          );
        ]
      );
    ]
