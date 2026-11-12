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
  ]

let status (Entry (name, machine)) =
  ( name,
    match Machine.state_codec machine with
    | Ok _ ->
        "kept"
    | Error reason ->
        reason
  )

let () =
  Linear_codecs.register ();
  Alcotest.run "linear state codecs"
    [
      ( "state codecs",
        [
          Alcotest.test_case
            "every machine of mini Linear that holds data has a state codec, \
             so a hot reload keeps its state"
            `Quick (fun () ->
              Alcotest.(check (list (pair string string)))
                "status"
                (List.map (fun (Entry (name, _)) -> (name, "kept")) machines)
                (List.map status machines)
          );
        ]
      );
    ]
