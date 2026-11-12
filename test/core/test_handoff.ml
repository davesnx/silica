open Silica

let echo =
  Remote.declare ~path:"/api/echo" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

let rec holds_function value =
  if Obj.is_int value then
    false
  else
    let tag = Obj.tag value in
    if tag = Obj.closure_tag || tag = Obj.infix_tag then
      true
    else if tag < Obj.no_scan_tag then
      List.exists
        (fun index -> holds_function (Obj.field value index))
        (List.init (Obj.size value) Fun.id)
    else
      false

type event = Bump | Tick | Answer of int

let counter ?(id = "counter") ?fingerprint () =
  Machine.make ~id ~init:0
    ~name:(fun _ -> "counting")
    ?fingerprint
    ~during:(fun count ->
      if count > 0 then
        [
          Source.after 1000 Tick;
          Remote.call echo count (function
            | Remote.Ok value ->
                Answer value
            | Remote.Rejected _ | Remote.Failed _ ->
                Answer (-1)
            );
        ]
      else
        []
    )
    ~entry:(fun count -> [ Action.Set_title ("count " ^ string_of_int count) ])
    (fun count -> function
      | Bump -> Machine.Go (count + 1) | Tick | Answer _ -> Machine.Ignore
      )

let int_fingerprint : int Fingerprint.t = Fingerprint.declared "int" []

let view count : event Node.t =
  Node.element "button"
    [ Attr.on_press Bump ]
    [ Node.text (string_of_int count) ]

let press tree =
  let node, _ = Instance.update tree None in
  let rec find node =
    match Node.view node with
    | Node.Element (_, attrs, children) -> (
        match
          List.find_map
            (function[@warning "-4"]
              | Node.On_press message -> Some message | _ -> None
              )
            attrs
        with
        | Some message ->
            Some message
        | None ->
            List.find_map find children
      )
    | Node.List children ->
        List.find_map find children
    | _ ->
        None
  in
  match find node with
  | Some message ->
      ignore (Instance.update tree (Some message))
  | None ->
      Alcotest.fail "no button"

let handoff_of tree =
  Handoff.of_running ~islands:[] ~url:(Url.of_string "/") (Instance.running tree)

let resume machine handoff =
  let hook, outcome = Handoff.hook ~holds_function handoff in
  let tree = Instance.mount ~resume:hook (Component.make machine view) in
  let _, actions = Instance.update tree None in
  (tree, actions, outcome ())

let kinds actions =
  List.map
    (function[@warning "-4"]
      | Action.Start_timer { ms; _ } ->
          "timer " ^ string_of_int ms
      | Action.Start_call _ ->
          "call"
      | Action.Run (Action.Set_title title) ->
          "title " ^ title
      | _ ->
          "other"
      )
    actions

let reasons (outcome : Handoff.outcome) =
  List.map
    (fun (restarted : Handoff.restarted) ->
      (restarted.path, Handoff.reason_text restarted.reason)
    )
    outcome.restarted

let bumped machine times =
  let tree = Instance.mount (Component.make machine view) in
  for _ = 1 to times do
    press tree
  done;
  tree

let restart = Alcotest.(pair string string)

let resume_tests =
  let open Alcotest in
  [
    test_case
      "an equal fingerprint keeps the snapshot and starts its timer and call \
       again, with no entry"
      `Quick (fun () ->
        let machine = counter ~fingerprint:int_fingerprint () in
        let before = bumped machine 3 in
        let tree, actions, outcome =
          resume (counter ~fingerprint:int_fingerprint ()) (handoff_of before)
        in
        check int "snapshot" 3 (Instance.snapshot tree);
        check (list string) "starts only" [ "timer 1000"; "call" ]
          (kinds actions);
        check (list string) "kept" [ "counter" ] outcome.kept;
        check (list restart) "restarted" [] (reasons outcome)
    );
    test_case "a resumed tree steps on from the snapshot" `Quick (fun () ->
        let machine = counter ~fingerprint:int_fingerprint () in
        let tree, _, _ = resume machine (handoff_of (bumped machine 2)) in
        press tree;
        check int "snapshot" 3 (Instance.snapshot tree)
    );
    test_case "another fingerprint starts again" `Quick (fun () ->
        let before = bumped (counter ~fingerprint:int_fingerprint ()) 3 in
        let tree, actions, outcome =
          resume
            (counter ~fingerprint:(Fingerprint.declared "int2" []) ())
            (handoff_of before)
        in
        check int "fresh" 0 (Instance.snapshot tree);
        check (list string) "entry" [ "title count 0" ] (kinds actions);
        check (list restart) "reason"
          [ ("counter", "its state type changed") ]
          (reasons outcome)
    );
    test_case "a shape with no fingerprint starts again" `Quick (fun () ->
        let before = bumped (counter ~fingerprint:int_fingerprint ()) 1 in
        let tree, _, outcome = resume (counter ()) (handoff_of before) in
        check int "fresh" 0 (Instance.snapshot tree);
        check (list restart) "reason"
          [ ("counter", "its shape has no fingerprint") ]
          (reasons outcome)
    );
    test_case
      "a machine with no fingerprint whose fresh snapshot equals the old one \
       is kept, with no entry"
      `Quick (fun () ->
        let before = bumped (counter ()) 0 in
        let tree, actions, outcome = resume (counter ()) (handoff_of before) in
        check int "snapshot" 0 (Instance.snapshot tree);
        check (list string) "no entry" [] (kinds actions);
        check (list string) "kept" [ "counter" ] outcome.kept
    );
    test_case "an old snapshot with no fingerprint is not taken" `Quick
      (fun () ->
        let before = bumped (counter ()) 1 in
        let tree, _, outcome =
          resume (counter ~fingerprint:int_fingerprint ()) (handoff_of before)
        in
        check int "fresh" 0 (Instance.snapshot tree);
        check (list restart) "reason"
          [ ("counter", "its state type changed") ]
          (reasons outcome)
    );
    test_case "a fingerprint with a reason gives that reason" `Quick (fun () ->
        let refused = Fingerprint.unknown "Issue.t" in
        let before = bumped (counter ~fingerprint:refused ()) 1 in
        let _, _, outcome =
          resume (counter ~fingerprint:refused ()) (handoff_of before)
        in
        check (list restart) "reason"
          [
            ( "counter",
              "its state type mentions Issue.t, which has no fingerprint"
            );
          ]
          (reasons outcome)
    );
    test_case "another machine at the path starts again" `Quick (fun () ->
        let before = bumped (counter ~fingerprint:int_fingerprint ()) 1 in
        let handoff = handoff_of before in
        let moved =
          {
            handoff with
            instances =
              List.map
                (fun (path, (entry : Handoff.entry)) ->
                  (path, { entry with machine = "other" })
                )
                handoff.instances;
          }
        in
        let tree, _, outcome =
          resume (counter ~fingerprint:int_fingerprint ()) moved
        in
        check int "fresh" 0 (Instance.snapshot tree);
        check (list restart) "reason"
          [ ("counter", "another machine (other) ran here before") ]
          (reasons outcome)
    );
    test_case "a snapshot that holds a function starts again" `Quick (fun () ->
        let fingerprint : (int -> int) Fingerprint.t =
          Fingerprint.declared "function" []
        in
        let machine =
          Machine.make ~id:"closure" ~init:succ
            ~name:(fun _ -> "holding")
            ~fingerprint
            (fun _ () -> Machine.Ignore)
        in
        let component =
          Component.make machine (fun _ : unit Node.t -> Node.text "f")
        in
        let before = Instance.mount component in
        ignore (Instance.update before None);
        let hook, outcome = Handoff.hook ~holds_function (handoff_of before) in
        let tree = Instance.mount ~resume:hook component in
        ignore (Instance.update tree None);
        check (list restart) "reason"
          [ ("closure", "its state holds a function") ]
          (reasons (outcome ()))
    );
    test_case "the hook is used by the first update only" `Quick (fun () ->
        let machine = counter ~fingerprint:int_fingerprint () in
        let hook, _ =
          Handoff.hook ~holds_function (handoff_of (bumped machine 2))
        in
        let tree = Instance.mount ~resume:hook (Component.make machine view) in
        ignore (Instance.update tree None);
        check int "resumed" 2 (Instance.snapshot tree);
        let again = Instance.mount (Component.make machine view) in
        ignore (Instance.update again None);
        check int "a later tree starts fresh" 0 (Instance.snapshot again)
    );
  ]

type child_event = Child_bump

let child_machine fingerprint =
  Machine.make ~id:"child" ~init:0
    ~name:(fun _ -> "child")
    ~fingerprint
    (fun count Child_bump -> Machine.Go (count + 1))

let child_view count : child_event Node.t =
  Node.element "button"
    [ Attr.on_press Child_bump ]
    [ Node.text (string_of_int count) ]

let parent_machine ?fingerprint () =
  Machine.make ~id:"parent" ~init:0
    ~name:(fun _ -> "parent")
    ?fingerprint
    (fun count () -> Machine.Go (count + 1))

let tree_tests =
  let open Alcotest in
  let child = Component.make (child_machine int_fingerprint) child_view in
  let parent ~wrapped fingerprint =
    Component.make (parent_machine ?fingerprint ()) (fun _ : unit Node.t ->
        if wrapped then
          Node.element "div" [] [ Node.text "new"; Component.node child ]
        else
          Node.element "div" [] [ Component.node child ]
    )
  in
  [
    test_case "the child of a parent that starts again keeps its state" `Quick
      (fun () ->
        let before =
          Instance.mount
            (parent ~wrapped:false (Some (Fingerprint.declared "old" [])))
        in
        press before;
        press before;
        let hook, outcome = Handoff.hook ~holds_function (handoff_of before) in
        let tree =
          Instance.mount ~resume:hook
            (parent ~wrapped:false (Some (Fingerprint.declared "new" [])))
        in
        ignore (Instance.update tree None);
        let result = outcome () in
        check (list string) "kept" [ "parent/0:child" ] result.kept;
        check (list restart) "restarted"
          [ ("parent", "its state type changed") ]
          (reasons result);
        let running =
          List.map
            (fun (Instance.Running { path; _ }) -> path)
            (Instance.running tree)
        in
        check (list string) "paths" [ "parent"; "parent/0:child" ] running
    );
    test_case "a child that moved in the view keeps its state" `Quick (fun () ->
        let before =
          Instance.mount (parent ~wrapped:false (Some int_fingerprint))
        in
        press before;
        let hook, outcome = Handoff.hook ~holds_function (handoff_of before) in
        let tree =
          Instance.mount ~resume:hook
            (parent ~wrapped:true (Some int_fingerprint))
        in
        let node, _ = Instance.update tree None in
        check (list string) "kept"
          [ "parent"; "parent/1:child" ]
          (outcome ()).kept;
        check string "view" "<div>new<button>1</button></div>"
          (Render.to_string node)
    );
  ]

exception New_view_broke

let guard_tests =
  let open Alcotest in
  let other = Component.make (child_machine int_fingerprint) child_view in
  let page counter =
    Component.make (parent_machine ~fingerprint:int_fingerprint ())
      (fun _ : unit Node.t ->
        Node.element "div" [] [ Component.node counter; Component.node other ]
    )
  in
  let breaks_above limit =
    Component.make (counter ~fingerprint:int_fingerprint ()) (fun count ->
        if count > limit then
          raise New_view_broke
        else
          view count
    )
  in
  let before () =
    let tree = Instance.mount (page (breaks_above 100)) in
    press tree;
    press tree;
    tree
  in
  [
    test_case
      "a view that raises on a resumed snapshot restarts that instance only"
      `Quick (fun () ->
        let hook, outcome =
          Handoff.hook ~holds_function (handoff_of (before ()))
        in
        let tree = Instance.mount ~resume:hook (page (breaks_above 1)) in
        let node, _ = Instance.update tree None in
        let result = outcome () in
        check (list restart) "restarted"
          [
            ( "parent/0:counter",
              "its view raised after the reload \
               (Dune__exe__Test_handoff.New_view_broke)"
            );
          ]
          (reasons result);
        check (list string) "kept" [ "parent"; "parent/1:child" ] result.kept;
        check string "the page shows both"
          "<div><button>0</button><button>0</button></div>"
          (Render.to_string node)
    );
    test_case "a view that raises again after the restart raises" `Quick
      (fun () ->
        let hook, _ = Handoff.hook ~holds_function (handoff_of (before ())) in
        let tree = Instance.mount ~resume:hook (page (breaks_above (-1))) in
        check bool "raises" true
          ( match Instance.update tree None with
          | _ ->
              false
          | exception New_view_broke ->
              true
          )
    );
  ]

let () =
  Alcotest.run "handoff"
    [ ("resume", resume_tests); ("tree", tree_tests); ("guard", guard_tests) ]
