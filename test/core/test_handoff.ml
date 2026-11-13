open Silica

let echo =
  Remote.declare ~path:"/api/echo" ~input:Codec.int ~output:Codec.int
    ~rejected:Codec.string

type event = Bump | Tick | Answer of int

let registered ?state machine =
  Option.iter (fun state -> Machine.register_codecs machine ~state ()) state;
  machine

let counter ?(id = "counter") ?state () =
  registered ?state
  @@ Machine.make ~id ~init:0
       ~name:(fun _ -> "counting")
       ~during:(fun count ->
         if count > 0 then
           [
             Host.after 1000 Tick;
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
       ~entry:(fun count ->
         [ Action.Set_title ("count " ^ string_of_int count) ]
       )
       (fun count -> function
         | Bump -> Machine.Go (count + 1) | Tick | Answer _ -> Machine.Ignore
         )

let as_object : int Codec.t =
  Codec.make
    ~encode:(fun count ->
      Silica_json.Json.declassify (`Assoc [ ("count", Codec.int.encode count) ])
    )
    ~decode:(Codec.field "count" Codec.int)

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
  let hook, outcome = Handoff.hook handoff in
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

let starts_with prefix text =
  String.length text >= String.length prefix
  && String.sub text 0 (String.length prefix) = prefix

let check_reason path prefix outcome =
  match reasons outcome with
  | [ (restarted, reason) ] ->
      Alcotest.(check string) "path" path restarted;
      Alcotest.(check bool)
        (reason ^ " starts with " ^ prefix)
        true
        (starts_with prefix reason)
  | other ->
      Alcotest.failf "one restart expected, got %d" (List.length other)

let given_codec (known : (unit, int, event) Machine.t) : Handoff.codecs =
  let state (type p s e) (machine : (p, s, e) Machine.t) :
      (s Codec.t, string) result =
    match
      Type.Id.provably_equal (Machine.type_id machine) (Machine.type_id known)
    with
    | Some Type.Equal ->
        Ok Codec.int
    | None ->
        Machine.state_codec machine
  in
  { state }

let no_codec = "the machine \"counter\" has no registered codecs"

let resume_tests =
  let open Alcotest in
  [
    test_case
      "a state codec in both builds keeps the snapshot and starts its timer \
       and call again, with no entry"
      `Quick (fun () ->
        let before = bumped (counter ~state:Codec.int ()) 3 in
        let tree, actions, outcome =
          resume (counter ~state:Codec.int ()) (handoff_of before)
        in
        check int "snapshot" 3 (Instance.snapshot tree);
        check (list string) "starts only" [ "timer 1000"; "call" ]
          (kinds actions);
        check (list string) "kept" [ "counter" ] outcome.kept;
        check (list restart) "restarted" [] (reasons outcome)
    );
    test_case "a resumed tree steps on from the snapshot" `Quick (fun () ->
        let before = bumped (counter ~state:Codec.int ()) 2 in
        let tree, _, _ =
          resume (counter ~state:Codec.int ()) (handoff_of before)
        in
        press tree;
        check int "snapshot" 3 (Instance.snapshot tree)
    );
    test_case
      "a snapshot that the new state codec does not decode starts again, with \
       the decode error"
      `Quick (fun () ->
        let before = bumped (counter ~state:Codec.int ()) 3 in
        let tree, actions, outcome =
          resume (counter ~state:as_object ()) (handoff_of before)
        in
        check int "fresh" 0 (Instance.snapshot tree);
        check (list string) "entry" [ "title count 0" ] (kinds actions);
        check (list string) "kept" [] outcome.kept;
        check_reason "counter" "its snapshot does not decode: " outcome
    );
    test_case "a machine with no state codec in the old code starts again"
      `Quick (fun () ->
        let before = bumped (counter ()) 1 in
        let tree, _, outcome =
          resume (counter ~state:Codec.int ()) (handoff_of before)
        in
        check int "fresh" 0 (Instance.snapshot tree);
        check_reason "counter" no_codec outcome
    );
    test_case "a machine with no state codec in the new code starts again"
      `Quick (fun () ->
        let before = bumped (counter ~state:Codec.int ()) 1 in
        let tree, _, outcome = resume (counter ()) (handoff_of before) in
        check int "fresh" 0 (Instance.snapshot tree);
        check_reason "counter" no_codec outcome
    );
    test_case
      "a machine with no state codec at its start snapshot is kept there, with \
       no entry"
      `Quick (fun () ->
        let before = bumped (counter ()) 0 in
        let tree, actions, outcome = resume (counter ()) (handoff_of before) in
        check int "snapshot" 0 (Instance.snapshot tree);
        check (list string) "no entry" [] (kinds actions);
        check (list string) "kept" [ "counter" ] outcome.kept
    );
    test_case "another machine at the path starts again" `Quick (fun () ->
        let before = bumped (counter ~state:Codec.int ()) 1 in
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
        let tree, _, outcome = resume (counter ~state:Codec.int ()) moved in
        check int "fresh" 0 (Instance.snapshot tree);
        check (list restart) "reason"
          [ ("counter", "another machine (other) ran here before") ]
          (reasons outcome)
    );
    test_case "codecs given to the hook keep a machine that has none registered"
      `Quick (fun () ->
        let machine = counter () in
        let codecs = given_codec machine in
        let before = bumped machine 3 in
        let handoff =
          Handoff.of_running ~codecs ~islands:[] ~url:(Url.of_string "/")
            (Instance.running before)
        in
        let hook, outcome = Handoff.hook ~codecs handoff in
        let tree = Instance.mount ~resume:hook (Component.make machine view) in
        ignore (Instance.update tree None);
        check int "snapshot" 3 (Instance.snapshot tree);
        check (list string) "kept" [ "counter" ] (outcome ()).kept
    );
    test_case "the hook is used by the first update only" `Quick (fun () ->
        let machine = counter ~state:Codec.int () in
        let hook, _ = Handoff.hook (handoff_of (bumped machine 2)) in
        let tree = Instance.mount ~resume:hook (Component.make machine view) in
        ignore (Instance.update tree None);
        check int "resumed" 2 (Instance.snapshot tree);
        let again = Instance.mount (Component.make machine view) in
        ignore (Instance.update again None);
        check int "a later tree starts fresh" 0 (Instance.snapshot again)
    );
  ]

type child_event = Child_bump

let child_machine () =
  registered ~state:Codec.int
  @@ Machine.make ~id:"child" ~init:0
       ~name:(fun _ -> "child")
       (fun count Child_bump -> Machine.Go (count + 1))

let child_view count : child_event Node.t =
  Node.element "button"
    [ Attr.on_press Child_bump ]
    [ Node.text (string_of_int count) ]

let parent_machine state =
  registered ~state
  @@ Machine.make ~id:"parent" ~init:0
       ~name:(fun _ -> "parent")
       (fun count () -> Machine.Go (count + 1))

let tree_tests =
  let open Alcotest in
  let child = Component.make (child_machine ()) child_view in
  let parent ~wrapped state =
    Component.make (parent_machine state) (fun _ : unit Node.t ->
        if wrapped then
          Node.element "div" [] [ Node.text "new"; Component.node child ]
        else
          Node.element "div" [] [ Component.node child ]
    )
  in
  [
    test_case "the child of a parent that starts again keeps its state" `Quick
      (fun () ->
        let before = Instance.mount (parent ~wrapped:false Codec.int) in
        press before;
        press before;
        let hook, outcome = Handoff.hook (handoff_of before) in
        let tree =
          Instance.mount ~resume:hook (parent ~wrapped:false as_object)
        in
        ignore (Instance.update tree None);
        let result = outcome () in
        check (list string) "kept" [ "parent/0:child" ] result.kept;
        check_reason "parent" "its snapshot does not decode: " result;
        let running =
          List.map
            (fun (Instance.Running { path; _ }) -> path)
            (Instance.running tree)
        in
        check (list string) "paths" [ "parent"; "parent/0:child" ] running
    );
    test_case "a child that moved in the view keeps its state" `Quick (fun () ->
        let before = Instance.mount (parent ~wrapped:false Codec.int) in
        press before;
        let hook, outcome = Handoff.hook (handoff_of before) in
        let tree =
          Instance.mount ~resume:hook (parent ~wrapped:true Codec.int)
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
  let other = Component.make (child_machine ()) child_view in
  let page counter =
    Component.make (parent_machine Codec.int) (fun _ : unit Node.t ->
        Node.element "div" [] [ Component.node counter; Component.node other ]
    )
  in
  let breaks_above limit =
    Component.make (counter ~state:Codec.int ()) (fun count ->
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
        let hook, outcome = Handoff.hook (handoff_of (before ())) in
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
        let hook, _ = Handoff.hook (handoff_of (before ())) in
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
