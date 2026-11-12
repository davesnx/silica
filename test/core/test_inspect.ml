open Silica
module Json = Silica_json.Json

let check_string = Alcotest.(check string)
let check_strings = Alcotest.(check (list string))

let step_record : Transition.t =
  {
    seq = 2;
    time_ms = Some 1731300000456.5;
    instance = "app/0:saver";
    number = 2;
    entry = 1;
    machine = "saver";
    from = "idle";
    target = "saving";
    change = Entered;
    event = "save";
    data = Some (Json.declassify (`List [ Json.declassify (`String "Save") ]));
    props = None;
    snapshot =
      Some (Json.declassify (`List [ Json.declassify (`String "Saving") ]));
    actions = [ "start timer 2.1/saver/saving/after 2000ms 2000ms" ];
    cause =
      Element
        {
          handler = "press";
          role = Some "button";
          name = "Save";
          element = "button";
          value = None;
        };
  }

let timer : Inspect.resource =
  {
    resource = Machine.Timer { id = "2.1/saver/saving/after 2000ms"; ms = 2000 };
    since = Some 1731300000456.5;
  }

let instance : Inspect.instance =
  {
    path = "app/0:saver";
    machine = "saver";
    state = "saving";
    data = Some (Json.declassify (`List [ Json.declassify (`String "Saving") ]));
    props = None;
    owns = [ timer ];
  }

let filter : Inspect.filter =
  {
    machines = [ "saver" ];
    path = Some "app";
    events = [ "save" ];
    changes = [ Entered; Kept ];
  }

let goldens : (Inspect.message * string) list =
  [
    ( Hello_target { target = "tab-3f2a"; kind = Browser; url = Some "/issues" },
      {|{"t":"hello","role":"target","version":1,"target":"tab-3f2a","kind":"browser","url":"/issues"}|}
    );
    ( Hello_target { target = "server"; kind = Server; url = None },
      {|{"t":"hello","role":"target","version":1,"target":"server","kind":"server"}|}
    );
    ( Hello_client "silica-inspect",
      {|{"t":"hello","role":"client","version":1,"name":"silica-inspect"}|}
    );
    ( Event { id = Some 4; target = "tab-3f2a"; event = Tree [ instance ] },
      {|{"t":"tree","id":4,"target":"tab-3f2a","instances":[{"path":"app/0:saver","machine":"saver","state":"saving","data":["Saving"],"owns":[{"kind":"timer","id":"2.1/saver/saving/after 2000ms","ms":2000,"since":1731300000456.5}]}]}|}
    );
    ( Event { id = None; target = "tab-3f2a"; event = Mount instance },
      {|{"t":"mount","target":"tab-3f2a","path":"app/0:saver","machine":"saver","state":"saving","data":["Saving"],"owns":[{"kind":"timer","id":"2.1/saver/saving/after 2000ms","ms":2000,"since":1731300000456.5}]}|}
    );
    ( Event { id = None; target = "tab-3f2a"; event = Unmount "app/0:saver" },
      {|{"t":"unmount","target":"tab-3f2a","path":"app/0:saver"}|}
    );
    ( Event
        {
          id = None;
          target = "tab-3f2a";
          event = Step { transition = step_record; owns = Some [ timer ] };
        },
      {|{"t":"step","target":"tab-3f2a","seq":2,"time_ms":1731300000456.5,"instance":"app/0:saver","number":2,"entry":1,"machine":"saver","from":"idle","to":"saving","change":"entered","event":"save","data":["Save"],"snapshot":["Saving"],"actions":["start timer 2.1/saver/saving/after 2000ms 2000ms"],"cause":{"type":"element","handler":"press","role":"button","name":"Save","element":"button"},"owns":[{"kind":"timer","id":"2.1/saver/saving/after 2000ms","ms":2000,"since":1731300000456.5}]}|}
    );
    ( Event
        {
          id = None;
          target = "server";
          event =
            Live
              {
                direction = In;
                channel = "counter";
                message = {|{"t":"event","event":["Increment"]}|};
              };
        },
      {|{"t":"live","target":"server","dir":"in","channel":"counter","message":"{\"t\":\"event\",\"event\":[\"Increment\"]}"}|}
    );
    ( Event
        {
          id = None;
          target = "tab-3f2a";
          event =
            Resumed
              {
                kept = [ "app" ];
                restarted =
                  [
                    {
                      path = "app/0:saver";
                      machine = "saver";
                      reason = "its state type changed";
                    };
                  ];
              };
        },
      {|{"t":"resumed","target":"tab-3f2a","kept":["app"],"restarted":[{"path":"app/0:saver","machine":"saver","reason":"its state type changed"}]}|}
    );
    ( Event { id = None; target = "tab-3f2a"; event = Paused 42 },
      {|{"t":"paused","target":"tab-3f2a","at":42}|}
    );
    ( Subscribe { id = Some 1; target = Some "tab-3f2a"; filter },
      {|{"t":"subscribe","id":1,"target":"tab-3f2a","filter":{"machine":["saver"],"path":"app","event":["save"],"change":["entered","kept"]}}|}
    );
    ( Subscribe { id = None; target = None; filter = Inspect.every },
      {|{"t":"subscribe","filter":{}}|}
    );
    (Request { id = Some 2; request = Targets }, {|{"t":"targets","id":2}|});
    ( Request { id = Some 3; request = Get_tree (Some "tab-3f2a") },
      {|{"t":"tree","id":3,"target":"tab-3f2a"}|}
    );
    (Request { id = Some 5; request = Export None }, {|{"t":"export","id":5}|});
    ( Request { id = Some 6; request = Travel { target = "tab-3f2a"; seq = 42 } },
      {|{"t":"travel","id":6,"target":"tab-3f2a","seq":42}|}
    );
    ( Request { id = Some 7; request = Return_live "tab-3f2a" },
      {|{"t":"live","id":7,"target":"tab-3f2a"}|}
    );
    (Subscribed { id = Some 1 }, {|{"t":"subscribed","id":1}|});
    ( Target_list
        {
          id = None;
          targets =
            [
              { target = "tab-3f2a"; kind = Browser; url = Some "/" };
              { target = "server"; kind = Server; url = None };
            ];
        },
      {|{"t":"targets","targets":[{"target":"tab-3f2a","kind":"browser","url":"/"},{"target":"server","kind":"server"}]}|}
    );
    ( Trace_file { id = Some 5; target = "tab-3f2a"; trace = "{}\n" },
      {|{"t":"trace","id":5,"target":"tab-3f2a","trace":"{}\n"}|}
    );
    (Gone "tab-3f2a", {|{"t":"gone","target":"tab-3f2a"}|});
    ( Failure { id = Some 6; message = "time travel is not available yet" },
      {|{"t":"error","id":6,"message":"time travel is not available yet"}|}
    );
  ]

let test_goldens () =
  List.iter
    (fun (message, line) ->
      check_string line line (Inspect.to_string message);
      match Inspect.of_string line with
      | Ok decoded ->
          check_string ("round trip " ^ line) line (Inspect.to_string decoded);
          Alcotest.(check bool) ("equal " ^ line) true (decoded = message)
      | Error error ->
          Alcotest.fail (line ^ ": " ^ error)
    )
    goldens

let test_errors () =
  let error line =
    match Inspect.of_string line with
    | Ok _ ->
        Alcotest.fail ("read " ^ line)
    | Error message ->
        message
  in
  check_string "another version"
    "protocol version 2; this side speaks version 1"
    (error {|{"t":"hello","role":"client","version":2,"name":"x"}|});
  check_string "an unknown message" "unknown message \"jump\""
    (error {|{"t":"jump"}|});
  check_string "a missing field" "missing field \"target\""
    (error {|{"t":"gone"}|});
  check_string "a bad change"
    "field \"filter\": field \"change\": unknown change \"moved\""
    (error {|{"t":"subscribe","filter":{"change":["moved"]}}|})

let test_filters () =
  let step ?(machine = "saver") ?(instance = "app/0:saver") ?(event = "save")
      ?(change = Transition.Entered) () =
    Inspect.Step
      {
        transition = { step_record with machine; instance; event; change };
        owns = None;
      }
  in
  let passes = Inspect.passes filter in
  Alcotest.(check bool)
    "every passes" true
    (Inspect.passes Inspect.every (step ()));
  Alcotest.(check bool) "the step passes" true (passes (step ()));
  Alcotest.(check bool)
    "another machine" false
    (passes (step ~machine:"list" ()));
  Alcotest.(check bool) "the path itself" true (passes (step ~instance:"app" ()));
  Alcotest.(check bool)
    "a path that only starts the same" false
    (passes (step ~instance:"apple/0:saver" ()));
  Alcotest.(check bool) "another event" false (passes (step ~event:"done" ()));
  Alcotest.(check bool)
    "another change" false
    (passes (step ~change:Unhandled ()));
  Alcotest.(check bool)
    "a mount by machine and path" true (passes (Mount instance));
  Alcotest.(check bool)
    "a mount of another machine" false
    (passes (Mount { instance with machine = "list" }));
  Alcotest.(check bool) "an unmount by path" false (passes (Unmount "other"));
  Alcotest.(check bool)
    "a hot reload always" true
    (passes (Resumed { kept = []; restarted = [] }))

let test_text () =
  check_string "a step"
    {|[tab-3f2a] 2 app/0:saver idle -> saving (entered) on save <- press button "Save" (button) => start timer 2.1/saver/saving/after 2000ms 2000ms|}
    (Option.get
       (Inspect.to_text
          (Event
             {
               id = None;
               target = "tab-3f2a";
               event = Step { transition = step_record; owns = None };
             }
          )
       )
    );
  Alcotest.(check (option string))
    "a tree has no line" None
    (Inspect.to_text (Event { id = None; target = "t"; event = Tree [] }))

module Saver = struct
  type event = Save | Done
  type state = Idle | Saving

  let name = function Idle -> "idle" | Saving -> "saving"

  let states = [ "idle"; "saving" ]

  let event_name = function Save -> "save" | Done -> "done"

  let machine =
    Machine.make ~id:"saver" ~init:Idle ~name ~states ~event_name
      ~during:(function Idle -> [] | Saving -> [ Source.after 2000 Done ])
      (fun state event ->
        match (state, event) with
        | Idle, Save ->
            Machine.Go Saving
        | Saving, Done ->
            Machine.Go Idle
        | Idle, Done | Saving, Save ->
            Machine.Ignore
      )

  let state_codec =
    Codec.make
      ~encode:(function
        | Idle ->
            Codec.constructor "Idle" []
        | Saving ->
            Codec.constructor "Saving" []
        )
      ~decode:(fun json ->
        match Codec.constructor_of json with
        | Ok ("Idle", []) ->
            Ok Idle
        | Ok ("Saving", []) ->
            Ok Saving
        | _ ->
            Error "not a saver state"
      )

  let component =
    Component.make machine (fun _ ->
        Node.element "button" [ Attr.on_press Save ] [ Node.text "Save" ]
    )
end

let test_target () =
  Machine.register_codecs Saver.machine ~state:Saver.state_codec ();
  let clock = ref 100. in
  let sent = ref [] in
  let target =
    Inspect.Target.create ~target:"tab-1"
      ~clock:(fun () -> !clock)
      ~send:(fun event -> sent := event :: !sent)
      ()
  in
  let lines () =
    let all = List.rev !sent in
    sent := [];
    List.map
      (fun event ->
        Inspect.to_string (Event { id = None; target = "tab-1"; event })
      )
      all
  in
  let tree =
    Instance.mount ~observe:(Inspect.Target.observer target) Saver.component
  in
  ignore (Instance.update tree None);
  check_strings "a start is a mount, then a step"
    [
      {|{"t":"mount","target":"tab-1","path":"saver","machine":"saver","state":"idle","data":["Idle"],"owns":[]}|};
      {|{"t":"step","target":"tab-1","seq":1,"time_ms":100.0,"instance":"saver","number":1,"entry":0,"machine":"saver","from":"","to":"idle","change":"started","event":"","snapshot":["Idle"],"cause":{"type":"start"},"owns":[]}|};
    ]
    (lines ());
  clock := 250.;
  ignore (Instance.update tree (Some (Instance.message tree Saver.Save)));
  check_strings "a step with the timer it owns from now"
    [
      {|{"t":"step","target":"tab-1","seq":2,"time_ms":250.0,"instance":"saver","number":1,"entry":1,"machine":"saver","from":"idle","to":"saving","change":"entered","event":"save","snapshot":["Saving"],"actions":["start timer 1.1/saver/saving/after 2000ms 2000ms"],"cause":{"type":"message"},"owns":[{"kind":"timer","id":"1.1/saver/saving/after 2000ms","ms":2000,"since":250.0}]}|};
    ]
    (lines ());
  ignore (Instance.update tree (Some (Instance.message tree Saver.Save)));
  check_strings "an ignored event has no owns"
    [
      {|{"t":"step","target":"tab-1","seq":3,"time_ms":250.0,"instance":"saver","number":1,"entry":1,"machine":"saver","from":"saving","to":"saving","change":"unhandled","event":"save","snapshot":["Saving"],"cause":{"type":"message"}}|};
    ]
    (lines ());
  check_strings "the tree"
    [
      {|{"t":"tree","target":"tab-1","instances":[{"path":"saver","machine":"saver","state":"saving","data":["Saving"],"owns":[{"kind":"timer","id":"1.1/saver/saving/after 2000ms","ms":2000,"since":250.0}]}]}|};
    ]
    [
      Inspect.to_string
        (Event
           {
             id = None;
             target = "tab-1";
             event = Tree (Inspect.Target.tree target);
           }
        );
    ];
  ignore (Instance.unmount tree);
  check_strings "an end is a step, then an unmount"
    [
      {|{"t":"step","target":"tab-1","seq":4,"time_ms":250.0,"instance":"saver","number":1,"entry":1,"machine":"saver","from":"saving","to":"saving","change":"ended","event":"","snapshot":["Saving"],"actions":["stop 1.1/saver/saving/after 2000ms"],"cause":{"type":"ended"},"owns":[]}|};
      {|{"t":"unmount","target":"tab-1","path":"saver"}|};
    ]
    (lines ());
  check_strings "an empty tree" []
    (List.map
       (fun (instance : Inspect.instance) -> instance.path)
       (Inspect.Target.tree target)
    )

let test_resumed () =
  let sent = ref [] in
  let target =
    Inspect.Target.create ~target:"tab"
      ~clock:(fun () -> 0.)
      ~send:(fun event -> sent := event :: !sent)
      ()
  in
  let take : type p s e.
      (p, s, e) Instance.asked -> (s * e Action.host list) option =
   fun asked ->
    match
      Type.Id.provably_equal
        (Machine.type_id (Instance.asked_component asked).machine)
        (Machine.type_id Saver.machine)
    with
    | Some Type.Equal ->
        Some (Saver.Saving, [])
    | None ->
        None
  in
  let tree =
    Instance.mount
      ~observe:(Inspect.Target.observer target)
      ~resume:{ take } Saver.component
  in
  ignore (Instance.update tree None);
  match List.rev !sent with
  | [ Mount { state; _ }; Step { transition; _ } ] ->
      check_string "the state it resumed" "saving" state;
      check_string "the change" "resumed"
        (Transition.change_to_string transition.change)
  | _ ->
      Alcotest.fail "expected a mount and a step"

let test_island_paths () =
  let sent = ref [] in
  let target =
    Inspect.Target.create ~target:"server"
      ~clock:(fun () -> 0.)
      ~send:(fun event -> sent := event :: !sent)
      ()
  in
  let observer =
    Inspect.Target.observer ~path:(fun path -> path ^ "#a1b2") target
  in
  observer.observe
    (Instance.Detached
       {
         instance = "saver";
         machine = Saver.machine;
         props = ();
         entry = 0;
         from = None;
         snapshot = Saver.Idle;
         change = Started;
         event = None;
         actions = [];
         cause = Start;
       }
    );
  check_strings "the island path" [ "saver#a1b2" ]
    (List.map
       (fun (instance : Inspect.instance) -> instance.path)
       (Inspect.Target.tree target)
    );
  match !sent with
  | [ Step { transition; _ }; Mount _ ] ->
      check_string "the step path" "saver#a1b2" transition.instance
  | _ ->
      Alcotest.fail "expected a mount and a step"

let test_combine () =
  let seen = ref [] in
  let app =
    Transition.observer (fun pending ->
        seen := ("app", (Transition.resolve pending).seq) :: !seen
    )
  in
  let target =
    Inspect.Target.create ~target:"tab"
      ~clock:(fun () -> 0.)
      ~send:(function
        | Step { transition; _ } ->
            seen := ("inspector", transition.seq) :: !seen
        | _ ->
            ()
        )
      ()
  in
  let tree =
    Instance.mount
      ~observe:
        (Inspect.Target.combine (Inspect.Target.observer target) (Some app))
      Saver.component
  in
  ignore (Instance.update tree None);
  Alcotest.(check (list (pair string int)))
    "the app first, then the inspector"
    [ ("app", 1); ("inspector", 1) ]
    (List.rev !seen)

let tests =
  let test name run = Alcotest.test_case name `Quick run in
  [
    test "every message has its golden JSON and reads back" test_goldens;
    test "errors name the field and the version" test_errors;
    test "filters by machine, path, event and change" test_filters;
    test "the text line of a step" test_text;
    test "a target sends mount, step and unmount with what it owns" test_target;
    test "a target gives island paths" test_island_paths;
    test "a hot reload start is resumed" test_resumed;
    test "an app observer and the inspector both observe" test_combine;
  ]

let () = Alcotest.run "inspect" [ ("protocol", tests) ]
