open Silica

let button label event =
  Node.element "button" [ Attr.on_press event ] [ Node.text label ]

type door_event = Open | Close | Lock | Unlock

let door_event_event_name = function
  | Open ->
      "open"
  | Close ->
      "close"
  | Lock ->
      "lock"
  | Unlock ->
      "unlock"

type door_state = Closed | Opened | Locked

let door_state_name = function
  | Closed ->
      "closed"
  | Opened ->
      "opened"
  | Locked ->
      "locked"

let door_state_states = [ "closed"; "opened"; "locked" ]

let door ~unlock_to =
  let machine =
    Machine.make ~id:"door" ~init:Closed ~name:door_state_name
      ~states:door_state_states ~event_name:door_event_event_name
      (fun state event ->
        match (state, event) with
        | Closed, Open ->
            Machine.Go Opened
        | Closed, Lock ->
            Machine.Go Locked
        | Opened, Close ->
            Machine.Go Closed
        | Locked, Unlock ->
            Machine.Go unlock_to
        | _ ->
            Machine.Ignore
    )
  in
  Component.make machine (function
    | Closed ->
        Node.element "div" [] [ button "Open" Open; button "Lock" Lock ]
    | Opened ->
        button "Close" Close
    | Locked ->
        button "Unlock" Unlock
    )

type save_event = Edit | Save

let save_event_event_name = function Edit -> "edit" | Save -> "save"

type save_state = Editing of int | Saved

let save_state_name = function Editing _ -> "editing" | Saved -> "saved"

let save_state_states = [ "editing"; "saved" ]

let make_save ~handled ~offered =
  let machine =
    Machine.make ~id:"save" ~init:(Editing 0) ~name:save_state_name
      ~states:save_state_states ~event_name:save_event_event_name
      (fun state event ->
        match (state, event) with
        | Editing count, Edit ->
            Machine.Go (Editing ((count + 1) mod 2))
        | Editing _, Save ->
            Machine.Go Saved
        | Saved, Save when handled ->
            Machine.Go (Editing 0)
        | _ ->
            Machine.Ignore
    )
  in
  Machine.register_codecs machine
    ~state:
      (Codec.make
         ~encode:(function
           | Editing count ->
               Codec.constructor "Editing" [ Codec.int.encode count ]
           | Saved ->
               Codec.constructor "Saved" []
           )
         ~decode:(fun json ->
           match Codec.constructor_of json with
           | Ok ("Editing", [ count ]) ->
               Result.map (fun count -> Editing count) (Codec.int.decode count)
           | Ok ("Saved", []) ->
               Ok Saved
           | _ ->
               Error "not a state of save"
         )
      )
    ~event:
      (Codec.make
         ~encode:(function
           | Edit ->
               Codec.constructor "Edit" []
           | Save ->
               Codec.constructor "Save" []
           )
         ~decode:(fun json ->
           match Codec.constructor_of json with
           | Ok ("Edit", []) ->
               Ok Edit
           | Ok ("Save", []) ->
               Ok Save
           | _ ->
               Error "not an event of save"
         )
      )
    ();
  Component.make machine (function
    | Editing count ->
        Node.element "form" []
          [
            Node.element "p" [] [ Node.text (string_of_int count ^ " edits") ];
            button "Edit" Edit;
            button "Save" Save;
          ]
    | Saved ->
        Node.element "div" []
          (Node.element "p" [] [ Node.text "Saved" ]
          ::
          ( if offered then
              [ button "Save" Save ]
            else
              []
          )
          )
    )

let save = make_save ~handled:false ~offered:true

type count_event = Increment

let count_event_event_name = function Increment -> "increment"

type count_state = Counting of int

let count_state_name = function Counting _ -> "counting"

let count_state_states = [ "counting" ]

let counter =
  Component.make
    (Machine.make ~id:"counter" ~init:(Counting 0) ~name:count_state_name
       ~states:count_state_states ~event_name:count_event_event_name
       (fun (Counting count) Increment -> Machine.Go (Counting (count + 1))
     )
    )
    (fun (Counting count) ->
      Node.element "div" []
        [
          Node.element "p" [] [ Node.text (string_of_int count) ];
          button "Add" Increment;
        ]
    )

type shell_event = Noop

let shell_event_event_name = function Noop -> "noop"

type shell_state = Showing

let shell_state_name = function Showing -> "showing"

let shell_state_states = [ "showing" ]

let shell =
  Component.make
    (Machine.make ~id:"shell" ~init:Showing ~name:shell_state_name
       ~states:shell_state_states ~event_name:shell_event_event_name
       (fun Showing Noop -> Machine.Stay []
     )
    )
    (fun Showing ->
      Node.element "main" []
        [ Node.component (door ~unlock_to:Closed); button "Wait" Noop ]
    )

type twice_event = Go | Wait

let twice_event_event_name = function Go -> "go" | Wait -> "wait"

type twice_state = First | Second

let twice_state_name = function First -> "first" | Second -> "second"

let twice_state_states = [ "first"; "second" ]

let twice =
  Component.make
    (Machine.make ~id:"twice" ~init:First ~name:twice_state_name
       ~states:twice_state_states ~event_name:twice_event_event_name
       (fun state event ->
         match (state, event) with
         | First, Go ->
             Machine.Go Second
         | _ ->
             Machine.Ignore
     )
    )
    (function
      | First ->
          Node.element "div" [] [ button "Go" Go; button "Wait" Wait ]
      | Second ->
          button "Wait" Wait
      )

type ping_event = Ping

let pinged =
  Component.make
    (Machine.make ~id:"pinged" ~init:false
       ~name:(fun _ -> "waiting")
       ~states:[ "waiting" ]
       ~event_name:(fun Ping -> "ping")
       (fun _ Ping -> Machine.Go true)
    )
    (fun _ -> Node.element "p" [] [ Node.text "Waiting" ])

type label_event = Relabel

let label =
  let machine =
    Machine.make_with_props ~id:"label" ~init:""
      ~name:(fun _ -> "shown")
      ~states:[ "shown" ]
      ~event_name:(fun Relabel -> "relabel")
      ~on_props:(fun _ props _ -> Machine.Go props)
      (fun _ _ Relabel -> Machine.Stay [])
  in
  Machine.register_codecs machine ~props:Codec.string ~state:Codec.string ();
  Component.make_with_props machine (fun _ shown ->
      Node.element "p" []
        [ Node.text ("shows " ^ shown); button "Again" Relabel ]
  )

type picker_event = Pick of string

let picker =
  let machine =
    Machine.make ~id:"picker" ~init:""
      ~name:(fun _ -> "picking")
      ~states:[ "picking" ]
      ~event_name:(fun (Pick _) -> "pick")
      (fun _ (Pick text) -> Machine.Go text)
  in
  Machine.register_codecs machine ~state:Codec.string
    ~event:
      (Codec.make
         ~encode:(fun (Pick text) -> Codec.string.encode text)
         ~decode:(fun json ->
           Result.map (fun text -> Pick text) (Codec.string.decode json)
         )
      )
    ();
  Component.make machine (fun picked ->
      Node.element "main" []
        [
          button "Pick a" (Pick "a");
          Component.node_with_props label ~props:picked;
        ]
  )

let picked =
  Check.component
    ~children:[ Check.child label ]
    ~properties:
      [
        Check.always ~name:"the label follows the pick"
          (Check.holds (fun picked view ->
               Check.view_shows view ("shows " ^ picked)
           )
          );
      ]
    picker

let label_stays_empty =
  Check.component
    ~children:[ Check.child label ]
    ~properties:
      [
        Check.always ~name:"the label stays empty"
          (Check.view (fun view -> not (Check.view_shows view "shows a")));
      ]
    picker

type note_event = Typed of string

let note =
  let machine =
    Machine.make ~id:"note" ~init:""
      ~name:(fun text ->
        if text = "" then
          "empty"
        else
          "written"
      )
      ~states:[ "empty"; "written" ]
      ~event_name:(fun (Typed _) -> "typed")
      (fun _ (Typed text) -> Machine.Go text)
  in
  Machine.register_codecs machine ~state:Codec.string
    ~event:
      (Codec.make
         ~encode:(fun (Typed text) -> Codec.string.encode text)
         ~decode:(fun json ->
           Result.map (fun text -> Typed text) (Codec.string.decode json)
         )
      )
    ();
  Component.make machine (fun text ->
      Node.element "input"
        [
          Attr.string "aria-label" "Note";
          Attr.string "value" text;
          Attr.on_input (fun text -> Typed text);
        ]
        []
  )

let broken =
  Check.app ~routes:[]
    ~parse:(fun _ -> Showing)
    ~print:(fun Showing -> Url.of_string "/")
    ~url_event:(fun _ -> Noop)
    (fun _ -> shell)

let sets =
  [
    ("pass", [ Check_suite.entry (Check.component (door ~unlock_to:Closed)) ]);
    ("changed", [ Check_suite.entry (Check.component (door ~unlock_to:Opened)) ]);
    ("bug", [ Check_suite.entry (Check.component save) ]);
    ("twice", [ Check_suite.entry (Check.component twice) ]);
    ( "pinged",
      [
        Check_suite.entry
          (Check.component
             ~inputs:(fun _ -> [ Ping ])
             ~properties:[ Check.always ~name:"never pinged" (Check.state not) ]
             pinged
          );
      ]
    );
    ("joint", [ Check_suite.entry picked ]);
    ("joint-bug", [ Check_suite.entry label_stays_empty ]);
    ("note", [ Check_suite.entry (Check.component ~texts:[ "a" ] note) ]);
    ( "fixed",
      [
        Check_suite.entry
          (Check.component (make_save ~handled:true ~offered:true));
      ]
    );
    ( "disabled",
      [
        Check_suite.entry
          (Check.component (make_save ~handled:false ~offered:false));
      ]
    );
    ("limit", [ Check_suite.entry (Check.component ~max_states:5 counter) ]);
    ("broken", [ Check_suite.entry broken ]);
    ( "limit-and-bug",
      [
        Check_suite.entry (Check.component ~max_states:5 counter);
        Check_suite.entry (Check.component save);
      ]
    );
    ( "broken-and-bug",
      [ Check_suite.entry broken; Check_suite.entry (Check.component save) ]
    );
    ("unchecked", [ Check_suite.entry (Check.component shell) ]);
    ( "dependents",
      [
        Check_suite.entry (Check.component shell) ~dependents:(fun _ ->
            [ Check_suite.entry (Check.component (door ~unlock_to:Closed)) ]
        );
      ]
    );
    ( "unchecked-covered",
      [
        Check_suite.entry (Check.component shell);
        Check_suite.entry (Check.component (door ~unlock_to:Closed));
      ]
    );
  ]

let memory_replay file =
  match Trace.of_jsonl (In_channel.with_open_bin file In_channel.input_all) with
  | Error message ->
      print_endline ("memory: " ^ message)
  | Ok trace -> (
      match Silica_memory.replay (Silica_memory.mount picker) trace with
      | Replayed ->
          print_endline "memory: replayed"
      | Different { seq; expected; got } ->
          Printf.printf "memory: different at %d: expected %s, got %s\n" seq
            expected got
      | Not_realisable { seq; reason } ->
          Printf.printf "memory: not realisable at %d: %s\n" seq reason
      | Not_found { seq; error } ->
          Printf.printf "memory: not found at %d: %s\n" seq error
    )

let () =
  match Sys.argv with
  | [| _; "memory"; file |] ->
      memory_replay file
  | _ -> (
      match
        Option.bind (Sys.getenv_opt "FIXTURE") (fun set ->
            List.assoc_opt set sets
        )
      with
      | Some entries ->
          Check_suite.main entries
      | None ->
          prerr_endline
            ("set FIXTURE to one of: " ^ String.concat ", " (List.map fst sets));
          exit 3
    )
