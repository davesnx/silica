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

let () =
  match
    Option.bind (Sys.getenv_opt "FIXTURE") (fun set -> List.assoc_opt set sets)
  with
  | Some entries ->
      Check_suite.main entries
  | None ->
      prerr_endline
        ("set FIXTURE to one of: " ^ String.concat ", " (List.map fst sets));
      exit 3
