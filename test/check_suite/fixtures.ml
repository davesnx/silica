open Silica

let button label event =
  Node.element "button" [ Attr.on_press event ] [ Node.text label ]

type door_event = Open | Close | Lock | Unlock [@@deriving event_shape]
type door_state = Closed | Opened | Locked [@@deriving state_shape]

let door ~unlock_to =
  let machine =
    Machine.make ~id:"door" ~init:Closed ~shape:door_state_shape
      ~events:door_event_shape (fun state event ->
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

type save_event = Edit | Save [@@deriving event_shape]
type save_state = Editing of int | Saved [@@deriving state_shape]

let make_save ~handled ~offered =
  let machine =
    Machine.make ~id:"save" ~init:(Editing 0) ~shape:save_state_shape
      ~events:save_event_shape (fun state event ->
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

type count_event = Increment [@@deriving event_shape]
type count_state = Counting of int [@@deriving state_shape]

let counter =
  Component.make
    (Machine.make ~id:"counter" ~init:(Counting 0) ~shape:count_state_shape
       ~events:count_event_shape (fun (Counting count) Increment ->
         Machine.Go (Counting (count + 1))
     )
    )
    (fun (Counting count) ->
      Node.element "div" []
        [
          Node.element "p" [] [ Node.text (string_of_int count) ];
          button "Add" Increment;
        ]
    )

type shell_event = Noop [@@deriving event_shape]
type shell_state = Showing [@@deriving state_shape]

let shell =
  Component.make
    (Machine.make ~id:"shell" ~init:Showing ~shape:shell_state_shape
       ~events:shell_event_shape (fun Showing Noop -> Machine.Stay []
     )
    )
    (fun Showing ->
      Node.element "main" []
        [ Node.component (door ~unlock_to:Closed); button "Wait" Noop ]
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
