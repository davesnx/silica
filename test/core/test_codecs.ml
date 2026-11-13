open Silica

type event = Save | Answer of (string, string) Remote.answer | Reset

let event_name = function
  | Save ->
      "save"
  | Answer _ ->
      "answer"
  | Reset ->
      "reset"

type state = Idle | Saving | Saved of string

let name = function Idle -> "idle" | Saving -> "saving" | Saved _ -> "saved"

let states = [ "idle"; "saving"; "saved" ]

let state_codec =
  Codec.make
    ~encode:(fun state ->
      Codec.string.encode
        ( match state with
        | Idle ->
            "idle"
        | Saving ->
            "saving"
        | Saved time ->
            "saved " ^ time
        )
    )
    ~decode:(fun json ->
      Result.bind (Codec.string.decode json) (function
        | "idle" ->
            Ok Idle
        | "saving" ->
            Ok Saving
        | text when String.starts_with ~prefix:"saved " text ->
            Ok (Saved (String.sub text 6 (String.length text - 6)))
        | text ->
            Error ("unknown state " ^ text)
        )
    )

let update state event : (state, event) Machine.next =
  match (state, event) with
  | Idle, Save ->
      Go Saving
  | Saving, Answer (Ok time) ->
      Go (Saved time)
  | _, Reset ->
      Go Idle
  | _ ->
      Ignore

let text codec value = Codec.to_string codec value

let round_trip (type a) name (codec : a Codec.t) (value : a) =
  Alcotest.(check bool)
    (name ^ " round trips") true
    (Codec.of_string codec (Codec.to_string codec value) = Ok value)

let test_event_names () =
  let machine =
    Machine.make ~id:"save" ~init:Idle ~name ~states ~event_name update
  in
  Alcotest.(check (list (option string)))
    "names by event_name"
    [ Some "save"; Some "answer"; Some "reset" ]
    (List.map (Machine.event_name machine) [ Save; Answer (Ok "noon"); Reset ]);
  let unnamed = Machine.make ~id:"save" ~init:Idle ~name ~states update in
  Alcotest.(check (option string))
    "no event_name, no name" None
    (Machine.event_name unnamed Save)

let test_registered_codecs () =
  let event_codec =
    Codec.make
      ~encode:(fun event -> Codec.string.encode (event_name event))
      ~decode:(fun _ -> Error "not read")
  in
  let machine =
    Machine.make ~id:"save" ~init:Idle ~name ~states ~event_name update
  and twin =
    Machine.make ~id:"save" ~init:Idle ~name ~states ~event_name update
  in
  let missing =
    {|the machine "save" has no registered codecs: call Machine.register_codecs on it in the codec module of the app (Codecs.register in app/shared/codecs.ml of an app that silica new made), and call it before asking for a codec, such as at the start of check/main.ml|}
  in
  Alcotest.(check (result unit string))
    "an unregistered machine says which and how" (Error missing)
    (Result.map ignore (Machine.state_codec machine));
  Machine.register_codecs machine ~state:state_codec ~event:event_codec ();
  ( match Machine.state_codec machine with
  | Ok codec ->
      Alcotest.(check string)
        "state" {|"saved noon"|}
        (text codec (Saved "noon"))
  | Error message ->
      Alcotest.fail message
  );
  ( match Machine.event_codec machine with
  | Ok codec ->
      Alcotest.(check string) "event" {|"reset"|} (text codec Reset)
  | Error message ->
      Alcotest.fail message
  );
  Alcotest.(check bool)
    "the ids of the registered machines" true
    (List.mem "save" (Machine.registered_codecs ()));
  Alcotest.(check bool)
    "another machine with the same id and types has none" true
    (Result.is_error (Machine.state_codec twin)
    && Result.is_error (Machine.event_codec twin)
    );
  Machine.register_codecs machine ~state:state_codec ();
  Alcotest.(check bool)
    "a second registration replaces the first" true
    (Result.is_ok (Machine.state_codec machine));
  Alcotest.(check (result unit string))
    "a codec left out says which"
    (Error
       {|the machine "save" is registered with no event codec: give ~event to Machine.register_codecs|}
    )
    (Result.map ignore (Machine.event_codec machine))

let answer = Remote.answer_codec Codec.int Codec.string

let test_answer_codec () =
  Alcotest.(check (list string))
    "constructors as [@@deriving json] writes them"
    [
      {|["Ok",3]|};
      {|["Rejected","no"]|};
      {|["Failed",["Http",500]]|};
      {|["Failed",["Decode","bad"]]|};
      {|["Failed",["Network","down"]]|};
    ]
    (List.map (text answer)
       [
         Ok 3;
         Rejected "no";
         Failed (Http 500);
         Failed (Decode "bad");
         Failed (Network "down");
       ]
    );
  List.iter
    (round_trip "answer" answer)
    [ Ok 3; Rejected "no"; Failed (Http 500); Failed (Network "down") ];
  Alcotest.(check bool)
    "an unknown constructor is an error" true
    (Result.is_error (Codec.of_string answer {|["Maybe",1]|}));
  Alcotest.(check bool)
    "a wrong argument is an error" true
    (Result.is_error (Codec.of_string answer {|["Ok","three"]|}))

let test_status_and_update_codecs () =
  let status = Status.codec Codec.int in
  Alcotest.(check (list string))
    "status"
    [
      {|["Idle"]|};
      {|["Loading",null]|};
      {|["Ready",{"data":4,"stale":true}]|};
      {|["Failed",{"error":["Network","down"],"last":2}]|};
    ]
    (List.map (text status)
       [
         Status.Idle;
         Loading None;
         Ready { data = 4; stale = true };
         Failed { error = Network "down"; last = Some 2 };
       ]
    );
  List.iter
    (round_trip "status" status)
    [
      Status.Idle;
      Loading (Some 1);
      Ready { data = 4; stale = false };
      Failed { error = Http 503; last = None };
    ];
  let update = Topic.update_codec Codec.int in
  let value : int Topic.update =
    { seq = 7; status = Ready { data = 4; stale = false } }
  in
  Alcotest.(check string)
    "update" {|{"seq":7,"status":["Ready",{"data":4,"stale":false}]}|}
    (text update value);
  round_trip "update" update value;
  Alcotest.(check bool)
    "a missing field is an error" true
    (Result.is_error (Codec.of_string update {|{"status":["Idle"]}|}))

let test_remote_machine_codecs () =
  let endpoint =
    Remote.declare ~path:"/api/item" ~input:Codec.int ~output:Codec.string
      ~rejected:Codec.string
  in
  let machine = Remote_machine.make ~id:"item" endpoint in
  let event_codec = Remote_machine.event_codec endpoint
  and state_codec = Remote_machine.state_codec endpoint in
  List.iter
    (round_trip "event" event_codec)
    [
      Remote_machine.Load 3;
      Retry;
      Settled (Ok "hat");
      Settled (Rejected "gone");
      Settled (Failed (Http 500));
      Reset;
    ];
  List.iter
    (round_trip "state" state_codec)
    [
      Remote_machine.Idle;
      Loading { input = 1; last = Some "old" };
      Ready { input = 1; data = "hat" };
      Rejected { input = 1; reason = "gone"; last = None };
      Failed { input = 2; error = Network "down"; last = Some "old" };
    ];
  Alcotest.(check string)
    "event text" {|["Load",3]|}
    (text event_codec (Remote_machine.Load 3));
  Alcotest.(check (list (option string)))
    "event names"
    [
      Some "load";
      Some "settled ok";
      Some "settled rejected";
      Some "settled error";
    ]
    (List.map
       (Machine.event_name machine)
       [
         Remote_machine.Load 3;
         Settled (Ok "hat");
         Settled (Rejected "gone");
         Settled (Failed (Http 500));
       ]
    )

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "codecs"
    [
      ( "machine",
        [
          t "event names" test_event_names;
          t "registered codecs" test_registered_codecs;
        ]
      );
      ( "codecs",
        [
          t "answer" test_answer_codec;
          t "status and update" test_status_and_update_codecs;
          t "remote machine" test_remote_machine_codecs;
        ]
      );
    ]
