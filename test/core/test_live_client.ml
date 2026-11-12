open Silica

type socket = { events : Platform.socket_events; mutable sent : string list }

type fake = {
  mutable sockets : socket list;
  mutable timers : (int * int * (unit -> unit)) list;
  mutable now : int;
  mutable next : int;
}

let fake () = { sockets = []; timers = []; now = 0; next = 0 }

let io fake : Platform.io =
  {
    schedule =
      (fun ms run ->
        fake.next <- fake.next + 1;
        let key = fake.next in
        fake.timers <- (key, fake.now + ms, run) :: fake.timers;
        fun () ->
          fake.timers <-
            List.filter (fun (other, _, _) -> other <> key) fake.timers
      );
    post = (fun ~path:_ ~body:_ _ -> ());
    connect =
      (fun events ->
        let socket = { events; sent = [] } in
        fake.sockets <- fake.sockets @ [ socket ];
        {
          send = (fun text -> socket.sent <- socket.sent @ [ text ]);
          close = ignore;
        }
      );
    csrf = (fun () -> "token");
    session_gone = ignore;
    log = ignore;
  }

let advance fake ms =
  fake.now <- fake.now + ms;
  let due, later =
    List.partition (fun (_, at, _) -> at <= fake.now) fake.timers
  in
  fake.timers <- later;
  List.iter (fun (_, _, run) -> run ()) (List.sort compare due)

let last fake = List.nth fake.sockets (List.length fake.sockets - 1)

let welcome socket ~ack =
  socket.events.on_open ();
  socket.events.on_message {|{"t":"hello","v":2}|};
  socket.events.on_message
    (Printf.sprintf {|{"t":"state","seq":1,"ack":%d,"state":0,"actions":[]}|}
       ack
    )

let events socket =
  List.filter
    (fun text ->
      String.length text > 10 && String.sub text 0 10 = {|{"t":"even|}
    )
    socket.sent

let client fake =
  Live_client.start ~io:(io fake)
    ~sub:(Protocol.Island { channel = "counter"; instance = "i" })
    ~on_state:(fun ~seq:_ ~state:_ ~actions:_ -> ())
    ~on_down:ignore ~on_fatal:ignore ()

let send client = Live_client.send_event client Codec.string "inc"

let tests =
  let open Alcotest in
  [
    test_case
      "an event before the first state is sent after it, with the next id"
      `Quick (fun () ->
        let fake = fake () in
        let client = client fake in
        send client;
        check int "nothing yet" 0 (List.length (events (last fake)));
        welcome (last fake) ~ack:4;
        check (list string) "sent after the state"
          [ {|{"t":"event","id":5,"event":"inc"}|} ]
          (events (last fake))
    );
    test_case "events during a loss are sent after the state of the new socket"
      `Quick (fun () ->
        let fake = fake () in
        let client = client fake in
        welcome (last fake) ~ack:0;
        send client;
        (last fake).events.on_close 1006;
        send client;
        send client;
        advance fake 200;
        check int "a new socket" 2 (List.length fake.sockets);
        (last fake).events.on_open ();
        (last fake).events.on_message {|{"t":"hello","v":2}|};
        check int "held until the state" 0 (List.length (events (last fake)));
        (last fake).events.on_message
          {|{"t":"state","seq":1,"ack":1,"state":1,"actions":[]}|};
        check (list string) "in order"
          [
            {|{"t":"event","id":2,"event":"inc"}|};
            {|{"t":"event","id":3,"event":"inc"}|};
          ]
          (events (last fake))
    );
    test_case "a held event is dropped after 10 seconds" `Quick (fun () ->
        let fake = fake () in
        let client = client fake in
        send client;
        advance fake 10_000;
        send client;
        welcome (last fake) ~ack:0;
        check (list string) "only the young one"
          [ {|{"t":"event","id":1,"event":"inc"}|} ]
          (events (last fake))
    );
    test_case "at most 16 events are held, the oldest dropped first" `Quick
      (fun () ->
        let fake = fake () in
        let client = client fake in
        for index = 1 to 20 do
          Live_client.send_event client Codec.string (string_of_int index)
        done;
        welcome (last fake) ~ack:0;
        let sent = events (last fake) in
        check int "16" 16 (List.length sent);
        check string "the first kept is the fifth"
          {|{"t":"event","id":1,"event":"5"}|} (List.hd sent)
    );
    test_case "stop drops the held events and their timers" `Quick (fun () ->
        let fake = fake () in
        let client = client fake in
        send client;
        Live_client.stop client;
        check int "no timer left" 0 (List.length fake.timers)
    );
  ]

let () = Alcotest.run "live client" [ ("held events", tests) ]
