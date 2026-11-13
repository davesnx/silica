open Silica

type target_state = {
  info : Inspect.target;
  instances : (string, int * Inspect.instance) Hashtbl.t;
  mutable next : int;
  steps : Transition.t Queue.t;
  mutable dropped : int;
}

type role =
  | Unknown
  | Target of target_state
  | Client of (string option * Inspect.filter) list ref
  | Closed

type connection = { send_text : string -> bool; mutable role : role }

type t = {
  lock : Mutex.t;
  mutable connections : connection list;
  capacity : int;
  seen : (string, Transition.t list) Hashtbl.t;
}

let create ?(capacity = 5000) () =
  {
    lock = Mutex.create ();
    connections = [];
    capacity;
    seen = Hashtbl.create 16;
  }

let kind (record : Transition.t) =
  ( record.change,
    record.from,
    record.target,
    record.event,
    Transition.label record
  )

let remember hub (record : Transition.t) =
  let known =
    Option.value (Hashtbl.find_opt hub.seen record.machine) ~default:[]
  in
  if
    not
      (List.exists
         (fun (other : Transition.t) -> kind other = kind record)
         known
      )
  then
    Hashtbl.replace hub.seen record.machine (Transition.strip record :: known)

let connect hub ~send =
  let connection = { send_text = send; role = Unknown } in
  Mutex.protect hub.lock (fun () ->
      hub.connections <- connection :: hub.connections
  );
  connection

let send connection message =
  if not (connection.send_text (Inspect.to_string message)) then
    connection.role <- Closed

let targets hub =
  List.filter_map
    (fun connection ->
      match connection.role with
      | Target state ->
          Some state
      | Unknown | Client _ | Closed ->
          None
    )
    hub.connections
  |> List.rev

let clients hub =
  List.filter_map
    (fun connection ->
      match connection.role with
      | Client subscriptions ->
          Some (connection, subscriptions)
      | Unknown | Target _ | Closed ->
          None
    )
    hub.connections

let find_target hub name =
  List.find_opt
    (fun (state : target_state) -> state.info.target = name)
    (targets hub)

let tree_of (state : target_state) =
  Hashtbl.fold (fun _ entry all -> entry :: all) state.instances []
  |> List.sort (fun (left, _) (right, _) -> compare left right)
  |> List.map snd

let forward hub ~target message ~passes =
  List.iter
    (fun (connection, subscriptions) ->
      if
        List.exists
          (fun (wanted, filter) ->
            (wanted = None || wanted = Some target) && passes filter
          )
          !subscriptions
      then
        send connection message
    )
    (clients hub)

let mount (state : target_state) (instance : Inspect.instance) =
  let order =
    match Hashtbl.find_opt state.instances instance.path with
    | Some (order, _) ->
        order
    | None ->
        state.next <- state.next + 1;
        state.next
  in
  Hashtbl.replace state.instances instance.path (order, instance)

let apply hub (state : target_state) (event : Inspect.event) =
  match event with
  | Tree instances ->
      Hashtbl.reset state.instances;
      List.iter (mount state) instances
  | Mount instance ->
      mount state instance
  | Unmount path ->
      Hashtbl.remove state.instances path
  | Step { transition; owns } -> (
      remember hub transition;
      Queue.push transition state.steps;
      if Queue.length state.steps > hub.capacity then (
        ignore (Queue.pop state.steps);
        state.dropped <- state.dropped + 1
      );
      let props instance =
        match transition.props with
        | Some _ ->
            transition.props
        | None ->
            instance.Inspect.props
      in
      match
        (transition.change, Hashtbl.find_opt state.instances transition.instance)
      with
      | Unhandled, Some (order, instance) ->
          Hashtbl.replace state.instances transition.instance
            (order, { instance with props = props instance })
      | (Kept | Entered), Some (order, instance) ->
          Hashtbl.replace state.instances transition.instance
            ( order,
              {
                instance with
                state = transition.target;
                props = props instance;
                data =
                  ( match transition.snapshot with
                  | Some _ ->
                      transition.snapshot
                  | None ->
                      instance.data
                  );
                owns = Option.value owns ~default:instance.owns;
              }
            )
      | _ ->
          ()
    )
  | Live _ | Resumed _ | Paused _ ->
      ()

let trace_of (state : target_state) =
  let transitions = List.of_seq (Queue.to_seq state.steps) in
  let root =
    match
      List.find_opt
        (fun (record : Transition.t) ->
          not (String.contains record.instance '/')
        )
        transitions
    with
    | Some record ->
        record.machine
    | None -> (
        match transitions with record :: _ -> record.machine | [] -> ""
      )
  in
  Trace.to_jsonl
    {
      header =
        {
          root;
          url = state.info.url;
          build = "dev";
          origin =
            Runtime
              {
                platform =
                  ( match state.info.kind with
                  | Browser ->
                      "browser"
                  | Server ->
                      "server"
                  );
              };
          dropped = state.dropped;
          loop_from = None;
        };
      transitions;
    }

let answer hub connection id (request : Inspect.request) =
  let failure message = send connection (Failure { id; message }) in
  let chosen name =
    match (name, targets hub) with
    | Some name, _ -> (
        match find_target hub name with
        | Some state ->
            Ok [ state ]
        | None ->
            Error ("no target " ^ name)
      )
    | None, all ->
        Ok all
  in
  match request with
  | Targets ->
      send connection
        (Target_list
           {
             id;
             targets =
               List.map (fun (state : target_state) -> state.info) (targets hub);
           }
        )
  | Get_tree name -> (
      match chosen name with
      | Ok states ->
          List.iter
            (fun (state : target_state) ->
              send connection
                (Event
                   {
                     id;
                     target = state.info.target;
                     event = Tree (tree_of state);
                   }
                )
            )
            states
      | Error message ->
          failure message
    )
  | Export name -> (
      match chosen name with
      | Ok [ state ] ->
          send connection
            (Trace_file
               { id; target = state.info.target; trace = trace_of state }
            )
      | Ok [] ->
          failure "no target is connected"
      | Ok states ->
          failure
            ("several targets are connected; name one of: "
            ^ String.concat ", "
                (List.map
                   (fun (state : target_state) -> state.info.target)
                   states
                )
            )
      | Error message ->
          failure message
    )
  | Seen machine ->
      send connection
        (Seen_steps
           {
             id;
             machine;
             steps =
               List.rev
                 (Option.value (Hashtbl.find_opt hub.seen machine) ~default:[]);
           }
        )
  | Travel _ | Return_live _ ->
      failure "time travel is not available yet"

let handle hub connection (message : Inspect.message) =
  match (connection.role, message) with
  | Unknown, Hello_target info ->
      List.iter
        (fun other ->
          match other.role with
          | Target state when state.info.target = info.target ->
              other.role <- Closed
          | Unknown | Target _ | Client _ | Closed ->
              ()
        )
        hub.connections;
      connection.role <-
        Target
          {
            info;
            instances = Hashtbl.create 16;
            next = 0;
            steps = Queue.create ();
            dropped = 0;
          };
      forward hub ~target:info.target message ~passes:(fun _ -> true)
  | Unknown, Hello_client _ ->
      connection.role <- Client (ref []);
      answer hub connection None Targets
  | Target state, Event { event; _ } ->
      apply hub state event;
      let target = state.info.target in
      forward hub ~target
        (Event { id = None; target; event })
        ~passes:(fun filter -> Inspect.passes filter event)
  | Client subscriptions, Subscribe { id; target; filter } ->
      subscriptions := !subscriptions @ [ (target, filter) ];
      send connection (Subscribed { id })
  | Client _, Request { id; request } ->
      answer hub connection id request
  | Unknown, _ ->
      send connection
        (Failure { id = None; message = "the first message must be a hello" })
  | (Target _ | Client _), _ ->
      send connection
        (Failure { id = None; message = "this message is not for this role" })
  | Closed, _ ->
      ()

let is_hello text =
  match Silica_json.Json.of_string text with
  | Ok json -> (
      match Silica_json.Json.classify json with
      | `Assoc fields ->
          List.assoc_opt "t" fields
          |> Option.map Silica_json.Json.classify
          = Some (`String "hello")
      | _ ->
          false
    )
  | Error _ ->
      false

let receive hub connection text =
  Mutex.protect hub.lock (fun () ->
      match connection.role with
      | Unknown when not (is_hello text) ->
          `Other
      | _ -> (
          match Inspect.of_string text with
          | Ok message ->
              handle hub connection message;
              `Inspector
          | Error message ->
              send connection (Failure { id = None; message });
              if connection.role = Unknown then connection.role <- Closed;
              `Inspector
        )
  )

let is_closed connection = connection.role = Closed

let disconnect hub connection =
  Mutex.protect hub.lock (fun () ->
      let was = connection.role in
      connection.role <- Closed;
      hub.connections <-
        List.filter (fun other -> other != connection) hub.connections;
      match was with
      | Target state ->
          forward hub ~target:state.info.target (Gone state.info.target)
            ~passes:(fun _ -> true
          )
      | Unknown | Client _ | Closed ->
          ()
  )

type listener = {
  socket : Unix.file_descr;
  path : string;
  stopped : bool Atomic.t;
}

let write_line socket lock text =
  Mutex.protect lock (fun () ->
      let line = text ^ "\n" in
      try
        let rec loop offset =
          if offset < String.length line then
            loop
              (offset
              + Unix.write_substring socket line offset
                  (String.length line - offset)
              )
        in
        loop 0;
        true
      with Unix.Unix_error _ -> false
  )

let serve_lines hub socket =
  let lock = Mutex.create () in
  let connection = connect hub ~send:(write_line socket lock) in
  let input = Unix.in_channel_of_descr socket in
  let rec loop () =
    match In_channel.input_line input with
    | Some text when String.length text > 1_048_576 ->
        ()
    | Some text ->
        if String.trim text <> "" then ignore (receive hub connection text);
        if not (is_closed connection) then loop ()
    | None ->
        ()
    | exception Sys_error _ ->
        ()
  in
  loop ();
  disconnect hub connection;
  try Unix.close socket with Unix.Unix_error _ -> ()

let bound ~path =
  ( match Unix.lstat path with
  | { st_kind = S_SOCK; _ } ->
      Unix.unlink path
  | _ ->
      ()
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      ()
  );
  let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let mask = Unix.umask 0o177 in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.umask mask))
    (fun () ->
      try
        Unix.bind socket (Unix.ADDR_UNIX path);
        Unix.chmod path 0o600;
        Unix.listen socket 16;
        socket
      with error ->
        Unix.close socket;
        raise error
    )

let listen hub ~path =
  match bound ~path with
  | exception Unix.Unix_error (error, _, _) ->
      Error (error, path ^ ": " ^ Unix.error_message error)
  | socket ->
      let listener = { socket; path; stopped = Atomic.make false } in
      let rec accept () =
        match Unix.accept ~cloexec:true socket with
        | client, _ ->
            ignore (Thread.create (serve_lines hub) client);
            accept ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) ->
            accept ()
        | exception Unix.Unix_error _ when not (Atomic.get listener.stopped) ->
            Thread.delay 0.05;
            accept ()
        | exception Unix.Unix_error _ ->
            ()
      in
      ignore (Thread.create accept ());
      Ok listener

let stop listener =
  Atomic.set listener.stopped true;
  ( try Unix.shutdown listener.socket Unix.SHUTDOWN_ALL
    with Unix.Unix_error _ -> ()
  );
  (try Unix.close listener.socket with Unix.Unix_error _ -> ());
  try Unix.unlink listener.path with Unix.Unix_error _ -> ()

let subscribers hub =
  Mutex.protect hub.lock (fun () ->
      List.length
        (List.filter
           (fun (_, subscriptions) -> !subscriptions <> [])
           (clients hub)
        )
  )
