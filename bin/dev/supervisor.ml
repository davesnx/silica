module Json = Silica_json.Json

type app = {
  child : Child_process.t;
  port : int;
  reader : Thread.t;
  output : string Queue.t;
}

type phase = No_app | Starting | Running of app | Crashed

type t = {
  server : string;
  metafile : string;
  environment : port:int -> string array;
  emit : Dev_event.t -> unit;
  echo : string -> unit;
  lock : Mutex.t;
  mutable phase : phase;
  mutable digests : (string * string) option;
  mutable outputs : string list option;
  mutable built_pending : bool;
  mutable stop_requested : bool;
  wake : Condition.t;
  mutable worker : Thread.t option;
}

let health_timeout = 10.
let stop_grace = 3.
let kept_lines = 60

let with_lock supervisor run = Mutex.protect supervisor.lock run

let free_port () =
  let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) ->
          port
      | Unix.ADDR_UNIX _ ->
          failwith "free_port: not an internet socket"
    )

let healthy port =
  let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      try
        Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
        let request =
          "GET /_silica/health HTTP/1.1\r\n\
           Host: 127.0.0.1\r\n\
           Connection: close\r\n\
           \r\n"
        in
        ignore (Unix.write_substring socket request 0 (String.length request));
        let buffer = Bytes.create 12 in
        let count = Unix.read socket buffer 0 12 in
        count = 12 && Bytes.sub_string buffer 9 3 = "200"
      with Unix.Unix_error _ -> false
    )

let digest path =
  match Digest.file path with
  | value ->
      Some (Digest.to_hex value)
  | exception Sys_error _ ->
      None

let metafile_outputs metafile =
  match
    Json.of_string (In_channel.with_open_bin metafile In_channel.input_all)
  with
  | exception Sys_error _ ->
      None
  | Error _ ->
      None
  | Ok json -> (
      match Json.classify json with
      | `Assoc fields -> (
          match Option.map Json.classify (List.assoc_opt "outputs" fields) with
          | Some (`Assoc outputs) ->
              Some outputs
          | _ ->
              None
        )
      | _ ->
          None
    )

let outputs metafile = Option.map (List.map fst) (metafile_outputs metafile)

let entry_of metafile =
  Option.bind (metafile_outputs metafile) (fun outputs ->
      List.find_map
        (fun (name, output) ->
          match Json.classify output with
          | `Assoc fields
            when Filename.check_suffix name ".js"
                 && List.mem_assoc "entryPoint" fields ->
              Some name
          | _ ->
              None
        )
        outputs
  )

let without_hash path =
  let base = Filename.remove_extension path in
  match String.rindex_opt base '-' with
  | Some index ->
      String.sub base 0 index ^ Filename.extension path
  | None ->
      path

let url path = "/static/" ^ Filename.basename path

let reload_of ~entry ~before ~after =
  let named suffix names =
    List.filter (fun name -> Filename.check_suffix name suffix) names
  in
  match (before, after) with
  | Some before, Some after ->
      let old_sheets = named ".css" before
      and new_sheets = named ".css" after in
      if
        List.sort compare (List.map without_hash old_sheets)
        <> List.sort compare (List.map without_hash new_sheets)
      then
        Some Dev_event.Page
      else
        let swaps =
          List.filter_map
            (fun old ->
              let name =
                List.find
                  (fun name -> without_hash name = without_hash old)
                  new_sheets
              in
              if name = old then
                None
              else
                Some { Dev_event.from = url old; target = url name }
            )
            old_sheets
        in
        if named ".js" before <> named ".js" after then
          match entry with
          | Some entry ->
              Some (Dev_event.Code { entry = url entry; css = swaps })
          | None ->
              Some Dev_event.Page
        else if swaps = [] then
          None
        else
          Some (Dev_event.Stylesheets swaps)
  | _ ->
      Some Dev_event.Page

let read_output channel output echo =
  let rec loop () =
    match In_channel.input_line channel with
    | Some line ->
        echo line;
        Queue.push line output;
        if Queue.length output > kept_lines then ignore (Queue.pop output);
        loop ()
    | None ->
        In_channel.close channel
  in
  try loop () with Sys_error _ -> ()

let crashed supervisor app status =
  Thread.join app.reader;
  supervisor.emit
    (Dev_event.Server_crashed
       {
         code = Child_process.describe status;
         output = String.concat "\n" (List.of_seq (Queue.to_seq app.output));
       }
    )

let on_exit supervisor app_cell status =
  let report =
    with_lock supervisor (fun () ->
        match (supervisor.phase, !app_cell) with
        | Running running, Some app when running.child == app.child ->
            supervisor.phase <- Crashed;
            Some app
        | _ ->
            None
    )
  in
  Option.iter (fun app -> crashed supervisor app status) report

let launch supervisor =
  let port = free_port () in
  let read_end, write_end = Unix.pipe ~cloexec:true () in
  let app_cell = ref None in
  let child =
    Child_process.spawn ~program:supervisor.server ~arguments:[]
      ~environment:(supervisor.environment ~port) ~output:write_end
      ~on_exit:(fun status -> on_exit supervisor app_cell status
    )
  in
  Unix.close write_end;
  let output = Queue.create () in
  let reader =
    Thread.create
      (fun () ->
        read_output (Unix.in_channel_of_descr read_end) output supervisor.echo
      )
      ()
  in
  let app = { child; port; reader; output } in
  app_cell := Some app;
  app

let wait_healthy supervisor app =
  let deadline = Unix.gettimeofday () +. health_timeout in
  let rec loop () =
    match Child_process.status app.child with
    | Some status ->
        `Exited status
    | None ->
        if with_lock supervisor (fun () -> supervisor.stop_requested) then
          `Stopped
        else if healthy app.port then
          `Healthy
        else if Unix.gettimeofday () > deadline then
          `Timeout
        else (
          Thread.delay 0.02;
          loop ()
        )
  in
  loop ()

let stop_app app =
  Child_process.stop app.child ~grace:stop_grace;
  Thread.join app.reader

let restart supervisor ~reload =
  let previous =
    with_lock supervisor (fun () ->
        let previous = supervisor.phase in
        supervisor.phase <- Starting;
        previous
    )
  in
  let restart =
    match previous with
    | Running app ->
        stop_app app;
        true
    | No_app | Starting ->
        false
    | Crashed ->
        true
  in
  let started = Unix.gettimeofday () in
  let app = launch supervisor in
  match wait_healthy supervisor app with
  | `Stopped ->
      stop_app app
  | `Healthy ->
      with_lock supervisor (fun () -> supervisor.phase <- Running app);
      supervisor.emit
        (Dev_event.Server_started
           {
             pid = Child_process.pid app.child;
             milliseconds =
               int_of_float ((Unix.gettimeofday () -. started) *. 1000.);
             restart;
           }
        );
      let reload =
        match previous with Running _ -> reload | _ -> Some Dev_event.Page
      in
      Option.iter
        (fun reload -> supervisor.emit (Dev_event.Reload reload))
        reload
  | `Exited status ->
      with_lock supervisor (fun () -> supervisor.phase <- Crashed);
      crashed supervisor app status
  | `Timeout ->
      with_lock supervisor (fun () -> supervisor.phase <- Crashed);
      stop_app app;
      supervisor.emit
        (Dev_event.Server_crashed
           {
             code = "no answer";
             output =
               Printf.sprintf
                 "GET /_silica/health gave no 200 within %.0f s\n%s"
                 health_timeout
                 (String.concat "\n" (List.of_seq (Queue.to_seq app.output)));
           }
        )

let built supervisor =
  match (digest supervisor.server, digest supervisor.metafile) with
  | None, _ ->
      with_lock supervisor (fun () -> supervisor.phase <- Crashed);
      supervisor.emit
        (Dev_event.Server_crashed
           { code = "missing"; output = "no file " ^ supervisor.server }
        )
  | _, None ->
      with_lock supervisor (fun () -> supervisor.phase <- Crashed);
      supervisor.emit
        (Dev_event.Server_crashed
           { code = "missing"; output = "no file " ^ supervisor.metafile }
        )
  | Some server, Some metafile ->
      let running =
        with_lock supervisor (fun () ->
            match supervisor.phase with Running _ -> true | _ -> false
        )
      in
      if (not running) || supervisor.digests <> Some (server, metafile) then (
        let after = outputs supervisor.metafile in
        let reload =
          reload_of
            ~entry:(entry_of supervisor.metafile)
            ~before:supervisor.outputs ~after
        in
        supervisor.digests <- Some (server, metafile);
        supervisor.outputs <- after;
        restart supervisor ~reload
      )

let rec work supervisor =
  let job =
    with_lock supervisor (fun () ->
        while not (supervisor.built_pending || supervisor.stop_requested) do
          Condition.wait supervisor.wake supervisor.lock
        done;
        if supervisor.stop_requested then (
          let phase = supervisor.phase in
          supervisor.phase <- No_app;
          `Stop phase
        ) else (
          supervisor.built_pending <- false;
          `Built
        )
    )
  in
  match job with
  | `Built ->
      built supervisor;
      work supervisor
  | `Stop (Running app) ->
      stop_app app
  | `Stop (No_app | Starting | Crashed) ->
      ()

let create ~server ~metafile ~environment ~emit ~echo =
  let supervisor =
    {
      server;
      metafile;
      environment;
      emit;
      echo;
      lock = Mutex.create ();
      phase = No_app;
      digests = None;
      outputs = None;
      built_pending = false;
      stop_requested = false;
      wake = Condition.create ();
      worker = None;
    }
  in
  supervisor.worker <- Some (Thread.create work supervisor);
  supervisor

let build_succeeded supervisor =
  with_lock supervisor (fun () ->
      supervisor.built_pending <- true;
      Condition.signal supervisor.wake
  )

let stop supervisor =
  with_lock supervisor (fun () ->
      supervisor.stop_requested <- true;
      Condition.signal supervisor.wake
  );
  Option.iter Thread.join supervisor.worker

let app_port supervisor ~timeout =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    match with_lock supervisor (fun () -> supervisor.phase) with
    | Running app ->
        Some app.port
    | Starting when Unix.gettimeofday () < deadline ->
        Thread.delay 0.02;
        loop ()
    | No_app | Starting | Crashed ->
        None
  in
  loop ()
