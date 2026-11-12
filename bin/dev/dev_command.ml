open Cmdliner

let state_directory = Filename.concat "_build" "silica-dev"
let inspect_socket = Filename.concat state_directory "inspect.sock"

let rec make_directory path =
  if not (Sys.file_exists path) then (
    make_directory (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  )

let lock_file () =
  let path = Filename.concat state_directory "lock" in
  let descriptor =
    Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC ] 0o600
  in
  match Unix.lockf descriptor Unix.F_TLOCK 0 with
  | () ->
      Unix.ftruncate descriptor 0;
      let pid = string_of_int (Unix.getpid ()) in
      ignore (Unix.write_substring descriptor pid 0 (String.length pid));
      Ok descriptor
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
      let owner =
        try String.trim (In_channel.with_open_bin path In_channel.input_all)
        with Sys_error _ -> "?"
      in
      Unix.close descriptor;
      Error
        (Printf.sprintf
           "another silica dev runs in this project (pid %s). Stop it first, \
            or use its page."
           owner
        )

let secret () =
  let path = Filename.concat state_directory "secret" in
  match In_channel.with_open_bin path In_channel.input_all with
  | text when String.length (String.trim text) >= 32 ->
      String.trim text
  | _ | (exception Sys_error _) ->
      let secret = Silica_server_kit.Random_source.hex_id ~bytes:32 in
      Out_channel.with_open_gen
        [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o600 path
        (fun channel -> output_string channel secret
      );
      secret

let app_environment ~static ~metafile ~secret ~proxy_port ~port =
  let ours =
    [
      ("PORT", string_of_int port);
      ("SILICA_SECRET", secret);
      ( "SILICA_ORIGINS",
        Printf.sprintf "http://127.0.0.1:%d,http://localhost:%d" proxy_port
          proxy_port
      );
      ("SILICA_STATIC", static);
      ("SILICA_METAFILE", metafile);
      ("SILICA_DEV_STATE", Filename.concat state_directory "state.json");
      ("SILICA_INSPECT", inspect_socket);
    ]
  in
  let inherited =
    Array.to_list (Unix.environment ())
    |> List.filter (fun binding ->
        match String.index_opt binding '=' with
        | Some index ->
            not (List.mem_assoc (String.sub binding 0 index) ours)
        | None ->
            true
    )
  in
  Array.of_list
    (inherited @ List.map (fun (name, value) -> name ^ "=" ^ value) ours)

let stop_requested = Atomic.make false
let ended = Atomic.make false

let on_signal = Sys.Signal_handle (fun _ -> Atomic.set stop_requested true)

let fail message =
  prerr_endline ("silica dev: " ^ message);
  1

let run ~server ~client ~port ~json =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let root = Sys.getcwd () in
  let server_path = Filename.concat "_build/default" server in
  let metafile =
    Filename.concat "_build/default" (Filename.concat client "meta.json")
  in
  let static =
    Filename.concat "_build/default" (Filename.concat client "dist")
  in
  let print_lock = Mutex.create () in
  let print event =
    Mutex.protect print_lock (fun () ->
        if json then
          match event with
          | Dev_event.Build_progress _ ->
              ()
          | _ ->
              print_endline (Dev_event.to_string event)
        else
          Option.iter print_endline (Dev_event.to_text event)
    )
  in
  let hub = Hub.create ~report:print () in
  let emit event =
    Hub.publish hub event;
    print event
  in
  if not (Sys.file_exists "dune-project") then
    fail "no dune-project here. Run the command from the root of the app."
  else (
    make_directory state_directory;
    match lock_file () with
    | Error message ->
        fail message
    | Ok _lock -> (
        match Build_watch.connect () with
        | Some socket ->
            Unix.close socket;
            fail
              "a dune build --watch already runs in this project. Stop it; \
               silica dev starts its own."
        | None -> (
            let secret = secret () in
            let supervisor_cell = ref None in
            let app_port ~timeout =
              match !supervisor_cell with
              | Some supervisor ->
                  Supervisor.app_port supervisor ~timeout
              | None ->
                  None
            in
            match
              Inspector_hub.listen (Hub.inspector hub) ~path:inspect_socket
            with
            | Error message ->
                fail ("cannot listen for the inspector: " ^ message)
            | Ok inspect_listener -> (
                match
                  Proxy.start ~port ~app_port ~on_dev_socket:(Hub.attach hub)
                    ~client_script:Dev_client_script.source
                with
                | Error message ->
                    Inspector_hub.stop inspect_listener;
                    fail
                      (Printf.sprintf
                         "cannot listen on port %d: %s. Choose another with \
                          --port."
                         port message
                      )
                | Ok proxy -> (
                    let proxy_port = Proxy.port proxy in
                    let supervisor =
                      Supervisor.create ~server:server_path ~metafile
                        ~environment:
                          (app_environment ~static ~metafile ~secret ~proxy_port)
                        ~emit ~echo:(fun line ->
                          Mutex.protect print_lock (fun () -> prerr_endline line)
                      )
                    in
                    supervisor_cell := Some supervisor;
                    List.iter
                      (fun signal -> Sys.set_signal signal on_signal)
                      [ Sys.sigint; Sys.sigterm; Sys.sighup ];
                    emit
                      (Dev_event.Listening
                         {
                           url = Printf.sprintf "http://127.0.0.1:%d" proxy_port;
                         }
                      );
                    let phase = ref `Idle and errors = ref [] in
                    let on_progress = function
                      | Build_watch.Running { complete; remaining } ->
                          if !phase <> `Building then (
                            phase := `Building;
                            emit Dev_event.Build_started
                          );
                          Hub.publish hub
                            (Dev_event.Build_progress { complete; remaining })
                      | Failed ->
                          phase := `Failed;
                          emit (Dev_event.Build_failed !errors)
                      | Success ->
                          phase := `Ok;
                          emit Dev_event.Build_ok;
                          Supervisor.build_succeeded supervisor
                      | Waiting | Interrupted ->
                          ()
                    in
                    let on_errors latest =
                      let changed = latest <> !errors in
                      errors := latest;
                      if changed && !phase = `Failed then
                        emit (Dev_event.Build_failed latest)
                    in
                    let watch =
                      Build_watch.start ~root
                        ~targets:[ server; Filename.concat client "meta.json" ]
                        ~log:(Filename.concat state_directory "dune.log")
                        ~on_progress ~on_errors
                        ~on_end:(fun () -> Atomic.set ended true)
                    in
                    let finish () =
                      Supervisor.stop supervisor;
                      Proxy.stop proxy;
                      Inspector_hub.stop inspect_listener
                    in
                    match watch with
                    | Error message ->
                        finish ();
                        fail message
                    | Ok watch ->
                        while
                          not (Atomic.get stop_requested || Atomic.get ended)
                        do
                          Thread.delay 0.05
                        done;
                        let lost = not (Atomic.get stop_requested) in
                        Build_watch.stop watch;
                        finish ();
                        if lost then
                          fail
                            ("dune stopped:\n"
                            ^ Build_watch.log_tail
                                (Filename.concat state_directory "dune.log")
                            )
                        else
                          0
                  )
              )
          )
      )
  )

let command =
  let server =
    Arg.(
      value
      & opt string "server/main.exe"
      & info [ "server" ] ~docv:"EXE"
          ~doc:"The server executable, as a dune target from the app root."
    )
  in
  let client =
    Arg.(
      value & opt string "client"
      & info [ "client" ] ~docv:"DIR"
          ~doc:
            "The client directory: dune builds $(docv)/meta.json and \
             $(docv)/dist with esbuild."
    )
  in
  let port =
    Arg.(
      value & opt int 8080
      & info [ "port" ] ~docv:"PORT" ~doc:"The port of the page, on 127.0.0.1."
    )
  in
  let json =
    Arg.(
      value & flag
      & info [ "json" ]
          ~doc:"Print each event as one JSON object on one line of stdout."
    )
  in
  Cmd.v
    (Cmd.info "dev" ~doc:"Run the app in development: build, restart, reload."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Runs $(b,dune build --watch) on the server and the client, \
              starts the server when a build succeeds, and serves the app on \
              one port. After each change the server starts again with its \
              sessions and Live state, and the page reloads. A compile error \
              shows in the page, with file, line and source lines, until a \
              build succeeds.";
           `P
             "With $(b,--json), each event is one JSON line on stdout: \
              $(b,{\"t\":\"build\",\"status\":\"started\"}), $(b,\"failed\") \
              with $(b,errors), $(b,\"ok\"); \
              $(b,{\"t\":\"server\",\"status\":\"started\"}), \
              $(b,\"restarted\"), $(b,\"crashed\") with $(b,code) and \
              $(b,output). The server output goes to stderr.";
           `S Manpage.s_exit_status;
           `P
             "0 after SIGINT or SIGTERM. 1 when the command cannot start, or \
              when dune stops.";
         ]
       ~exits:[]
    )
    Term.(
      const (fun server client port json -> run ~server ~client ~port ~json)
      $ server $ client $ port $ json
    )
