module Adapter = Silica_adapter

let read_file path = In_channel.with_open_bin path In_channel.input_all

let () =
  let port =
    ref
      ( match Sys.getenv_opt "PORT" with
      | Some p ->
          Option.value (int_of_string_opt p) ~default:8080
      | None ->
          8080
      )
  in
  let static =
    ref
      (Option.value
         (Sys.getenv_opt "SILICA_STATIC")
         ~default:"_build/default/linear/client/dist"
      )
  in
  let metafile =
    ref
      (Option.value
         (Sys.getenv_opt "SILICA_METAFILE")
         ~default:"_build/default/linear/client/meta.json"
      )
  in
  let count =
    ref
      (Option.value
         (Option.bind (Sys.getenv_opt "SILICA_LINEAR_ISSUES") int_of_string_opt)
         ~default:30
      )
  in
  let max_connections =
    ref
      (Option.value
         (Option.bind
            (Sys.getenv_opt "SILICA_MAX_CONNECTIONS")
            int_of_string_opt
         )
         ~default:1024
      )
  in
  let origins = ref [] in
  Arg.parse
    [
      ("--port", Arg.Set_int port, "port (default 8080, env PORT)");
      ( "--max-connections",
        Arg.Set_int max_connections,
        "open connections at most, Live sockets included (default 1024, env \
         SILICA_MAX_CONNECTIONS)"
      );
      ( "--static",
        Arg.Set_string static,
        "directory served at /static/ (env SILICA_STATIC)"
      );
      ( "--metafile",
        Arg.Set_string metafile,
        "esbuild metafile (env SILICA_METAFILE)"
      );
      ( "--issues",
        Arg.Set_int count,
        "number of seed issues (default 30, env SILICA_LINEAR_ISSUES)"
      );
      ( "--origin",
        Arg.String (fun o -> origins := o :: !origins),
        "allowed Origin, repeatable; with none, only local addresses work"
      );
    ]
    ignore "mini Linear server";
  let origins =
    match !origins with
    | [] ->
        Silica_server.Localhost
    | list ->
        Silica_server.Origins list
  in
  let dev = Silica_server.dev_of_env () in
  let secret =
    match Silica_server.secret_of_env ~origins ~dev with
    | Ok secret ->
        secret
    | Error message ->
        prerr_endline ("mini Linear: " ^ message);
        exit 1
  in
  let config = { (Silica_server.default_config ~origins ~secret) with dev } in
  let assets =
    match
      Silica_server.Assets.of_metafile ~prefix:"/static/" (read_file !metafile)
    with
    | Ok assets ->
        assets
    | Error message ->
        prerr_endline ("mini Linear: bad metafile: " ^ message);
        exit 1
  in
  let server =
    Linear_server.create ~count:!count config ~assets
      ~read:(Adapter.read_dir !static) ()
  in
  Adapter.run ~port:!port ~max_connections:!max_connections
    ~on_stop:(fun () -> Adapter.Server.shutdown server)
    server
