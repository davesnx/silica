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
         ~default:"_build/default/site/client/dist"
      )
  in
  let metafile =
    ref
      (Option.value
         (Sys.getenv_opt "SILICA_METAFILE")
         ~default:"_build/default/site/client/meta.json"
      )
  in
  let addr =
    ref (Option.value (Sys.getenv_opt "SILICA_ADDR") ~default:"127.0.0.1")
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
  let origins =
    ref
      ( match Sys.getenv_opt "SILICA_ORIGINS" with
      | Some list ->
          List.filter (( <> ) "") (String.split_on_char ',' list)
      | None ->
          []
      )
  in
  let secure_cookie = ref (Sys.getenv_opt "SILICA_SECURE_COOKIE" = Some "1") in
  Arg.parse
    [
      ("--port", Arg.Set_int port, "port (default 8080, env PORT)");
      ( "--max-connections",
        Arg.Set_int max_connections,
        "open connections at most, Live sockets included (default 1024, env \
         SILICA_MAX_CONNECTIONS)"
      );
      ( "--addr",
        Arg.Set_string addr,
        "address to listen on (default 127.0.0.1, env SILICA_ADDR)"
      );
      ( "--static",
        Arg.Set_string static,
        "directory served at /static/ (env SILICA_STATIC)"
      );
      ( "--metafile",
        Arg.Set_string metafile,
        "esbuild metafile (env SILICA_METAFILE)"
      );
      ( "--origin",
        Arg.String (fun o -> origins := o :: !origins),
        "allowed Origin, repeatable; with none, only local addresses work (env \
         SILICA_ORIGINS, comma separated)"
      );
      ( "--secure-cookie",
        Arg.Set secure_cookie,
        "mark the session cookie Secure, for TLS (env SILICA_SECURE_COOKIE=1)"
      );
    ]
    ignore "Silica site server";
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
        prerr_endline ("site: " ^ message);
        exit 1
  in
  let config =
    {
      (Silica_server.default_config ~origins ~secret) with
      dev;
      secure_cookie = !secure_cookie;
    }
  in
  let assets =
    match
      Silica_server.Assets.of_metafile ~prefix:"/static/" (read_file !metafile)
    with
    | Ok assets ->
        assets
    | Error message ->
        prerr_endline ("site: bad metafile: " ^ message);
        exit 1
  in
  let server =
    Site_server.create config ~assets ~read:(Adapter.read_dir !static) ()
  in
  Adapter.run ~port:!port ~max_connections:!max_connections ~addr:!addr
    ~on_stop:(fun () -> Adapter.Server.shutdown server)
    server
