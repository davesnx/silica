module Adapter = Silica_adapter
module Server = Adapter.Server

let read_file path = In_channel.with_open_bin path In_channel.input_all

let () =
  Example_codecs.register ();
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
         ~default:"_build/default/example/client/dist"
      )
  in
  let metafile =
    ref
      (Option.value
         (Sys.getenv_opt "SILICA_METAFILE")
         ~default:"_build/default/example/client/meta.json"
      )
  in
  let origins = ref [] in
  Arg.parse
    [
      ("--port", Arg.Set_int port, "port (default 8080, env PORT)");
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
        "allowed Origin, repeatable; with none, only local addresses work"
      );
    ]
    ignore "silica example server";
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
        prerr_endline ("silica example: " ^ message);
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
        prerr_endline ("silica example: bad metafile: " ^ message);
        exit 1
  in
  let t =
    Example_server.create config ~assets ~read:(Adapter.read_dir !static)
  in
  Adapter.run ~port:!port ~on_stop:(fun () -> Server.shutdown t) t
