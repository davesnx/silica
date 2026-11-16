module Adapter = Silica_adapter
module Server = Adapter.Server

let read_file path = In_channel.with_open_bin path In_channel.input_all

let () =
  Todomvc_codecs.register ();
  let port =
    ref
      ( match Sys.getenv_opt "PORT" with
      | Some port ->
          Option.value (int_of_string_opt port) ~default:5000
      | None ->
          5000
      )
  in
  let static = ref "_build/default/todomvc/client/dist" in
  let metafile = ref "_build/default/todomvc/client/meta.json" in
  let styles = ref "todomvc/styles" in
  let origins = ref [] in
  Arg.parse
    [
      ("--port", Arg.Set_int port, "port (default 5000, env PORT)");
      ("--static", Arg.Set_string static, "directory of the client bundle");
      ("--metafile", Arg.Set_string metafile, "esbuild metafile");
      ("--styles", Arg.Set_string styles, "directory of the TodoMVC stylesheets");
      ( "--origin",
        Arg.String (fun origin -> origins := origin :: !origins),
        "allowed Origin, repeatable; with none, only local addresses work"
      );
    ]
    ignore "silica TodoMVC server";
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
        prerr_endline ("silica todomvc: " ^ message);
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
        prerr_endline ("silica todomvc: bad metafile: " ^ message);
        exit 1
  in
  let script =
    match Silica_server.Assets.resolve assets "client_main.js" with
    | Some script ->
        script
    | None ->
        prerr_endline "silica todomvc: bad metafile: no client_main.js";
        exit 1
  in
  let read name =
    if List.mem name Todomvc_server.styles then
      Adapter.read_dir !styles name
    else
      Adapter.read_dir !static name
  in
  let server = Todomvc_server.create config ~assets ~read ~script in
  Adapter.run ~port:!port ~on_stop:(fun () -> Server.shutdown server) server
