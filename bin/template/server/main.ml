module Server = Silica_adapter.Server

let read_file path = In_channel.with_open_bin path In_channel.input_all

let environment name ~default = Option.value (Sys.getenv_opt name) ~default

let save _context count =
  Printf.printf "saved %d\n%!" count;
  Ok ()

let () =
  let port = int_of_string (environment "PORT" ~default:"8080") in
  let dev = Silica_server.dev_of_env () in
  let secret =
    match Silica_server.secret_of_env ~origins:Silica_server.Localhost ~dev with
    | Ok secret ->
        secret
    | Error message ->
        prerr_endline message;
        exit 1
  in
  let static =
    environment "SILICA_STATIC" ~default:"_build/default/client/dist"
  in
  let metafile =
    environment "SILICA_METAFILE" ~default:"_build/default/client/meta.json"
  in
  let assets =
    match
      Silica_server.Assets.of_metafile ~prefix:"/static/" (read_file metafile)
    with
    | Ok assets ->
        assets
    | Error message ->
        prerr_endline ("bad esbuild metafile: " ^ message);
        exit 1
  in
  let server =
    Server.create
      {
        (Silica_server.default_config ~origins:Silica_server.Localhost ~secret) with
        dev;
      }
      ~assets
      ~read:(Silica_adapter.read_dir static)
      ~handlers:[ Server.implement Api.save save ]
      ~islands:[] ~topics:[]
      ~page:(fun page ->
        Server.render_page_settled
          (fun _url -> Counter.component)
          ~status:(fun _ -> 200)
          ~scripts:[ page.asset "client_main.js" ]
          page
      )
      ()
  in
  Printf.printf "http://127.0.0.1:%d\n%!" port;
  Silica_adapter.run ~port ~on_stop:(fun () -> Server.shutdown server) server
