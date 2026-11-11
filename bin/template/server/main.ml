module Server = Silica_adapter.Server

let read_file path = In_channel.with_open_bin path In_channel.input_all

let environment name ~default = Option.value (Sys.getenv_opt name) ~default

let save _context count =
  Printf.printf "saved %d\n%!" count;
  Ok ()

let () =
  let port = int_of_string (environment "PORT" ~default:"8080") in
  let secret =
    environment "SILICA_SECRET"
      ~default:(Silica_server_kit.Random_source.hex_id ~bytes:32)
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
      (Silica_server.default_config ~origins:Silica_server.Localhost ~secret)
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
  Silica_adapter.run ~port server
