module Adapter = Silica_example_adapter
module Server = Adapter.Server

let read_file path = In_channel.with_open_bin path In_channel.input_all

let greeting _context name = Ok ("Hello, " ^ name)

let () =
  let handlers = [ Server.implement Tutorial_api.greeting greeting ] in
  let assets =
    match
      Silica_server.Assets.of_metafile ~prefix:"/static/"
        (read_file "_build/default/client/meta.json")
    with
    | Ok assets ->
        assets
    | Error message ->
        failwith message
  in
  let config =
    Silica_server.default_config ~origins:Silica_server.Localhost
      ~secret:(Silica_server_kit.Random_source.hex_id ~bytes:32)
  in
  let server =
    Server.create config ~assets
      ~read:(Adapter.read_dir "_build/default/client/dist")
      ~handlers ~islands:[] ~topics:[]
      ~page:(fun (page : Server.page) ->
        Server.render_page_settled
          (fun _url -> Tutorial_greeting.component)
          ~status:(fun _ -> 200)
          ~scripts:[ page.asset "tutorial_client.js" ]
          ~csrf:page.csrf ~nonce:page.nonce ~handlers ~context:page.context
          page.url
      )
      ()
  in
  Adapter.run ~port:8080 server
