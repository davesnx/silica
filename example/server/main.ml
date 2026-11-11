module Adapter = Silica_adapter
module Server = Adapter.Server

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
  let item _ id =
    if id < 0 then
      Error (Printf.sprintf "no item %d" id)
    else
      Ok
        {
          Example_model.Item.id;
          title = Printf.sprintf "Item %d" id;
          price_cents = 100 * (id + 1);
          tags = [ "demo" ];
        }
  in
  let votes = Atomic.make 0 in
  let server = ref None in
  let vote _ () =
    let n = Atomic.fetch_and_add votes 1 + 1 in
    ignore
      (Server.publish (Option.get !server) Example_model.votes_topic ~key:"main"
         n
      );
    Ok n
  in
  let handlers =
    [
      Server.implement Example_model.item_endpoint item;
      Server.implement Example_model.vote_endpoint vote;
    ]
  in
  let islands = [ Server.Island Example_counter.live ] in
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
    Server.create config ~assets ~read:(Adapter.read_dir !static) ~handlers
      ~islands
      ~topics:
        [
          Server.Topic_def
            {
              topic = Example_model.votes_topic;
              init = (fun _ ~key:_ -> Ok (Atomic.get votes));
            };
        ]
      ~page:(fun (page : Server.page) ->
        Server.render_page_settled Example_app.app ~status:Example_app.status
          ~scripts:[ page.asset "client_main.js" ]
          page
      )
      ()
  in
  server := Some t;
  Adapter.run ~port:!port ~on_stop:(fun () -> Server.shutdown t) t
