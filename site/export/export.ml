module Adapter = Silica_example_adapter

let read_file path = In_channel.with_open_bin path In_channel.input_all

let pages =
  [ "/" ]
  @ List.map
      (fun (slug, _, _) ->
        if slug = Site_routes.index_slug then
          "/docs"
        else
          "/docs/" ^ slug
      )
      Site_docs_index.pages
  @ [ "/components" ]
  @ List.map
      (fun (e : Site_components.info) -> "/components/" ^ e.slug)
      Site_components.all

let () =
  let out = ref "_build/site-static" in
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
  Arg.parse
    [
      ( "--out",
        Arg.Set_string out,
        "directory to write (default _build/site-static)"
      );
      ( "--static",
        Arg.Set_string static,
        "directory of the client build (env SILICA_STATIC)"
      );
      ( "--metafile",
        Arg.Set_string metafile,
        "esbuild metafile (env SILICA_METAFILE)"
      );
    ]
    ignore "Write the Silica site as static files";
  let assets =
    match
      Silica_server.Assets.of_metafile ~prefix:"/static/" (read_file !metafile)
    with
    | Ok assets ->
        assets
    | Error message ->
        prerr_endline ("export: bad metafile: " ^ message);
        exit 1
  in
  let config =
    Silica_server.default_config ~origins:Silica_server.Localhost
      ~secret:(Silica_server_kit.Random_source.hex_id ~bytes:32)
  in
  let server =
    Site_server.create config ~assets ~read:(Adapter.read_dir !static) ()
  in
  match
    Silica_static_export.run
      ~handle:(Adapter.Server.handle server)
      ~pages ~not_found:"/404"
      ~files:(Silica_server.Assets.outputs assets)
      ~prefix:"/silica-answers/" ~out:!out
  with
  | Ok count ->
      Printf.printf "export: %d files in %s\n" count !out
  | Error message ->
      prerr_endline ("export: " ^ message);
      exit 1
