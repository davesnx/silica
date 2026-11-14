module Server = Silica_adapter.Server

let doc slug =
  match
    List.find_opt
      (fun (p : Site_doc.page) -> p.slug = slug)
      Site_docs_pages.pages
  with
  | Some page ->
      Site_doc.Page page
  | None ->
      Site_doc.Missing

let clock () =
  let t = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "%02d:%02d:%02d" t.tm_hour t.tm_min t.tm_sec

let handlers =
  [
    Server.implement Site_doc.endpoint (fun _ slug -> Ok (doc slug));
    Server.implement Tutorial_api.greeting (fun _ name -> Ok ("Hello, " ^ name));
    Server.implement Landing_save.save (fun _ _ ->
        Unix.sleepf 0.6;
        Ok (clock ())
    );
  ]

let text body : Silica_server.response =
  {
    status = 200;
    headers =
      [
        ("content-type", "text/plain; charset=utf-8");
        ("cache-control", "no-cache");
      ];
    body;
  }

let markdown = function
  | [ "llms.txt" ] ->
      Some Site_llms.index
  | [ "llms-full.txt" ] ->
      Some Site_llms.full
  | [ "docs"; file ] when Filename.check_suffix file ".md" ->
      List.assoc_opt (Filename.chop_suffix file ".md") Site_llms.pages
  | _ ->
      None

let text_files =
  "/llms.txt" :: "/llms-full.txt"
  :: List.map (fun (slug, _) -> "/docs/" ^ slug ^ ".md") Site_llms.pages

let asset assets name =
  Option.to_result ~none:("no asset named " ^ name)
    (Silica_server.Assets.resolve assets name)

let create config ~assets ~read () =
  match (asset assets "client_main.js", asset assets "app.css") with
  | Error message, _ | _, Error message ->
      Error message
  | Ok script, Ok style ->
      Ok
        (Server.create config ~assets ~read ~handlers ~islands:[] ~topics:[]
           ~page:(fun (page : Server.page) ->
             match markdown page.url.path with
             | Some body ->
                 text body
             | None ->
                 Server.render_page_settled
                   (Site_app.make
                      (Site_theme.choice
                         (Silica_server.cookie page.context.request
                            Silica_ui.Theme.cookie_name
                         )
                      )
                   )
                   ~status:Site_app.status ~scripts:[ script ]
                   ~preload:(page.imports script)
                   ~head:
                     (Printf.sprintf "<link rel=\"stylesheet\" href=\"%s\">"
                        style
                     )
                   page
           )
           ()
        )
