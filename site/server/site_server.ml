module Server = Silica_example_adapter.Server

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

let create config ~assets ~read () =
  Server.create config ~assets ~read ~handlers ~islands:[] ~topics:[]
    ~page:(fun (page : Server.page) ->
      Server.render_page_settled
        (Site_app.make
           (Site_theme.choice
              (Silica_server.cookie page.context.request
                 Silica_ui.Theme.cookie_name
              )
           )
        )
        ~status:Site_app.status
        ~scripts:[ page.asset "client_main.js" ]
        ~preload:(page.imports (page.asset "client_main.js"))
        ~head:
          (Printf.sprintf "<link rel=\"stylesheet\" href=\"%s\">"
             (page.asset "app.css")
          )
        ~csrf:page.csrf ~nonce:page.nonce ~islands:[] ~handlers
        ~context:page.context page.url
    )
    ()
