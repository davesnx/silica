module Server = Silica_adapter.Server

let styles = [ "base.css"; "index.css" ]

let head =
  String.concat ""
    (List.map
       (fun name ->
         Printf.sprintf {|<link rel="stylesheet" href="/static/%s">|} name
       )
       styles
    )

let create config ~assets ~read ~script =
  Server.create config ~assets ~read ~handlers:[] ~islands:[] ~topics:[]
    ~page:(fun (page : Server.page) ->
      Server.render_page_settled Todomvc_app.app ~status:Todomvc_app.status
        ~scripts:[ script ] ~head page
    )
    ()
