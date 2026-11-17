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

let mcp todos =
  let snapshot () = Server.snapshot todos in
  Silica_mcp.make ~name:"silica-todomvc" ~version:"0.0.0"
    ~instructions:Todomvc_tools.instructions ~routes:Todomvc_tools.routes
    Todomvc_tools.publications
    (Silica_mcp.server ~app:Todomvc_app.app
       ~islands:
         (List.map
            (fun live -> Silica_mcp.island live snapshot)
            Todomvc_live.islands
         )
       [
         Silica_mcp.instance Todomvc_todos.machine ~path:"shared" ~props:()
           ~snapshot ~send:(Server.send todos);
       ]
    )

let create config ~assets ~read ~script =
  let todos = Server.share Todomvc_live.all in
  let mcp = mcp todos in
  Server.create config ~assets ~read ~handlers:[]
    ~islands:
      (List.map (fun live -> Server.Shared (live, todos)) Todomvc_live.islands)
    ~topics:[] ~mcp:(Silica_mcp.http mcp)
    ~page:(fun (page : Server.page) ->
      Server.render_page_settled Todomvc_app.app ~status:Todomvc_app.status
        ~scripts:[ script ] ~head page
    )
    ()
