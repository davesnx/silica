open Silica_browser

let app =
  mount_app ~root:(by_id "silica-root")
    ~lives:(List.map (fun live -> Live live) Todomvc_live.islands)
    Todomvc_app.app

let () = Todomvc_codecs.register ()
