open Silica_browser

let () =
  mount_app ~root:"silica-root"
    ~lives:[ Live Example_counter.live ]
    Example_app.app
