open Silica_browser

let app =
  mount_app ~root:(by_id "silica-root")
    ~lives:[ Live Example_counter.live ]
    Example_app.app
