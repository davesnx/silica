open Silica_browser

let app = mount_app ~root:(by_id "silica-root") Todomvc_app.app
