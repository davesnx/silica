open Silica

let json ~to_json ~of_json = Silica_jsonkit.codec ~to_json ~of_json

let register () =
  Machine.register_codecs Todomvc_todos.machine
    ~props:
      (json ~to_json:Todomvc_routes.filter_to_json
         ~of_json:Todomvc_routes.filter_of_json
      )
    ~state:
      (json ~to_json:Todomvc_todos.state_to_json
         ~of_json:Todomvc_todos.state_of_json
      )
    ~event:
      (json ~to_json:Todomvc_todos.event_to_json
         ~of_json:Todomvc_todos.event_of_json
      )
    ()
