open Silica
open Todomvc_routes

let codec ~to_json ~of_json = Silica_jsonkit.codec ~to_json ~of_json

let state =
  codec ~to_json:Todomvc_todos.state_to_json
    ~of_json:Todomvc_todos.state_of_json

let event =
  codec ~to_json:Todomvc_todos.event_to_json
    ~of_json:Todomvc_todos.event_of_json

let declare channel filter =
  Live.declare ~channel ~state ~event
    (Component.make Todomvc_todos.machine (Todomvc_todos.view filter))

let all = declare "todos" All
let active = declare "todos.active" Active
let completed = declare "todos.completed" Completed
let island = function All -> all | Active -> active | Completed -> completed
let islands = [ all; active; completed ]
