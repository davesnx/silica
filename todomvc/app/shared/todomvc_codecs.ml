open Silica

let register () =
  Machine.register_codecs Todomvc_todos.machine ~state:Todomvc_live.state
    ~event:Todomvc_live.event ()
