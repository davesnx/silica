open Silica
open Todomvc_routes

let routes =
  Silica_mcp.routes routes
    ~published:(function Todos _ -> true | Not_found _ -> false)
    ~patterns:
      (String.concat "\n"
         [
           Router.name routes (Todos All) ^ " - every todo";
           Router.name routes (Todos Active)
           ^ " - the todos that are not completed";
           Router.name routes (Todos Completed) ^ " - the completed todos";
         ]
      )

let answer request =
  (404, "The TodoMVC app has no endpoint " ^ Silica_memory.path request)

let () =
  Todomvc_codecs.register ();
  Silica_mcp.serve
    (Silica_mcp.make ~name:"silica-todomvc" ~version:"0.0.0"
       ~instructions:
         "TodoMVC, a list of todos. read shows the page: each todo is a \
          checkbox named \"Todo <number>: <text>\", checked when completed. \
          navigate opens /, /active or /completed, which filter the list; the \
          todos stay. Each todo tool sends one event to the todos machine; a \
          state that ignores the event refuses the call."
       ~answer ~routes Todomvc_tools.publications
       (Silica_memory.mount_app ~url:"/" Todomvc_app.app)
    )
