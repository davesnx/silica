open Todomvc_todos

let ids_unique =
  Check.always ~name:"todo ids are unique"
    (Check.state (fun state ->
         let ids = List.map (fun todo -> todo.id) state.todos in
         List.length (List.sort_uniq compare ids) = List.length ids
     )
    )

let editing_exists =
  Check.always ~name:"the open todo exists"
    (Check.state (fun state ->
         match state.editing with
         | Some edit ->
             has edit.todo state
         | None ->
             true
     )
    )

let todos =
  Check.component ~texts:[ "milk"; " " ]
    ~inputs:(fun _ -> [ Add "milk"; Add " " ])
    ~within:(fun state -> state.next_id <= 3)
    ~properties:[ ids_unique; editing_exists ]
    Todomvc_live.all.component

let () =
  Todomvc_codecs.register ();
  Check_suite.main [ Check_suite.entry todos ]
