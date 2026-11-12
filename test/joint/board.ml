open Silica

let element = Node.element
let text = Node.text
let id value = Attr.string "id" value

type state = {
  items : int list;
  label : int;
  last : int option;
  finished : int list;
  job : bool;
}

type event =
  | Add
  | Remove_first
  | Reverse
  | Relabel
  | Toggle_job
  | Bumped of int
  | Done of int

let free items = List.find_opt (fun key -> not (List.mem key items)) [ 1; 2 ]

let machine =
  Machine.make ~id:"board"
    ~init:{ items = [ 1 ]; label = 0; last = None; finished = []; job = true }
    ~name:(fun _ -> "board")
    ~states:[ "board" ]
    ~event_name:(function
      | Add ->
          "add"
      | Remove_first ->
          "remove first"
      | Reverse ->
          "reverse"
      | Relabel ->
          "relabel"
      | Toggle_job ->
          "toggle job"
      | Bumped _ ->
          "bumped"
      | Done _ ->
          "done"
      )
    (fun state event ->
      match event with
      | Add -> (
          match free state.items with
          | Some key ->
              Go { state with items = state.items @ [ key ] }
          | None ->
              Ignore
        )
      | Remove_first -> (
          match state.items with
          | _ :: rest ->
              Go { state with items = rest }
          | [] ->
              Ignore
        )
      | Reverse ->
          Go { state with items = List.rev state.items }
      | Relabel ->
          Go { state with label = 1 - state.label }
      | Toggle_job ->
          Go { state with job = not state.job }
      | Bumped key ->
          Go { state with last = Some key }
      | Done key ->
          Go
            {
              state with
              finished = List.sort_uniq compare (key :: state.finished);
            }
    )

let button name event =
  element "button" [ id name; Attr.on_press event ] [ text name ]

let view state =
  element "main" []
    [
      element "p"
        [ id "board-state" ]
        [
          text
            (Printf.sprintf "label %d last %s finished %s" state.label
               ( match state.last with
               | Some key ->
                   string_of_int key
               | None ->
                   "none"
               )
               (String.concat "," (List.map string_of_int state.finished))
            );
        ];
      ( if free state.items <> None then
          button "add" Add
        else
          Node.list []
      );
      ( if state.items <> [] then
          button "remove-first" Remove_first
        else
          Node.list []
      );
      ( if List.length state.items = 2 then
          button "reverse" Reverse
        else
          Node.list []
      );
      button "relabel" Relabel;
      button "toggle-job" Toggle_job;
      element "ul"
        [ id "rows" ]
        (List.map
           (fun key ->
             Component.node_with_props ~key:(string_of_int key)
               ~props:{ Row.key; label = state.label }
               ~output:(fun Row.Bump _ -> [ Bumped key ])
               ~on_final:(fun _ -> [ Done key ])
               Row.component
           )
           state.items
        );
      ( if state.job then
          Component.node Job.component
        else
          Node.list []
      );
    ]

let component = Component.make machine view
