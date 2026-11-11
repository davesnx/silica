open Silica
open Example_model
open Example_routes

let item n =
  {
    Item.id = n;
    title = "Item " ^ string_of_int n;
    price_cents = 1999;
    tags = [ "a" ];
  }

let rec exists predicate n =
  predicate (Node.view n)
  ||
  match Node.view n with
  | Node.Element (_, _, children) | Node.List children ->
      List.exists (exists predicate) children
  | Node.Keyed (_, child) ->
      exists predicate child
  | Node.Portal (_, children) ->
      List.exists (exists predicate) children
  | Node.Boundary { body; _ } ->
      exists predicate body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      false

let rec text n =
  match Node.view n with
  | Node.Text t ->
      t
  | Node.Element (_, _, children) | Node.List children ->
      String.concat "" (List.map text children)
  | Node.Keyed (_, child) ->
      text child
  | Node.Portal (_, children) ->
      String.concat "" (List.map text children)
  | Node.Boundary { body; _ } ->
      text body
  | Node.Component _ | Node.Chunk _ ->
      ""

let has_article view =
  exists (function Node.Element ("article", _, _) -> true | _ -> false) view

let has_loading view =
  exists
    (function
      | Node.Element ("p", _, children) ->
          let t = String.concat "" (List.map text children) in
          String.length t >= 7 && String.sub t 0 7 = "Loading"
      | _ ->
          false
      )
    view

let pp_answer f : (_, string) Remote.answer -> unit = function
  | Ok _ ->
      Format.fprintf f "ok"
  | Rejected reason ->
      Format.fprintf f "rejected %s" reason
  | Failed (Remote.Http code) ->
      Format.fprintf f "http %d" code
  | Failed (Remote.Network _) ->
      Format.fprintf f "network"
  | Failed (Remote.Decode _) ->
      Format.fprintf f "decode"

let all_routes =
  [ Home; Item 1; Item 2; About; Edit; Locked; Not_found "/nowhere" ]

let router () =
  Check.app ~max_states:500_000
    ~inputs:(fun _ ->
      List.map
        (fun path -> Silica.Router.Url_changed (Url.of_string path))
        [ "/"; "/item/1"; "/item/2"; "/about"; "/edit"; "/locked"; "/nowhere" ]
    )
    ~routes:all_routes ~parse:(Router.parse routes) ~print:(Router.print routes)
    ~is_not_found:(Router.is_not_found routes)
    ~url_event:(fun u -> Silica.Router.Url_changed u)
    Example_app.app

let home () = Check.component ~max_states:10 Example_home.component

let children (report : (_, _) Check.report) =
  if Check.passed report then
    report.children
  else
    failwith "the parent check did not pass"

let todo_inputs (t : Example_todo.state) =
  ( if t.next_id < 3 then
      [ Example_todo.Draft "milk" ]
    else
      []
  )
  @
  if t.draft <> "" then
    [ Example_todo.Draft "" ]
  else
    []

let todo () =
  let properties =
    [
      Check.always ~name:"todo ids unique"
        (Check.state (fun (t : Example_todo.state) ->
             let ids = List.map (fun (x : Example_todo.todo) -> x.id) t.todos in
             List.length (List.sort_uniq compare ids) = List.length ids
         )
        );
    ]
  in
  Check.component ~max_states:500_000 ~inputs:todo_inputs ~properties
    Example_todo.component

let pp_item_event f (Example_item.Settled answer) =
  Format.fprintf f "settled %a" pp_answer answer

let pp_item_input f : (_, Example_item.event) Check.input -> unit = function
  | Event event ->
      pp_item_event f event
  | Props _ ->
      Format.pp_print_string f "new props"

let item ?(properties = []) ~props () =
  let properties =
    Check.always ~name:"never shows loading and an item"
      (Check.view (fun view -> not (has_loading view && has_article view)))
    :: properties
  in
  Check.component ~props
    ~samples:
      [ Check.answers item_endpoint ~rejected:[ "no item" ] [ item 1; item 2 ] ]
    ~properties Example_item.component

let about () = Check.component Example_about.component

let edit () =
  Check.component
    ~inputs:(fun _ -> [ Example_edit.Draft "note"; Example_edit.Draft "" ])
    Example_edit.component

let not_found () = Check.component Example_not_found.component

let votes () =
  Check.component
    ~samples:[ Check.states votes_topic [ 3; 4 ] ]
    Example_votes.component

let vote () =
  Check.component
    ~samples:[ Check.answers vote_endpoint ~rejected:[ "closed" ] [ 1 ] ]
    Example_vote_button.component

let counter_bounded () =
  Check.component ~within:(fun count -> count <= 5) Example_counter.component
