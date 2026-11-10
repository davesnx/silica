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

let pp_result f = function
  | Ok _ ->
      Format.fprintf f "ok"
  | Error (Remote.Http code) ->
      Format.fprintf f "http %d" code
  | Error (Remote.Network _) ->
      Format.fprintf f "network"
  | Error (Remote.Decode _) ->
      Format.fprintf f "decode"

let pp_router_event f = function
  | Silica.Router.Url_changed u ->
      Format.fprintf f "url_changed %s" (Url.to_string u)
  | Silica.Router.Preload u ->
      Format.fprintf f "preload %s" (Url.to_string u)

let router_label = function
  | Silica.Router.Url_changed _ ->
      "url changed"
  | Silica.Router.Preload _ ->
      "preload"

let all_routes =
  [
    Router.At (State.Home, ());
    Router.At (State.Item, 1);
    Router.At (State.Item, 2);
    Router.At (State.About, ());
    Router.At (State.Edit, ());
    Router.At (State.Locked, ());
    Router.At (State.Not_found, "/nowhere");
  ]

let locked_edges =
  List.concat_map
    (fun route -> [ (route, "/locked"); ("/locked", route) ])
    [ "/"; "/item/:int"; "/about"; "/edit"; "/*" ]

let router () =
  Check.app ~max_states:500_000 ~label:router_label
    ~allow_unreached:locked_edges
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

let children report =
  match report with
  | Check.Ok { children; _ } ->
      children
  | _ ->
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
  let invariants : (Example_todo.state, Example_todo.event) Check.invariant list
      =
    [
      {
        name = "todo ids unique";
        holds =
          (fun t _ ->
            let ids = List.map (fun (x : Example_todo.todo) -> x.id) t.todos in
            List.length (List.sort_uniq compare ids) = List.length ids
          );
      };
    ]
  in
  Check.component ~max_states:500_000 ~inputs:todo_inputs ~invariants
    ~label:(function
      | Example_todo.Draft _ ->
          "draft"
      | Example_todo.Add ->
          "add"
      | Example_todo.Toggle _ ->
          "toggle"
      | Example_todo.Delete _ ->
          "delete"
      )
    Example_todo.component

let pp_item_event f = function
  | Example_item.Props id ->
      Format.fprintf f "props %d" id
  | Example_item.Settled r ->
      Format.fprintf f "settled %a" pp_result r

let item_label = function
  | Example_item.Props _ ->
      "props"
  | Example_item.Settled (Ok _) ->
      "settled ok"
  | Example_item.Settled (Error _) ->
      "settled error"

let item ?(invariants = []) ~props () =
  let invariants =
    {
      Check.name = "never shows loading and an item";
      holds = (fun _ view -> not (has_loading view && has_article view));
    }
    :: invariants
  in
  Check.component ~props
    ~samples:[ Check.answers item_endpoint [ item 1; item 2 ] ]
    ~label:item_label ~invariants Example_item.component

let about () =
  Check.component
    ~label:(fun Example_about.Go_back -> "go back")
    Example_about.component

let edit () =
  Check.component
    ~inputs:(fun _ -> [ Example_edit.Draft "note"; Example_edit.Draft "" ])
    ~label:(function
      | Example_edit.Draft _ ->
          "draft"
      | Example_edit.Attempt _ ->
          "attempt"
      | Example_edit.Leave ->
          "leave"
      | Example_edit.Stay ->
          "stay"
      | Example_edit.Save ->
          "save"
      )
    Example_edit.component

let not_found ~props () =
  Check.component ~props ~label:(fun _ -> "url") Example_not_found.component

let pp_votes_event f = function
  | Example_votes.Reconnect ->
      Format.fprintf f "reconnect"
  | Example_votes.Update { seq; status } ->
      Format.fprintf f "update %d %s" seq
        ( match status with
        | Status.Idle ->
            "idle"
        | Status.Loading _ ->
            "loading"
        | Status.Ready { data; stale } ->
            Printf.sprintf "%s %d"
              ( if stale then
                  "stale"
                else
                  "ready"
              )
              data
        | Status.Failed _ ->
            "failed"
        )

let votes () =
  Check.component
    ~samples:[ Check.states votes_topic [ 3; 4 ] ]
    ~label:(function
      | Example_votes.Update _ ->
          "update"
      | Example_votes.Reconnect ->
          "reconnect"
      )
    Example_votes.component

let vote () =
  Check.component
    ~samples:[ Check.answers vote_endpoint [ 1 ] ]
    ~allow_unreached:
      [ ("failed", "idle"); ("loading", "idle"); ("ready", "idle") ]
    ~label:(fun (event : (unit, int) Remote_machine.event) ->
      match event with
      | Remote_machine.Load () ->
          "load"
      | Remote_machine.Retry ->
          "retry"
      | Remote_machine.Settled (Ok _) ->
          "settled ok"
      | Remote_machine.Settled (Error _) ->
          "settled error"
      | Remote_machine.Reset ->
          "reset"
    )
    Example_vote_button.component

let counter_bounded () =
  Check.component
    ~within:(fun n -> n <= 5)
    ~label:(function
      | Counter_event.Incr ->
          "incr"
      | Counter_event.Decr ->
          "decr"
      | Counter_event.Reset ->
          "reset"
      )
    Example_counter.component
