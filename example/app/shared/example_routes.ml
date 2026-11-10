open Silica

module State = struct
  type _ t =
    | Home : unit t
    | Item : int t
    | About : unit t
    | Edit : unit t
    | Locked : unit t
    | Not_found : string t

  let path : type data. data t -> data Route.t = function
    | Home ->
        Route.root
    | Item ->
        Route.(s "item" / int)
    | About ->
        Route.(s "about")
    | Edit ->
        Route.(s "edit")
    | Locked ->
        Route.(s "locked")
    | Not_found ->
        Route.rest
end

module Router = Router.Make (State)

let routes =
  Router.routes ~not_found:State.Not_found
    ~preload:(function
      | Router.At (State.Item, id) ->
          [ Remote.request Example_model.item_endpoint id ]
      | _ ->
          []
      )
    [
      State.Home;
      State.Item;
      State.About;
      State.Edit;
      State.Locked;
      State.Not_found;
    ]
