open Silica

type route =
  | Home [@route Route.root]
  | Item of int [@route Route.(s "item" / int)]
  | About [@route Route.(s "about")]
  | Edit [@route Route.(s "edit")]
  | Locked [@route Route.(s "locked")]
  | Not_found of string [@route Route.rest]
[@@deriving routes]

let routes =
  Router.routes
    ~not_found:(function Not_found _ -> true | _ -> false)
    ~preload:(function
      | Item id -> [ Remote.request Example_model.item_endpoint id ] | _ -> []
      )
    route_routes
