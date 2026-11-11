open Silica

type route =
  | Home [@route Route.root]
  | Docs [@route Route.(s "docs")]
  | Doc of string [@route Route.(s "docs" / string)]
  | Components [@route Route.(s "components")]
  | Component of string [@route Route.(s "components" / string)]
  | Not_found of string [@route Route.rest]
[@@deriving routes]

let index_slug = "index"

let preload = function
  | Docs ->
      [ Remote.request Site_doc.endpoint index_slug ]
  | Doc slug ->
      [ Remote.request Site_doc.endpoint slug ]
  | Home | Components | Component _ | Not_found _ ->
      []

let routes =
  Router.routes
    ~not_found:(function Not_found _ -> true | _ -> false)
    ~preload
    ~chunked:(function Component _ -> true | _ -> false)
    route_routes
