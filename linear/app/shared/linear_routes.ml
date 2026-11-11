open Silica

type issues = { create : bool; view : Linear_view.t }

let list = { create = false; view = Linear_view.default }

type route =
  | Issues of issues
      [@route
        Route.(
          root
          +? map_query
               (fun (create, view) -> { create; view })
               (fun { create; view } -> (create, view))
               (bool_query "new" ~default:false &? Linear_view.query)
        )]
  | Issue of int [@route Route.(s "issue" / int)]
  | Not_found of string [@route Route.rest]
[@@deriving routes]

let preload = function
  | Issue number ->
      [
        Remote.request Linear_model.issues_endpoint ();
        Remote.request Linear_model.comments_endpoint number;
      ]
  | Issues _ | Not_found _ ->
      []

let routes =
  Router.routes
    ~not_found:(function Not_found _ -> true | Issues _ | Issue _ -> false)
    ~preload route_routes
