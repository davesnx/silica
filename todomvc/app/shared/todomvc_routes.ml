open Silica

type filter = All | Active | Completed [@@deriving json]
type route = Todos of filter | Not_found of string

let filter_route path filter =
  Router.route path
    (fun () -> Todos filter)
    (function
      | Todos other when other = filter ->
          Some ()
      | Todos _ | Not_found _ ->
          None
      )

let routes =
  Router.routes
    ~not_found:(function Not_found _ -> true | Todos _ -> false)
    [
      filter_route Route.root All;
      filter_route Route.(s "active") Active;
      filter_route Route.(s "completed") Completed;
      Router.route Route.rest
        (fun path -> Not_found path)
        (function Not_found path -> Some path | Todos _ -> None);
    ]
