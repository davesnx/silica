open Silica

type issues = { create : bool; view : Linear_view.t }

let list = { create = false; view = Linear_view.default }

module State = struct
  type _ t = Issues : issues t | Issue : int t | Not_found : string t

  let path : type data. data t -> data Route.t = function
    | Issues ->
        Route.(
          root
          +? map_query
               (fun (create, view) -> { create; view })
               (fun { create; view } -> (create, view))
               (bool_query "new" ~default:false &? Linear_view.query)
        )
    | Issue ->
        Route.(s "issue" / int)
    | Not_found ->
        Route.rest
end

module Router = Router.Make (State)

let preload : Router.snapshot -> Remote.request list = function
  | Router.At (State.Issue, number) ->
      [
        Remote.request Linear_model.issues_endpoint ();
        Remote.request Linear_model.comments_endpoint number;
      ]
  | Router.At (State.Issues, _) | Router.At (State.Not_found, _) ->
      []

let routes =
  Router.routes ~not_found:State.Not_found ~preload
    [ State.Issues; State.Issue; State.Not_found ]
