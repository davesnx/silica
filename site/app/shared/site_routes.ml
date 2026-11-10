open Silica

module State = struct
  type _ t =
    | Home : unit t
    | Docs : unit t
    | Doc : string t
    | Components : unit t
    | Component : string t
    | Not_found : string t

  let path : type data. data t -> data Route.t = function
    | Home ->
        Route.root
    | Docs ->
        Route.(s "docs")
    | Doc ->
        Route.(s "docs" / string)
    | Components ->
        Route.(s "components")
    | Component ->
        Route.(s "components" / string)
    | Not_found ->
        Route.rest
end

module Router = Router.Make (State)

let index_slug = "index"

let preload : Router.snapshot -> Remote.request list = function
  | Router.At (State.Docs, ()) ->
      [ Remote.request Site_doc.endpoint index_slug ]
  | Router.At (State.Doc, slug) ->
      [ Remote.request Site_doc.endpoint slug ]
  | Router.At (State.Home, ())
  | Router.At (State.Components, ())
  | Router.At (State.Component, _)
  | Router.At (State.Not_found, _) ->
      []

let routes =
  Router.routes ~not_found:State.Not_found ~preload ~chunked:[ State.Component ]
    [
      State.Home;
      State.Docs;
      State.Doc;
      State.Components;
      State.Component;
      State.Not_found;
    ]
