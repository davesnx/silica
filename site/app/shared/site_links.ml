open Silica
open Site_routes

let doc ?hash ?current ?(attrs = []) slug children =
  if slug = index_slug then
    Router.link ?hash ?current routes State.Docs () attrs children
  else
    Router.link ?hash ?current routes State.Doc slug attrs children

let component ?current ?(attrs = []) slug children =
  Router.link ?current routes State.Component slug attrs children

let doc_url ?hash slug =
  let url =
    if slug = index_slug then
      Router.print routes (Router.At (State.Docs, ()))
    else
      Router.print routes (Router.At (State.Doc, slug))
  in
  Url.to_string url ^ match hash with Some h -> "#" ^ h | None -> ""
