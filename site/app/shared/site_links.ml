open Silica
open Site_routes

let doc_route slug =
  if slug = index_slug then
    Docs
  else
    Doc slug

let doc ?hash ?current ?(attrs = []) slug children =
  Router.link ?hash ?current routes (doc_route slug) attrs children

let component ?current ?(attrs = []) slug children =
  Router.link ?current routes (Component slug) attrs children

let doc_url ?hash slug =
  Url.to_string (Router.print routes (doc_route slug))
  ^ match hash with Some h -> "#" ^ h | None -> ""
