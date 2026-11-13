open Silica

type route =
  | Home [@route Route.root]
  | Issue of int [@route Route.(s "issue" / int)]
  | Pair of int * string [@route Route.(s "pair" / int /+ string)]
  | Not_found of string [@route Route.rest]
[@@deriving routes]

type t = Only [@route Route.root] [@@deriving routes]

let round_trip routes route =
  if Router.parse routes (Router.print routes route) <> Ok route then
    failwith (Url.to_string (Router.print routes route))

let () =
  let table = Router.routes route_routes in
  List.iter (round_trip table)
    [ Home; Issue 4; Pair (2, "x"); Not_found "/missing/path" ];
  assert (Url.to_string (Router.print table (Issue 4)) = "/issue/4");
  assert (Router.parse table (Url.of_string "/pair/3/y") = Ok (Pair (3, "y")));
  round_trip (Router.routes routes) Only
