open Silica

let () =
  let url = Url.of_string "/a/b%20c?x=1#h" in
  let html =
    Render.to_string (Node.element "p" [] [ Node.text (Url.to_string url) ])
  in
  let json = Codec.to_string (Codec.list Codec.int) [ 1; 2 ] in
  Js.log2 html json
