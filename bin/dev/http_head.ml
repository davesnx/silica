type t = {
  meth : string;
  target : string;
  version : string;
  headers : (string * string) list;
}

let max_size = 65536

let parse text =
  match String.split_on_char '\n' text with
  | [] ->
      None
  | first :: rest -> (
      let trim line =
        if String.ends_with ~suffix:"\r" line then
          String.sub line 0 (String.length line - 1)
        else
          line
      in
      match String.split_on_char ' ' (trim first) with
      | [ meth; target; version ] ->
          let headers =
            List.filter_map
              (fun line ->
                let line = trim line in
                match String.index_opt line ':' with
                | Some index ->
                    Some
                      ( String.lowercase_ascii (String.sub line 0 index),
                        String.trim
                          (String.sub line (index + 1)
                             (String.length line - index - 1)
                          )
                      )
                | None ->
                    None
              )
              rest
          in
          Some { meth; target; version; headers }
      | _ ->
          None
    )

let find_end buffer =
  let text = Buffer.contents buffer in
  let rec search index =
    if index + 4 > String.length text then
      None
    else if String.sub text index 4 = "\r\n\r\n" then
      Some (index + 4)
    else
      search (index + 1)
  in
  search 0

type read = Head of t * string * string | Closed | Too_large | Malformed

let read socket =
  let buffer = Buffer.create 1024 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    match find_end buffer with
    | Some stop -> (
        let text = Buffer.contents buffer in
        let head = String.sub text 0 stop in
        let rest = String.sub text stop (String.length text - stop) in
        match parse head with
        | Some parsed ->
            Head (parsed, head, rest)
        | None ->
            Malformed
      )
    | None -> (
        if Buffer.length buffer > max_size then
          Too_large
        else
          match Unix.read socket chunk 0 (Bytes.length chunk) with
          | 0 ->
              Closed
          | count ->
              Buffer.add_subbytes buffer chunk 0 count;
              loop ()
          | exception Unix.Unix_error _ ->
              Closed
      )
  in
  loop ()

let header head name = List.assoc_opt name head.headers

let path head =
  match String.index_opt head.target '?' with
  | Some index ->
      String.sub head.target 0 index
  | None ->
      head.target

let has_token value token =
  List.exists
    (fun part -> String.lowercase_ascii (String.trim part) = token)
    (String.split_on_char ',' value)

let is_upgrade head =
  match (header head "upgrade", header head "connection") with
  | Some upgrade, Some connection ->
      String.lowercase_ascii upgrade = "websocket"
      && has_token connection "upgrade"
  | _ ->
      false

let closing head =
  let lines =
    List.filter_map
      (fun (name, value) ->
        if name = "connection" || name = "keep-alive" then
          None
        else
          Some (name ^ ": " ^ value ^ "\r\n")
      )
      head.headers
  in
  Printf.sprintf "%s %s %s\r\n%sConnection: close\r\n\r\n" head.meth head.target
    head.version (String.concat "" lines)

let rec write_all socket text offset =
  if offset < String.length text then
    let count =
      Unix.write_substring socket text offset (String.length text - offset)
    in
    write_all socket text (offset + count)

let write socket text = write_all socket text 0

let respond socket ~status ~content_type body =
  write socket
    (Printf.sprintf
       "HTTP/1.1 %s\r\n\
        Content-Type: %s\r\n\
        Content-Length: %d\r\n\
        Cache-Control: no-store\r\n\
        Connection: close\r\n\
        \r\n\
        %s"
       status content_type (String.length body) body
    )
