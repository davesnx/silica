let meta_name = "silica-static"

let fnv1a text =
  let hash = ref 0x811c9dc5l in
  String.iter
    (fun c ->
      hash :=
        Int32.mul (Int32.logxor !hash (Int32.of_int (Char.code c))) 0x01000193l
    )
    text;
  Printf.sprintf "%08lx" !hash

let strip_slashes path =
  let rec go i =
    if i < String.length path && path.[i] = '/' then
      go (i + 1)
    else
      i
  in
  let i = go 0 in
  String.sub path i (String.length path - i)

let answer_file ~path ~input = strip_slashes path ^ "/" ^ fnv1a input ^ ".json"

let page_file = function
  | "/" ->
      "index.html"
  | path ->
      strip_slashes path ^ ".html"

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755
  )

let write ~out name content =
  let path = Filename.concat out name in
  try
    mkdir_p (Filename.dirname path);
    Ok
      (Out_channel.with_open_bin path (fun oc ->
           Out_channel.output_string oc content;
           Out_channel.flush oc
       )
      )
  with Sys_error message -> Error (name ^ ": " ^ message)

let get handle path : Silica_server.response =
  handle
    { Silica_server.meth = "GET"; path; query = []; headers = []; body = "" }

let csrf_meta = Str.regexp {|<meta name="silica-csrf" content="[^"]*">|}

let answers_script =
  Str.regexp
    ({|<script type="application/json" id="|} ^ Silica.Answers.script_id
   ^ {|"[^>]*>\([^<]*\)</script>|}
    )

let answers_of html =
  match Str.search_forward answers_script html 0 with
  | exception Not_found ->
      Ok []
  | _ ->
      Result.map Silica.Answers.to_list
        (Silica.Answers.of_string (Str.matched_group 1 html))

let static_page ~prefix html =
  match Str.search_forward csrf_meta html 0 with
  | exception Not_found ->
      Error "no silica-csrf meta element"
  | _ ->
      Ok
        (Str.replace_first csrf_meta
           (Printf.sprintf {|<meta name="%s" content="%s">|} meta_name prefix)
           html
        )

let ( let* ) = Result.bind

let rec each f = function
  | [] ->
      Ok ()
  | x :: rest ->
      let* () = f x in
      each f rest

let run ~handle ~pages ~not_found ~files ~prefix ~out =
  let written = ref 0 in
  let write name content =
    let* () = write ~out name content in
    Ok (incr written)
  in
  let answers = Hashtbl.create 64 in
  let page ~status ~file path =
    let r = get handle path in
    if r.status <> status then
      Error (Printf.sprintf "%s: status %d, expected %d" path r.status status)
    else
      let fail message = Error (path ^ ": " ^ message) in
      match (static_page ~prefix r.body, answers_of r.body) with
      | Error message, _ | _, Error message ->
          fail message
      | Ok html, Ok list ->
          let* () = write file html in
          each
            (fun (call, input, output) ->
              let name = answer_file ~path:call ~input in
              match Hashtbl.find_opt answers name with
              | Some (other, _) when other <> (call, input) ->
                  fail (name ^ ": two calls have the same file")
              | Some (_, previous) when previous <> output ->
                  fail (name ^ ": two pages have different answers")
              | Some _ ->
                  Ok ()
              | None ->
                  Hashtbl.replace answers name ((call, input), output);
                  Ok ()
            )
            list
  in
  let* () =
    each (fun path -> page ~status:200 ~file:(page_file path) path) pages
  in
  let* () = page ~status:404 ~file:"404.html" not_found in
  let* () =
    each
      (fun (name, (_, output)) -> write (strip_slashes prefix ^ name) output)
      (List.of_seq (Hashtbl.to_seq answers))
  in
  let* () =
    each
      (fun path ->
        let r = get handle path in
        if r.status = 200 then
          write (strip_slashes path) r.body
        else
          Error (Printf.sprintf "%s: status %d" path r.status)
      )
      files
  in
  let hashed =
    List.filter
      (fun path -> Silica_server.Assets.is_hashed (Filename.basename path))
      files
  in
  let* () =
    write "_headers"
      (String.concat ""
         (List.map
            (fun path ->
              path ^ "\n  Cache-Control: public, max-age=31536000, immutable\n"
            )
            hashed
         )
      )
  in
  Ok !written
