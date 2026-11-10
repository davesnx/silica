type t = {
  entries : (string * string) list;
  imports : (string * string list) list;
  outputs : string list;
}

let ( let* ) = Result.bind

let extension name =
  match String.rindex_opt name '.' with
  | Some i ->
      String.sub name i (String.length name - i)
  | None ->
      ""

let stem name =
  match String.rindex_opt name '.' with
  | Some i ->
      String.sub name 0 i
  | None ->
      name

let basename = Filename.basename

let entry ~prefix (path, value) =
  match value with
  | `Assoc fields -> (
      match List.assoc_opt "entryPoint" fields with
      | Some (`String entry_point) ->
          let name = stem (basename entry_point) ^ extension path in
          Ok (Some (name, prefix ^ basename path))
      | Some _ ->
          Error (Printf.sprintf "entryPoint of %s is not a string" path)
      | None ->
          Ok None
    )
  | _ ->
      Error (Printf.sprintf "output %s is not an object" path)

let rec collect ~prefix acc = function
  | [] ->
      Ok (List.rev acc)
  | output :: rest -> (
      let* found = entry ~prefix output in
      match found with
      | None ->
          collect ~prefix acc rest
      | Some (name, public) ->
          if List.mem_assoc name acc then
            Error (Printf.sprintf "two outputs have the logical name %s" name)
          else
            collect ~prefix ((name, public) :: acc) rest
    )

let static_imports ~prefix (path, value) =
  match value with
  | `Assoc fields -> (
      match List.assoc_opt "imports" fields with
      | Some imports -> (
          match Jsonkit.classify imports with
          | `List imports ->
              Some
                ( prefix ^ basename path,
                  List.filter_map
                    (fun import ->
                      match Jsonkit.classify import with
                      | `Assoc import -> (
                          match
                            ( List.assoc_opt "path" import,
                              List.assoc_opt "kind" import
                            )
                          with
                          | Some path, Some kind -> (
                              match
                                (Jsonkit.classify path, Jsonkit.classify kind)
                              with
                              | `String path, `String "import-statement" ->
                                  Some (prefix ^ basename path)
                              | _ ->
                                  None
                            )
                          | _ ->
                              None
                        )
                      | _ ->
                          None
                    )
                    imports
                )
          | _ ->
              None
        )
      | None ->
          None
    )
  | _ ->
      None

let of_metafile ~prefix text =
  match Jsonkit.of_string text with
  | exception Jsonkit.Of_string_error msg ->
      Error ("invalid JSON: " ^ msg)
  | json -> (
      match Jsonkit.classify json with
      | `Assoc fields -> (
          match List.assoc_opt "outputs" fields with
          | Some outputs -> (
              match Jsonkit.classify outputs with
              | `Assoc outputs ->
                  let outputs =
                    List.map (fun (k, v) -> (k, Jsonkit.classify v)) outputs
                  in
                  let* entries = collect ~prefix [] outputs in
                  Ok
                    {
                      entries;
                      imports = List.filter_map (static_imports ~prefix) outputs;
                      outputs =
                        List.map
                          (fun (path, _) -> prefix ^ basename path)
                          outputs;
                    }
              | _ ->
                  Error "outputs is not an object"
            )
          | None ->
              Error "no outputs object"
        )
      | _ ->
          Error "metafile is not an object"
    )

let resolve t name = List.assoc_opt name t.entries
let all t = List.map snd t.entries
let outputs t = t.outputs

let imports t public =
  let rec visit seen = function
    | [] ->
        List.rev seen
    | path :: rest ->
        if List.mem path seen then
          visit seen rest
        else
          let direct =
            Option.value (List.assoc_opt path t.imports) ~default:[]
          in
          visit (path :: seen) (direct @ rest)
  in
  match visit [] [ public ] with _ :: found -> found | [] -> []

let is_hash_char = function 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

let is_hashed path =
  let base = stem (basename path) in
  if extension (basename path) = "" then
    false
  else
    match String.rindex_opt base '-' with
    | None ->
        false
    | Some i ->
        let hash = String.sub base (i + 1) (String.length base - i - 1) in
        i > 0 && String.length hash >= 8 && String.for_all is_hash_char hash

let content_type path =
  let utf8 mime = mime ^ "; charset=utf-8" in
  match String.lowercase_ascii (extension (basename path)) with
  | ".js" | ".mjs" ->
      utf8 "text/javascript"
  | ".css" ->
      utf8 "text/css"
  | ".html" ->
      utf8 "text/html"
  | ".json" | ".map" ->
      utf8 "application/json"
  | ".txt" ->
      utf8 "text/plain"
  | ".svg" ->
      "image/svg+xml"
  | ".png" ->
      "image/png"
  | ".jpg" | ".jpeg" ->
      "image/jpeg"
  | ".webp" ->
      "image/webp"
  | ".woff2" ->
      "font/woff2"
  | ".ico" ->
      "image/x-icon"
  | _ ->
      "application/octet-stream"

let etag content = "\"" ^ String.sub (Sha256.hex content) 0 16 ^ "\""
