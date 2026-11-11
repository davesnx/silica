module Json = Silica_json.Json

type entry = { path : string; input : string; output : string }
type t = entry list

let script_id = "silica-answers"
let empty = []
let is_empty = function [] -> true | _ -> false
let same ~path ~input entry = entry.path = path && entry.input = input

let remove table ~path ~input =
  List.filter (fun entry -> not (same ~path ~input entry)) table

let add table ~path ~input ~output =
  remove table ~path ~input @ [ { path; input; output } ]

let to_list table =
  List.map (fun entry -> (entry.path, entry.input, entry.output)) table

let find table ~path ~input =
  Option.map
    (fun entry -> entry.output)
    (List.find_opt (same ~path ~input) table)

let escape_angle buffer text =
  let rec go start =
    match String.index_from_opt text start '<' with
    | None ->
        Buffer.add_substring buffer text start (String.length text - start)
    | Some i ->
        Buffer.add_substring buffer text start (i - start);
        Buffer.add_string buffer "\\u003c";
        go (i + 1)
  in
  go 0

let to_json table =
  let string value = Json.declassify (`String value) in
  Json.declassify
    (`Assoc
       [
         ("v", Json.declassify (`Int 1));
         ( "answers",
           Json.declassify
             (`List
                (List.map
                   (fun entry ->
                     Json.declassify
                       (`Assoc
                          [
                            ("path", string entry.path);
                            ("input", string entry.input);
                            ("output", string entry.output);
                          ]
                       )
                   )
                   table
                )
             )
         );
       ]
    )

let to_script ?nonce table =
  let json = Json.to_string (to_json table) in
  let buffer = Buffer.create (String.length json + 128) in
  Buffer.add_string buffer "<script type=\"application/json\" id=\"";
  Buffer.add_string buffer script_id;
  Buffer.add_char buffer '"';
  Option.iter
    (fun value ->
      if value <> "" then (
        Buffer.add_string buffer " nonce=\"";
        Buffer.add_string buffer value;
        Buffer.add_char buffer '"'
      )
    )
    nonce;
  Buffer.add_char buffer '>';
  escape_angle buffer json;
  Buffer.add_string buffer "</script>";
  Buffer.contents buffer

let ( let* ) = Result.bind

let string_field fields name =
  match List.assoc_opt name fields with
  | Some value -> (
      match Json.classify value with
      | `String text ->
          Ok text
      | _ ->
          Error ("answers: " ^ name ^ " is not a string")
    )
  | None ->
      Error ("answers: missing " ^ name)

let entry_of_json json =
  match Json.classify json with
  | `Assoc fields ->
      let* path = string_field fields "path" in
      let* input = string_field fields "input" in
      let* output = string_field fields "output" in
      Ok { path; input; output }
  | _ ->
      Error "answers: entry is not an object"

let of_string text =
  let* json = Json.of_string text in
  match Json.classify json with
  | `Assoc fields -> (
      match Option.map Json.classify (List.assoc_opt "answers" fields) with
      | Some (`List entries) ->
          List.fold_left
            (fun acc entry ->
              let* table = acc in
              let* entry = entry_of_json entry in
              Ok (table @ [ entry ])
            )
            (Ok empty) entries
      | _ ->
          Error "answers: missing list"
    )
  | _ ->
      Error "answers: not an object"
