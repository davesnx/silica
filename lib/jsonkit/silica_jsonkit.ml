module Json = Silica_json.Json

let rec to_silica (j : Jsonkit.t) : Json.t =
  match Jsonkit.classify j with
  | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as leaf ->
      Json.declassify leaf
  | `List l ->
      Json.declassify (`List (List.map to_silica l))
  | `Assoc l ->
      Json.declassify (`Assoc (List.map (fun (k, v) -> (k, to_silica v)) l))

let rec of_silica (j : Json.t) : Jsonkit.t =
  match Json.classify j with
  | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as leaf ->
      Jsonkit.declassify leaf
  | `List l ->
      Jsonkit.declassify (`List (List.map of_silica l))
  | `Assoc l ->
      Jsonkit.declassify (`Assoc (List.map (fun (k, v) -> (k, of_silica v)) l))

let codec ~to_json ~of_json : _ Silica.Codec.t =
  Silica.Codec.make
    ~encode:(fun x -> to_silica (to_json x))
    ~decode:(fun j ->
      match of_json (of_silica j) with
      | x ->
          Ok x
      | exception (Jsonkit.Of_json_error _ as e) ->
          Error (Printexc.to_string e)
    )
