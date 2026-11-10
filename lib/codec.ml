module Json = Silica_json.Json

type 'a t = { encode : 'a -> Json.t; decode : Json.t -> ('a, string) result }

let make ~encode ~decode = { encode; decode }
let mismatch what = Error ("expected " ^ what)

let string =
  {
    encode = (fun s -> Json.declassify (`String s));
    decode =
      (fun j ->
        match Json.classify j with `String s -> Ok s | _ -> mismatch "string"
      );
  }

let int =
  {
    encode = (fun i -> Json.declassify (`Int i));
    decode =
      (fun j ->
        match Json.classify j with `Int i -> Ok i | _ -> mismatch "int"
      );
  }

let bool =
  {
    encode = (fun b -> Json.declassify (`Bool b));
    decode =
      (fun j ->
        match Json.classify j with `Bool b -> Ok b | _ -> mismatch "bool"
      );
  }

let float =
  {
    encode = (fun f -> Json.declassify (`Float f));
    decode =
      (fun j ->
        match Json.classify j with
        | `Float f ->
            Ok f
        | `Int i ->
            Ok (Float.of_int i)
        | _ ->
            mismatch "float"
      );
  }

let unit =
  {
    encode = (fun () -> Json.declassify `Null);
    decode =
      (fun j ->
        match Json.classify j with `Null -> Ok () | _ -> mismatch "null"
      );
  }

let list c =
  {
    encode = (fun l -> Json.declassify (`List (List.map c.encode l)));
    decode =
      (fun j ->
        match Json.classify j with
        | `List l ->
            List.fold_right
              (fun x acc ->
                match (c.decode x, acc) with
                | Ok x, Ok acc ->
                    Ok (x :: acc)
                | Error e, _ | _, Error e ->
                    Error e
              )
              l (Ok [])
        | _ ->
            mismatch "array"
      );
  }

let option c =
  {
    encode = (function None -> Json.declassify `Null | Some x -> c.encode x);
    decode =
      (fun j ->
        match Json.classify j with
        | `Null ->
            Ok None
        | _ ->
            Result.map Option.some (c.decode j)
      );
  }

let pair a b =
  {
    encode = (fun (x, y) -> Json.declassify (`List [ a.encode x; b.encode y ]));
    decode =
      (fun j ->
        match Json.classify j with
        | `List [ x; y ] -> (
            match (a.decode x, b.decode y) with
            | Ok x, Ok y ->
                Ok (x, y)
            | Error e, _ | _, Error e ->
                Error e
          )
        | _ ->
            mismatch "pair"
      );
  }

let to_string c v = Json.to_string (c.encode v)

let of_string c s =
  match Json.of_string s with Error e -> Error e | Ok j -> c.decode j
