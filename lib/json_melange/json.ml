type t = Jsonkit.t

type view =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `List of t list
  | `Assoc of (string * t) list ]

let classify = Jsonkit.classify
let declassify = Jsonkit.declassify
let to_string = Jsonkit.to_string

let of_string s =
  match Jsonkit.of_string s with
  | json ->
      Ok json
  | exception Jsonkit.Of_string_error msg ->
      Error msg
