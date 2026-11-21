(** JSON values. The implementation is [Jsonkit.t] on both native and Melange,
    selected by dune through the virtual library [silica.json]. *)

type t

type view =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `List of t list
  | `Assoc of (string * t) list ]

val classify : t -> view

val declassify : view -> t

val to_string : t -> string

val of_string : string -> (t, string) result
