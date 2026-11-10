(** Typed conversion between URLs and the data of a route: path segments and a
    typed query with defaults. A route is built from {!s}, {!int}, {!string},
    {!rest} and the operators, for example [s "issues" / int // s "edit"]. One
    value gives both parse and print. *)

type 'a t
(** A pattern for the data ['a]. *)

val s : string -> unit t
(** One literal path segment. *)

val root : unit t
(** The empty path. *)

val int : int t
(** One path segment that prints as an integer: ["007"] and ["+7"] do not match.
*)

val string : string t
(** One path segment. *)

val rest : string t
(** The whole remaining path, as a string such as ["/a/b"]. It matches every
    path, so a route that uses it, placed last, is the not-found route. *)

val ( / ) : unit t -> 'a t -> 'a t
(** A literal segment first, then the pattern. *)

val ( // ) : 'a t -> unit t -> 'a t
(** The pattern, then a literal segment. *)

val ( /+ ) : 'a t -> 'b t -> ('a * 'b) t
(** Two patterns in a row. *)

type 'a query
(** Typed query parameters with defaults. *)

val query :
  name:string ->
  default:'a ->
  parse:(string -> 'a option) ->
  print:('a -> string) ->
  'a query
(** A value that [parse] cannot read falls back to [default]. A value equal to
    [default] is not printed. *)

val string_query : string -> default:string -> string query
(** A text query parameter. *)

val int_query : string -> default:int -> int query
(** An integer query parameter. Text that is not an integer gives [default]. *)

val bool_query : string -> default:bool -> bool query
(** A boolean query parameter, ["true"] or ["false"]. *)

val choice_query : string -> default:'a -> (string * 'a) list -> 'a query
(** A value from a fixed list of names. *)

val map_query : ('a -> 'b) -> ('b -> 'a) -> 'a query -> 'b query
(** [map_query read write query]: the same keys with the value turned into
    another type, such as a record of the values of {!( &? )}. [write (read v)]
    must equal [v]. *)

val ( &? ) : 'a query -> 'b query -> ('a * 'b) query
(** Two query parameters. They print in this order, which is the canonical one.
*)

val with_query : 'a t -> 'q query -> ('a * 'q) t
(** The path pattern with a query. Query keys that the route does not know are
    ignored and not printed. *)

val ( +? ) : unit t -> 'q query -> 'q t
(** A path with no data, such as [s "issues"], with a query: the data is the
    query. *)

val parse : 'a t -> Url.t -> 'a option
(** [None] when the path does not match. The hash is ignored. *)

val print : 'a t -> 'a -> Url.t
(** The canonical URL: default query values are left out, there is no hash. *)

val describe : 'a t -> string
(** A name for the pattern, such as ["/issues/:int"]. *)
