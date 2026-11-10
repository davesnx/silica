(** The answers of Remote calls that the server got while it rendered a page.
    The server writes them into the page as JSON. The client answers the same
    calls from this table before its first paint, so no call runs twice and the
    first render has the same state as the server HTML. *)

type t

val empty : t
(** A table with no answers. *)

val add : t -> path:string -> input:string -> output:string -> t
(** [input] and [output] are the encoded JSON bodies of the call (see
    {!Codec.to_string}). Adding the same [path] and [input] again replaces the
    answer. *)

val is_empty : t -> bool
(** [true] when the table has no answers. *)

val find : t -> path:string -> input:string -> string option
(** The encoded output of a call. *)

val to_list : t -> (string * string * string) list
(** The answers as [(path, input, output)], in the order they were added. *)

val remove : t -> path:string -> input:string -> t
(** The client uses each answer once. *)

val to_script : ?nonce:string -> t -> string
(** An HTML element [<script type="application/json" id="silica-answers">] with
    the table as JSON. The character [<] is written as [<], so the text cannot
    end the script element or start a comment. [nonce] becomes the [nonce]
    attribute. *)

val script_id : string
(** ["silica-answers"]. *)

val of_string : string -> (t, string) result
(** Read the JSON text that {!to_script} put into the element. *)
