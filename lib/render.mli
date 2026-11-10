(** Server-side HTML5 rendering. Text and attribute values are escaped, void
    elements self-close, [true] boolean attributes print the bare name, [false]
    ones are omitted, refs print as [data-silica-ref], handlers are not printed.
    An [html] root is prefixed with the doctype. A component node renders as its
    machine starts, after its props event; its commands are dropped. *)

val to_string : ?island:(string -> string option) -> 'e Node.t -> string
(** [island channel] gives the HTML that an island with [data-silica-live] set
    to [channel] and no children prints inside its element. *)

val to_buffer :
  ?island:(string -> string option) -> Buffer.t -> 'e Node.t -> unit
(** Like {!to_string}, adding the HTML to the buffer. *)
