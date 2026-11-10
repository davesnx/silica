(** Names for nodes that commands can target (focus, blur, scroll). *)

type t

val make : string -> t
(** A ref with this name. Use a name that is unique in the page. The host writes
    it to the [data-silica-ref] attribute. *)

val name : t -> string
(** The name given to {!make}. *)
