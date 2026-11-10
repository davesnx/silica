(** Base64 encoding (RFC 4648). *)

val encode : string -> string
(** Standard alphabet, with padding. *)

val encode_url : string -> string
(** URL-safe alphabet, without padding. *)
