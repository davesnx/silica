(** CSRF tokens tied to a session. *)

val token : secret:string -> session:string -> string
(** URL-safe Base64 of HMAC-SHA-256 of ["silica-csrf:" ^ session]. *)

val valid : secret:string -> session:string -> string -> bool
(** Compares in constant time. *)
