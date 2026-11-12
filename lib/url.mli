(** Application URLs: path segments, query pairs and hash, percent-encoded. *)

type t = {
  path : string list;
  query : (string * string) list;
  hash : string option;
}

val of_string : string -> t
(** Empty path segments are dropped. Invalid escapes are kept as written. *)

val to_string : t -> string
(** The empty path prints as ["/"]. Everything except [A-Za-z0-9-._~] is
    percent-encoded. *)

val fingerprint : t Fingerprint.t
(** The fingerprint of {!t} (see {!Fingerprint}). *)
