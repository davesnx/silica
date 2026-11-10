(** A keyboard event as a portable value. Hosts build it from the platform
    event; machines and the checker match on it. *)

type t = {
  key : string;  (** The value of [KeyboardEvent.key], such as ["Escape"]. *)
  ctrl : bool;
  meta : bool;
  shift : bool;
  alt : bool;
  repeat : bool;
}

val make :
  ?ctrl:bool ->
  ?meta:bool ->
  ?shift:bool ->
  ?alt:bool ->
  ?repeat:bool ->
  string ->
  t
(** A key with no modifier unless given. *)

val escape : t
(** The Escape key with no modifier. *)

val enter : t
(** The Enter key with no modifier. *)

val tab : t
(** The Tab key with no modifier. *)

val arrow_up : t
(** The Up arrow key with no modifier. *)

val arrow_down : t
(** The Down arrow key with no modifier. *)

val equal : t -> t -> bool
(** Two keys are equal when the key name and every modifier are equal. *)

val to_string : t -> string
(** ["Ctrl+Shift+k"] style text for traces and logs. *)
