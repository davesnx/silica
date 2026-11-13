(** A password field with an input handler and key handlers that give an event
    for every key, to test that a key typed into a password field does not show
    in traces or the inspector. *)

type state = { password : string; keys : int; sent : bool }

type event =
  | Typed of string
  | Pressed of string
      (** The value of [KeyboardEvent.key]: Enter sends, any other key counts.
      *)

val machine : (unit, state, event) Silica.Machine.t
(** Named ["empty"], ["typed"] and ["sent"]; the snapshot and the events have
    codecs (see {!register}). *)

val register : unit -> unit
(** Registers the codecs of {!machine}. *)

val component : (unit, state, event) Silica.Component.t
(** A [<input type="password">] with the id ["password"] and the label
    ["Password"]. Its [on_key_down] and [on_key_up] give [Pressed] with the key.
*)
