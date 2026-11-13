(** A sign-in form with a password field, to test that a typed secret does not
    show in views, traces or the inspector. *)

type state = { password : string; sent : bool }
type event = Typed of string | Send

val machine : (unit, state, event) Silica.Machine.t
(** Named ["empty"], ["typed"] and ["sent"]; the snapshot and the events have
    codecs (see {!register}). *)

val register : unit -> unit
(** Registers the codecs of {!machine}. *)

val component : (unit, state, event) Silica.Component.t
(** A [<input type="password">] with the id ["password"] and the label
    ["Password"], and a button ["Send"] when the field is not empty. *)
