(** The text of a password field, kept out of what silica prints and records.

    A person types a secret into an [<input type="password">]. The text view
    (see {!View_text}), the transition log (see {!Transition}), trace files (see
    {!Trace}) and the inspector show {!mask} in its place, so that a secret does
    not end in a golden file, a trace or a log. A key that types a character
    into a password field (see {!is_typed_key}) is also {!mask}, and its record
    has no event data.

    A text that the app builds from key events, such as in its snapshot, is not
    known to silica, and stays. *)

val mask : string
(** ["••••••••"]: eight bullets, whatever the length of the text. *)

val hide : string -> string
(** [""] stays [""]; any other text is {!mask}. *)

val is_password : tag:string -> kind:string option -> bool
(** [is_password ~tag ~kind]: an element with the tag [tag] and the [type]
    attribute [kind] is a password field: an [input] of the type ["password"],
    in any case. *)

val is_typed_key : string -> bool
(** [is_typed_key key]: the key [key], a value of [KeyboardEvent.key] such as
    ["h"] or ["Enter"], types a character. A named key, such as ["Enter"],
    ["Escape"], ["Tab"], ["Backspace"] or ["ArrowUp"], types none. *)

type t
(** The texts typed into password fields. *)

val create : unit -> t

val learn : t -> field:string -> string -> unit
(** [learn secrets ~field text]: [text] is now the text of the password field
    [field], such as an instance path and an element path. It replaces the text
    that [field] had before. *)

val scrub : t -> Silica_json.Json.t -> Silica_json.Json.t
(** The value with each string that is the text of a field of {!learn} replaced
    by {!mask}, in all lists and objects. A secret that the app changed, such as
    a hash or a part of it, does not match and stays. *)
