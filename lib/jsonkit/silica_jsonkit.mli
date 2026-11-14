(** Codecs from a [Jsonkit] encoder and decoder, for app code that derives them
    with [[@@deriving json]]. Two libraries have this module, with the same
    name: [silica.jsonkit] for native code and [silica.jsonkit-melange] for
    Melange. Shared app code links the one that its stanza needs. *)

val codec :
  to_json:('a -> Jsonkit.t) -> of_json:(Jsonkit.t -> 'a) -> 'a Silica.Codec.t
(** The value goes through a safe copy of the JSON tree between [Jsonkit.t] and
    [Silica_json.Json.t]. A decoder that raises, such as with
    [Jsonkit.Of_json_error], [Failure] or [Not_found], gives [Error] with the
    text of the exception. [Out_of_memory] and [Stack_overflow] go on. *)

val to_json : 'a Silica.Codec.t -> 'a -> Jsonkit.t
(** The encoder of a codec, for a hand-written [to_json] next to derived ones.
*)

val of_json : 'a Silica.Codec.t -> Jsonkit.t -> 'a
(** The decoder of a codec. An [Error] raises [Jsonkit.Of_json_error]. *)

(** {1 Silica types in derived JSON}

    [[@@deriving json]] calls [answer_to_json] for a field of type
    [('o, 'x) Silica_jsonkit.answer], and so on. These aliases let an event or
    state type that holds answers, updates or optimistic data derive its JSON:

    {[
      type event =
        | Loaded of (Issue.t, string) Silica_jsonkit.answer
        | Updated of Issue.t Silica_jsonkit.update
      [@@deriving json]
    ]}

    The JSON is the one of {!Silica.Remote.error_codec},
    {!Silica.Remote.answer_codec}, {!Silica.Status.codec},
    {!Silica.Topic.update_codec} and {!Silica.Optimistic.codec}. *)

type error = Silica.Remote.error

val error_to_json : error -> Jsonkit.t
val error_of_json : Jsonkit.t -> error

type ('o, 'x) answer = ('o, 'x) Silica.Remote.answer

val answer_to_json :
  ('o -> Jsonkit.t) -> ('x -> Jsonkit.t) -> ('o, 'x) answer -> Jsonkit.t

val answer_of_json :
  (Jsonkit.t -> 'o) -> (Jsonkit.t -> 'x) -> Jsonkit.t -> ('o, 'x) answer

type 'a status = 'a Silica.Status.t

val status_to_json : ('a -> Jsonkit.t) -> 'a status -> Jsonkit.t
val status_of_json : (Jsonkit.t -> 'a) -> Jsonkit.t -> 'a status

type 's update = 's Silica.Topic.update

val update_to_json : ('s -> Jsonkit.t) -> 's update -> Jsonkit.t
val update_of_json : (Jsonkit.t -> 's) -> Jsonkit.t -> 's update

type ('value, 'change) optimistic = ('value, 'change) Silica.Optimistic.state

val optimistic_to_json :
  ('value -> Jsonkit.t) ->
  ('change -> Jsonkit.t) ->
  ('value, 'change) optimistic ->
  Jsonkit.t

val optimistic_of_json :
  (Jsonkit.t -> 'value) ->
  (Jsonkit.t -> 'change) ->
  Jsonkit.t ->
  ('value, 'change) optimistic
