(** Codecs from a [Jsonkit] encoder and decoder, for app code that derives them
    with [[@@deriving json]]. Two libraries have this module, with the same
    name: [silica.jsonkit] for native code and [silica-jsonkit-melange] for
    Melange. Shared app code links the one that its stanza needs. *)

val codec :
  to_json:('a -> Jsonkit.t) -> of_json:(Jsonkit.t -> 'a) -> 'a Silica.Codec.t
(** The value goes through a safe copy of the JSON tree between [Jsonkit.t] and
    [Silica_json.Json.t]. A decoder that raises [Jsonkit.Of_json_error] gives
    [Error] with the text of the exception. Any other exception goes on. *)
