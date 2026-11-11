(** Encoders and decoders to JSON, identical on native and Melange. *)

type 'a t = {
  encode : 'a -> Silica_json.Json.t;
  decode : Silica_json.Json.t -> ('a, string) result;
}

val make :
  encode:('a -> Silica_json.Json.t) ->
  decode:(Silica_json.Json.t -> ('a, string) result) ->
  'a t
(** A codec from an encoder and a decoder. *)

val string : string t
(** A JSON string. *)

val int : int t
(** A JSON number with no fraction. *)

val bool : bool t
(** A JSON boolean. *)

val float : float t
(** A JSON number. *)

val unit : unit t
(** JSON [null]. *)

val list : 'a t -> 'a list t
(** A JSON array; each item goes through the item codec. *)

val option : 'a t -> 'a option t
(** [None] is JSON [null]. [Some v] is the encoding of [v], so [Some None] and
    [None] give the same JSON. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** A JSON array of two items. *)

val to_string : 'a t -> 'a -> string
(** The JSON text of the value. *)

val of_string : 'a t -> string -> ('a, string) result
(** Parse JSON text and decode it. [Error] holds the parse or decode error. *)

val constructor : string -> Silica_json.Json.t list -> Silica_json.Json.t
(** [constructor "Saved" [ json ]] is [["Saved", json]] and
    [constructor "Retry" []] is [["Retry"]]: a variant constructor as
    [[@@deriving json]] writes it. *)

val constructor_of :
  Silica_json.Json.t -> (string * Silica_json.Json.t list, string) result
(** The name and the arguments of a constructor written by {!constructor}. *)

val field : string -> 'a t -> Silica_json.Json.t -> ('a, string) result
(** Decode the field of a JSON object with the codec. *)
