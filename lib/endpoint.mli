(** The types of {!Remote}, in a module below {!Host} and {!Action}. Use them
    through {!Remote}. *)

type ('i, 'o, 'x) t
(** An endpoint with input ['i], output ['o] and application error ['x]. *)

type error =
  | Http of int  (** The server answered with this status. *)
  | Decode of string  (** The answer did not decode. *)
  | Network of string
      (** No answer: the network failed or the call timed out. *)

type ('o, 'x) answer =
  | Ok of 'o  (** The output. *)
  | Rejected of 'x
      (** The handler answered [Error x]: an application error, such as a failed
          validation. *)
  | Failed of error  (** The call did not give an output or an error. *)

val declare :
  path:string ->
  input:'i Codec.t ->
  output:'o Codec.t ->
  rejected:'x Codec.t ->
  ('i, 'o, 'x) t

val path : ('i, 'o, 'x) t -> string
val input : ('i, 'o, 'x) t -> 'i Codec.t
val output : ('i, 'o, 'x) t -> 'o Codec.t
val rejected : ('i, 'o, 'x) t -> 'x Codec.t
val answer_id : ('i, 'o, 'x) t -> ('o * 'x) Type.Id.t

type request = Request : ('i, 'o, 'x) t * 'i -> request

val error_codec : error Codec.t
val answer_codec : 'o Codec.t -> 'x Codec.t -> ('o, 'x) answer Codec.t
