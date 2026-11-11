(** Typed HTTP endpoints declared in shared code. *)

type ('i, 'o) endpoint

type error = Http of int | Decode of string | Network of string

val declare :
  path:string -> input:'i Codec.t -> output:'o Codec.t -> ('i, 'o) endpoint
(** Declare an endpoint in shared code. The client sends the input to [path]
    with POST and the server answers with the output. *)

val path : ('i, 'o) endpoint -> string
(** The URL path of the endpoint. *)

val input : ('i, 'o) endpoint -> 'i Codec.t
(** The codec of the input. *)

val output : ('i, 'o) endpoint -> 'o Codec.t
(** The codec of the output. *)

val output_id : ('i, 'o) endpoint -> 'o Type.Id.t
(** A type id made by [declare], one for each endpoint. The checker uses it to
    find the sample answers of an endpoint. *)

type request =
  | Request : ('i, 'o) endpoint * 'i -> request
      (** One call of an endpoint with its input, as {!Action.Preload} carries
          it. *)

val request : ('i, 'o) endpoint -> 'i -> request
(** [request endpoint input] is [Request (endpoint, input)]. *)
