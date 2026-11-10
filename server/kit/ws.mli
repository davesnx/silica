(** WebSocket frames (RFC 6455), server side. One frame is one message.
    Fragmented messages are not supported. *)

val accept_key : string -> string
(** [accept_key client_key] is the Sec-WebSocket-Accept value for the
    Sec-WebSocket-Key sent by the client. *)

type message =
  | Text of string
  | Binary of string
  | Close of { code : int option; reason : string }
  | Ping of string
  | Pong of string

type error =
  | Protocol of string
  | Too_big  (** The payload length is above [max]. *)

val read_message :
  max:int -> read:(int -> string option) -> (message option, error) result
(** [read_message ~max ~read] reads one client frame. [read n] must return
    exactly [n] bytes, or [None] at end of input. The result is [Ok None] when
    the input ends at a frame boundary. The payload length is checked against
    [max] before the payload is read. Client frames must be masked. *)

val encode : message -> string
(** A server frame: FIN set, not masked. *)

val text_header : string -> string
(** The header of the server frame of [Text text]: [text_header text ^ text] is
    [encode (Text text)]. An adapter writes the header and then the text, so a
    large message that goes to many sockets is not copied for each one. *)

val encode_masked : mask:string -> message -> string
(** A client frame with a 4 byte [mask]. *)
