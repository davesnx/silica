(** A component whose state lives on the server, one instance per connection.
    The client sends encoded events and receives encoded state. *)

type ('s, 'e) t = {
  channel : string;
  state : 's Codec.t;
  event : 'e Codec.t;
  component : (unit, 's, 'e) Component.t;
}

val declare :
  channel:string ->
  state:'s Codec.t ->
  event:'e Codec.t ->
  (unit, 's, 'e) Component.t ->
  ('s, 'e) t
(** Declare a Live component in shared code. [channel] is the same on the server
    and in the browser. *)

val slot : ('s, 'e) t -> 'a Node.t
(** An island: an empty [div] with [data-silica-live] set to the channel. It has
    no handlers, so it fits any event type. Hosts never diff its children and
    connect the session while it exists. *)
