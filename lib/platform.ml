type cancel = unit -> unit
type reply = { status : int; body : string }
type socket = { send : string -> unit; close : int -> unit }
type socket_events = {
  on_open : unit -> unit;
  on_message : string -> unit;
  on_close : int -> unit;
  on_error : unit -> unit;
}
type io = {
  schedule : int -> (unit -> unit) -> cancel;
  post : path:string -> body:string -> ((reply, string) result -> unit) -> unit;
  connect : socket_events -> socket;
  csrf : unit -> string;
  session_gone : unit -> unit;
  log : string -> unit;
}
type 'e t = {
  renderer : on_route:(Router.event -> 'e) option -> 'e Renderer.make;
  location : unit -> Url.t;
  io : io;
}
