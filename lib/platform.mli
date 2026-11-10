(** The contract between a host runtime ({!Runtime}) and the platform it runs
    on: a browser, or the in-memory platform of the tests.

    The runtime keeps the queue of events, the instance tree, timers, calls,
    subscriptions, the error page and the replay of server answers. It reaches
    the platform only through a value of {!t}:

    - {!field-renderer} paints views, adopts a server-painted screen, turns
      platform input into events (element handlers and held sources), runs the
      platform commands of {!Cmd.t} (focus, scroll, history, title, ...) and
      paints portals. See {!Renderer}.
    - {!field-location} and the [on_route] argument of {!field-renderer} are the
      navigation part: the URL at the start, each URL change (a navigation
      command, a link press, back and forward) and each preload of a link, which
      the runtime gives to the root component.
    - {!field-io} holds what the runtime itself needs: a clock, one HTTP request
      form for Remote calls, a socket for Live subscriptions, and a log.

    A platform for a new screen writes one value of this type and no runtime
    code. *)

type cancel = unit -> unit
(** Stops what was started. Calling it again, or after the end, does nothing. *)

type reply = { status : int; body : string }
(** An HTTP answer. *)

type socket = { send : string -> unit; close : int -> unit }
(** An open Live socket: send a text message, close with a code. *)

type socket_events = {
  on_open : unit -> unit;
  on_message : string -> unit;
  on_close : int -> unit;  (** The close code, [0] when there is none. *)
  on_error : unit -> unit;
}
(** What a socket reports. A platform calls [on_message] with text: binary
    frames are decoded as UTF-8. It never calls a handler inside {!io.connect}.
*)

type io = {
  schedule : int -> (unit -> unit) -> cancel;
      (** Run the function after the milliseconds, unless cancelled first. It
          never runs it at once, inside the call. *)
  post : path:string -> body:string -> ((reply, string) result -> unit) -> unit;
      (** Send a Remote call: a POST of the JSON body to the path of the page
          origin, with the CSRF token of the page. [Error] is a network failure
          with a message. *)
  connect : socket_events -> socket;
      (** Open the Live socket of the page origin ([/_silica/live]). *)
  csrf : unit -> string;  (** The CSRF token that the server gave the page. *)
  session_gone : unit -> unit;
      (** The server no longer knows the session (see
          {!Protocol.close_session}). A browser reloads the page once. *)
  log : string -> unit;  (** An error for the developer, such as the console. *)
}

type 'e t = {
  renderer : on_route:(Router.event -> 'e) option -> 'e Renderer.make;
      (** Make the renderer. With [on_route], each URL change raises the event
          that [on_route] makes from {!Router.event.Url_changed}, and each
          preload of a link the event made from {!Router.event.Preload}. *)
  location : unit -> Url.t;  (** The URL of the page now. *)
  io : io;
}
