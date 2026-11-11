(** The boundary between a host and the platform it paints on.

    A renderer paints views, turns platform input into events and carries out
    the platform actions and held sources. It is the only part of a browser host
    that touches the DOM. The queue of events, timers, calls, the error boundary
    and the instance tree stay in the host runtime, and are the same for any
    renderer. A renderer for another platform replaces this record and nothing
    else.

    A renderer is made for one host with a [dispatch] function: it calls
    [dispatch] for each event that platform input produces, and the host queues
    it. Rate limits and [prevent] are applied by the renderer, when it receives
    the platform input. *)

type 'e t = {
  paint : 'e Node.t -> unit;
      (** Make the screen show the view. The view has no component nodes. The
          first call may adopt a screen that the server already painted (see the
          browser renderer); later calls change only what differs. *)
  run : 'e Action.t -> unit;
      (** Carry out an action. {!Action.Remount} and {!Action.Preload} never
          reach a renderer. *)
  start_source : id:string -> 'e Action.source -> unit;
      (** Start a held resource under the id. The view is already painted. Start
          with an id that is running replaces it. *)
  stop_source : id:string -> unit;
      (** Stop the held resource. Stopping an id that is not running does
          nothing. *)
}

type 'e make = dispatch:('e -> unit) -> 'e t
(** What a host needs from a renderer: a renderer wired to [dispatch]. *)
