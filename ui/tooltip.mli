(** A short text that describes a button (WAI-ARIA tooltip pattern; Base UI
    Tooltip).

    The tooltip opens when the pointer rests on the trigger for [delay], or at
    once when the trigger gets focus while the pointer is not waiting on it (a
    mouse press focuses the trigger, and must not show the tooltip). It closes
    [close_delay] after the pointer leaves the trigger and the popup, and at
    once on blur, Escape or a press on the trigger. While it closes, the pointer
    can move onto the popup and keep it open. The popup is painted in a portal
    at the end of the body, next to the trigger. It never takes focus. While it
    is open, the trigger is described by it ([aria-describedby]).

    Styling: classes per part ({!type-classes}), [data-open] and [data-closed]
    on the trigger and the popup, [data-popup-open] on the trigger, and the
    [data-side] and [data-align] attributes that the renderer writes on the
    popup. *)

type classes = { trigger : string; popup : string }

val no_classes : classes
(** All empty. *)

(** A delay of the tooltip. A timer of a state has a fixed duration, so the
    delays are a fixed set. *)
type delay =
  | Instant  (** 0 ms: at once. *)
  | Short  (** 300 ms. *)
  | Medium  (** 600 ms. *)
  | Long  (** 1000 ms. *)

val delay_ms : delay -> int
(** The delay in milliseconds. *)

type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The text of the trigger button, its accessible name. *)
  tip : string;  (** The text of the tooltip. *)
  delay : delay;  (** From the pointer on the trigger to the open tooltip. *)
  close_delay : delay;
      (** From the pointer out of the trigger and the popup to the closed
          tooltip. *)
  place : Silica.Place.t;  (** Where the popup goes next to the trigger. *)
  classes : classes;
}

type event =
  | Hover  (** The pointer entered the closed trigger. *)
  | Focus  (** The trigger got focus. *)
  | Cancel  (** The pointer left, or pressed, the trigger before the delay. *)
  | Leave  (** The pointer left the open trigger or popup. *)
  | Resume  (** The pointer or focus came back while the tooltip closes. *)
  | Close of Popup.reason
  | Elapsed of delay  (** The timer of {!delay_ms} ms fired. *)

type state =
  | Closed
  | Opening  (** Hidden; waits for [delay]. *)
  | Open
  | Closing  (** Shown; waits for [close_delay]. *)

val machine : (props, state, event) Silica.Machine.t
(** The machine of the component. The checker reads it. *)

val component : (props, state, event) Silica.Component.t
(** The machine with its view. *)

val view : props -> state -> event Silica.Node.t
(** The view of a state. *)

val props :
  id:string ->
  label:string ->
  tip:string ->
  ?delay:delay ->
  ?close_delay:delay ->
  ?place:Silica.Place.t ->
  ?classes:classes ->
  unit ->
  props
(** [delay] defaults to [Medium], [close_delay] to [Instant], [place] to the top
    side, centered, 4 pixels away, 8 pixels from the edges of the viewport. *)

val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  props ->
  'e Silica.Node.t
(** The node. [on_open_change] hears each time the tooltip is shown or hidden,
    with the reason: [Trigger_hover], [Trigger_focus] (focus and blur),
    [Trigger_press] or [Escape_key]. *)
