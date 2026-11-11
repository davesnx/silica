(** A modal dialog (WAI-ARIA dialog pattern; Base UI Dialog and Alert Dialog).

    The popup is painted in a portal at the end of the body, over a backdrop.
    While it is open, focus stays inside it, the page does not scroll, Escape
    and a press outside close it (unless [dismissible] is [false]; an alert
    dialog ignores outside presses). On close, focus goes to the trigger, or,
    with no trigger, back to the element that had it before.

    The content is the caller's: {!val-component} takes its view, and the props
    carry its data. The dialog gives the content events to the parent, and
    closes when [closes] says so.

    Styling: classes per part ({!type-classes}) and the attributes [data-open]
    and [data-closed] on the backdrop and the popup, [data-popup-open] on the
    trigger. *)

type classes = {
  trigger : string;
  backdrop : string;
  popup : string;
  title : string;
  description : string;
  close : string;
}

val no_classes : classes
(** All empty. *)

type ('content, 'ce) props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  title : string;  (** The heading; the popup is labelled by it. *)
  description : string option;
  trigger : string option;
      (** The label of a trigger button; [None]: the parent opens it. *)
  close : string option;  (** The label of a close button in the popup. *)
  open_ : bool option;
      (** When this changes to [Some b], the dialog opens or closes. With [None]
          only the trigger opens it. A parent that sets it must follow the open
          changes it gets, or it cannot set the same value again. *)
  dismissible : bool;  (** Escape and an outside press close it. *)
  alert : bool;  (** [role="alertdialog"]: an outside press never closes it. *)
  classes : classes;
  content : 'content;
      (** The data the content shows. Compared with [=]: no functions. *)
}

type 'ce event =
  | Open_request of Popup.reason
  | Close_request of Popup.reason
  | Content of 'ce  (** An event of the content. *)

type state = Closed | Open

type ('content, 'ce) t = {
  component : (('content, 'ce) props, state, 'ce event) Silica.Component.t;
      (** The machine with its view. The checker reads it. *)
  closes : 'ce -> bool;
}
(** A dialog for one kind of content. *)

val component :
  id:string ->
  closes:('ce -> bool) ->
  ?events:'ce Silica.Machine.shape ->
  ('content -> 'ce Silica.Node.t) ->
  ('content, 'ce) t
(** [component ~id ~closes view] is a dialog whose content is [view content].
    [id] is the id of the machine, as in the spec and the checker. The dialog
    closes after a content event for which [closes] is [true], with reason
    {!Popup.Close_press}. [events] names the content events (see
    {!Silica.Machine.event_name}): the dialog names its own events
    ["open_request <reason>"] and ["close_request <reason>"] (see
    {!Popup.reason_name}), and a content event by [events], or ["content"]
    without it. Define it once, at the top level. *)

val view :
  content:('content -> 'ce Silica.Node.t) ->
  ('content, 'ce) props ->
  state ->
  'ce event Silica.Node.t
(** The view of a state. *)

val props :
  id:string ->
  title:string ->
  ?description:string ->
  ?trigger:string ->
  ?close:string ->
  ?open_:bool ->
  ?dismissible:bool ->
  ?alert:bool ->
  ?classes:classes ->
  'content ->
  ('content, 'ce) props
(** [dismissible] defaults to [true], [alert] to [false]. *)

val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_content:('ce -> 'e) ->
  ('content, 'ce) t ->
  ('content, 'ce) props ->
  'e Silica.Node.t
(** The node. [on_open_change] hears each open or close that the dialog did by
    itself (trigger, Escape, outside press, close part), not those that [open_]
    asked for. *)
