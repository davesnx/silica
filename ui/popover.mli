(** A non-modal popup next to a trigger button (WAI-ARIA disclosure of a
    non-modal dialog; Base UI Popover).

    The trigger opens and closes the popup. The popup is painted in a portal at
    the end of the body and kept next to the trigger. When it opens, focus moves
    into it. Focus is not trapped and the page still scrolls. Escape closes it
    and focuses the trigger; a press outside the popup and the trigger closes it
    and leaves focus where the press put it.

    The content is the caller's, as in {!Dialog}: {!val-component} takes its
    view, and the props carry its data. The popover gives the content events to
    the parent, and closes when [closes] says so.

    Styling: classes per part ({!type-classes}), [data-open] and [data-closed]
    on the trigger and the popup, [data-popup-open] on the trigger, and the
    [data-side] and [data-align] attributes that the renderer writes on the
    popup. *)

type classes = {
  trigger : string;
  popup : string;
  title : string;
  close : string;
}

val no_classes : classes
(** All empty. *)

type ('content, 'ce) props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  trigger : string;  (** The label of the trigger button. *)
  title : string option;
      (** A heading in the popup; the popup is labelled by it. *)
  close : string option;  (** The label of a close button in the popup. *)
  open_ : bool option;
      (** When this changes to [Some b], the popover opens or closes. With
          [None] only the trigger and the dismiss layer change it. A parent that
          sets it must follow the open changes it gets, or it cannot set the
          same value again. *)
  place : Silica.Place.t;  (** Where the popup goes next to the trigger. *)
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
(** A popover for one kind of content. *)

val component :
  id:string ->
  closes:('ce -> bool) ->
  ('content -> 'ce Silica.Node.t) ->
  ('content, 'ce) t
(** [component ~id ~closes view] is a popover whose content is [view content].
    [id] is the id of the machine, as in the spec and the checker. The popover
    closes after a content event for which [closes] is [true], with reason
    {!Popup.Close_press}. Define it once, at the top level. *)

val view :
  content:('content -> 'ce Silica.Node.t) ->
  ('content, 'ce) props ->
  state ->
  'ce event Silica.Node.t
(** The view of a state. *)

val props :
  id:string ->
  trigger:string ->
  ?title:string ->
  ?close:string ->
  ?open_:bool ->
  ?place:Silica.Place.t ->
  ?classes:classes ->
  'content ->
  ('content, 'ce) props
(** [place] defaults to {!Silica.Place.default}. *)

val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_content:('ce -> 'e) ->
  ('content, 'ce) t ->
  ('content, 'ce) props ->
  'e Silica.Node.t
(** The node. [on_open_change] hears each open or close that the popover did by
    itself (trigger, Escape, outside press, close part, content), not those that
    [open_] asked for. *)
