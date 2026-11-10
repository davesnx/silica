(** A non-modal popup next to a trigger button (WAI-ARIA disclosure of a
    non-modal dialog; Base UI Popover).

    The trigger opens and closes the popup. The popup is painted in a portal at
    the end of the body and kept next to the trigger. When it opens, focus moves
    into it. Focus is not trapped and the page still scrolls. Escape closes it
    and focuses the trigger; a press outside the popup and the trigger closes it
    and leaves focus where the press put it.

    The content is the caller's, as in {!Dialog.Make}: the popover gives the
    content events to the parent, and closes when {!Dialog.CONTENT.closes} says
    so.

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

module Make (Content : Dialog.CONTENT) : sig
  type props = {
    id : string;  (** Unique in the page; the ids of the parts start with it. *)
    trigger : string;  (** The label of the trigger button. *)
    title : string option;
        (** A heading in the popup; the popup is labelled by it. *)
    close : string option;  (** The label of a close button in the popup. *)
    open_ : bool option;
        (** When this changes to [Some b], the popover opens or closes. With
            [None] only the trigger and the dismiss layer change it. A parent
            that sets it must follow the open changes it gets, or it cannot set
            the same value again. *)
    place : Silica.Place.t;  (** Where the popup goes next to the trigger. *)
    classes : classes;
    content : Content.props;
  }

  type event =
    | Props of props
    | Open_request of Popup.reason
    | Close_request of Popup.reason
    | Content of Content.event

  module State : sig
    type _ t = Idle : unit t | Closed : props t | Open : props t
  end

  type snapshot = At : 'data State.t * 'data -> snapshot

  val machine : (snapshot, event) Silica.Machine.t
  (** The machine of the component. The checker reads it. *)

  val component : (snapshot, event) Silica.Component.t
  (** The machine with its view. *)

  val view : snapshot -> event Silica.Node.t
  (** The view of a snapshot. *)

  val props :
    id:string ->
    trigger:string ->
    ?title:string ->
    ?close:string ->
    ?open_:bool ->
    ?place:Silica.Place.t ->
    ?classes:classes ->
    Content.props ->
    props
  (** [place] defaults to {!Silica.Place.default}. *)

  val make :
    ?key:string ->
    ?on_open_change:(bool -> Popup.reason -> 'e) ->
    ?on_content:(Content.event -> 'e) ->
    props ->
    'e Silica.Node.t
  (** The node. [on_open_change] hears each open or close that the popover did
      by itself (trigger, Escape, outside press, close part, content), not those
      that [open_] asked for. *)
end
