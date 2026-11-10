(** A modal dialog (WAI-ARIA dialog pattern; Base UI Dialog and Alert Dialog).

    The popup is painted in a portal at the end of the body, over a backdrop.
    While it is open, focus stays inside it, the page does not scroll, Escape
    and a press outside close it (unless [dismissible] is [false]; an alert
    dialog ignores outside presses). On close, focus goes to the trigger, or,
    with no trigger, back to the element that had it before.

    The content is the caller's: {!Make} takes its view and its events. The
    dialog gives the content events to the parent, and closes when
    {!CONTENT.closes} says so.

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

module type CONTENT = sig
  type props
  (** The data the content shows. Compared with [=]: no functions. *)

  type event

  val id : string
  (** The id of the machine, as in the spec and the checker. *)

  val view : props -> event Silica.Node.t
  (** The content, for the props of the dialog. *)

  val closes : event -> bool
  (** The dialog closes after this content event, with reason
      {!Popup.Close_press}. *)
end

module Make (Content : CONTENT) : sig
  type props = {
    id : string;  (** Unique in the page; the ids of the parts start with it. *)
    title : string;  (** The heading; the popup is labelled by it. *)
    description : string option;
    trigger : string option;
        (** The label of a trigger button; [None]: the parent opens it. *)
    close : string option;  (** The label of a close button in the popup. *)
    open_ : bool option;
        (** When this changes to [Some b], the dialog opens or closes. With
            [None] only the trigger opens it. A parent that sets it must follow
            the open changes it gets, or it cannot set the same value again. *)
    dismissible : bool;  (** Escape and an outside press close it. *)
    alert : bool;
        (** [role="alertdialog"]: an outside press never closes it. *)
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
    title:string ->
    ?description:string ->
    ?trigger:string ->
    ?close:string ->
    ?open_:bool ->
    ?dismissible:bool ->
    ?alert:bool ->
    ?classes:classes ->
    Content.props ->
    props
  (** [dismissible] defaults to [true], [alert] to [false]. *)

  val make :
    ?key:string ->
    ?on_open_change:(bool -> Popup.reason -> 'e) ->
    ?on_content:(Content.event -> 'e) ->
    props ->
    'e Silica.Node.t
  (** The node. [on_open_change] hears each open or close that the dialog did by
      itself (trigger, Escape, outside press, close part), not those that
      [open_] asked for. *)
end
