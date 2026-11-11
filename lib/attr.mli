(** Attribute constructors. *)

val string : string -> string -> 'e Node.attr
(** [string name value] sets the attribute [name] to [value]. *)

val bool : string -> bool -> 'e Node.attr
(** [bool name b] sets the attribute [name] with no value when [b] is [true],
    and leaves it out when [b] is [false]. *)

val ref_ : Ref.t -> 'e Node.attr
(** Connects the element to a {!Ref.t}, so the machine can read its size and
    position or move focus to it. *)

val style : string -> 'e Node.attr
(** Sets the [style] attribute to the given CSS text. *)

val on_press : 'e -> 'e Node.attr
(** A press (click, or Enter or Space on a button) on the element sends the
    event. *)

val on_double_press : 'e -> 'e Node.attr
(** A double press on the element sends the event. *)

val on_input : ?rate:Rate.t -> (string -> 'e) -> 'e Node.attr
(** Each change of the field value sends the event made from the new text. *)

val on_submit : 'e -> 'e Node.attr
(** A form submit sends the event. The browser does not load a new page. *)

val on_focus : 'e -> 'e Node.attr
(** The element gets focus. *)

val on_blur : 'e -> 'e Node.attr
(** The element loses focus. *)

val rated : Rate.t -> 'e Node.attr -> 'e Node.attr
(** Rate limit any event handler attribute. *)

val on_key_down :
  ?prevent:bool -> ?rate:Rate.t -> (Key.t -> 'e option) -> 'e Node.attr
(** A key goes down while the element or a descendant has focus. With [prevent],
    the default behavior of the key is cancelled when the handler returns
    [Some _]. *)

val on_key_up :
  ?prevent:bool -> ?rate:Rate.t -> (Key.t -> 'e option) -> 'e Node.attr
(** A key goes up while the element or a descendant has focus. [prevent] works
    as in {!on_key_down}. *)

val on_pointer_enter :
  ?prevent:bool -> ?rate:Rate.t -> (Pointer.t -> 'e option) -> 'e Node.attr
(** The pointer moves onto the element. *)

val on_pointer_leave :
  ?prevent:bool -> ?rate:Rate.t -> (Pointer.t -> 'e option) -> 'e Node.attr
(** The pointer moves off the element. *)

val on_reset : 'e Node.attr
(** See {!Node.On_reset}. *)
