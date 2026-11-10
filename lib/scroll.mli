(** The scroll position and sizes of the page or of an element, as a portable
    value, in CSS pixels. *)

type t = {
  left : float;  (** The distance scrolled from the left edge. *)
  top : float;  (** The distance scrolled from the top edge. *)
  scroll_width : float;  (** The width of the content, scrolled or not. *)
  scroll_height : float;  (** The height of the content, scrolled or not. *)
  client_width : float;  (** The width of the visible part. *)
  client_height : float;  (** The height of the visible part. *)
}

val zero : t
(** All six values are [0.]. *)
