(** Where a popup goes next to its anchor. Pure: a renderer measures the
    rectangles and writes the result. *)

type side = Top | Bottom | Left | Right
type align = Start | Center | End

type t = {
  side : side;  (** The preferred side of the anchor. *)
  align : align;  (** The alignment along that side. *)
  offset : float;  (** The gap between the anchor and the popup. *)
  padding : float;  (** The smallest gap between the popup and the viewport. *)
}

val default : t
(** Below the anchor, aligned at its start, 4 pixels away, 8 pixels from the
    edges of the viewport. *)

type result = {
  x : float;
  y : float;
  side : side;  (** The side used: the preferred one, or its opposite. *)
  align : align;
  available_width : float;
  available_height : float;
      (** The room on the used side, along the main axis and across it, inside
          the padding. *)
}

val compute : t -> anchor:Rect.t -> popup:Rect.t -> viewport:Rect.t -> result
(** The popup goes on the preferred side when it fits there. When it does not
    fit and the opposite side has more room, it goes on the opposite side. Along
    the side it is aligned with the anchor and then moved inside the viewport,
    less the padding. Only the [width] and [height] of [popup] are read. *)

val side_name : side -> string
(** ["top"], ["bottom"], ["left"] or ["right"], as in [data-side]. *)

val align_name : align -> string
(** ["start"], ["center"] or ["end"], as in [data-align]. *)
