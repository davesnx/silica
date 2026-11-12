(** A left to right layered drawing of a {!Spec.t}, computed in OCaml with no
    external tool. The same spec gives the same drawing on every host, native
    and Melange.

    Coordinates are in pixels from the top left corner, y downwards. Sizes
    assume a monospace font: 13 pixels for state names, 11 pixels for details
    and edge labels. *)

type node = {
  name : string;
  x : float;  (** Left side. *)
  y : float;  (** Top side. *)
  width : float;
  height : float;
  final : bool;
  initial : bool;
  details : string list;  (** {!Spec.details} of the state. *)
  drift : bool;
      (** The state is not in the spec: only an [observed] step names it. *)
}

type label = {
  lines : string list;  (** The events, wrapped at about 24 characters. *)
  center : float;  (** The x of the middle of the lines. *)
  bottom : float;  (** The y under the last line. *)
  width : float;  (** The width of the widest line, with a small margin. *)
}

type edge = {
  from : string;
  target : string;
  points : (float * float) list;
      (** From the border of [from] to the border of [target]. Between two
          points with the same y the edge is a straight line; between two points
          with different y it is a cubic curve with horizontal tangents at both
          ends, which stays in the box of the two points. For a self edge
          ([from = target]) the four points are the start, the two control
          points and the end of one cubic curve above the node. *)
  back : bool;  (** The edge goes right to left, against the main direction. *)
  label : label option;
      (** [None] when the edge has no events, or when {!t.note} holds them. The
          label of an edge between two states sits on the edge, in the middle
          column of its path, centered on its horizontal part, so the line runs
          into the label and out of it. The label of a self edge sits above its
          loop. *)
  drift : bool;
      (** The edge is not an arrow of the spec: only an [observed] step took it.
      *)
}

type t = {
  machine : string;  (** The id of the spec. *)
  width : float;
  height : float;
  nodes : node list;  (** In the order of the spec states. *)
  edges : edge list;
      (** In the order of {!Spec.arrows}, then the [drift] edges, sorted. *)
  note : label option;
      (** When every arrow of the spec has the same events, and there are three
          arrows or more (the routers: ["url changed"] on each), the edges have
          no label and the note says it once, as ["every edge: url changed"],
          under the drawing. *)
}

val layout : ?observed:(string * string) list -> Spec.t -> t
(** [observed] are the [(from, target)] of steps that entered a state at run
    time, such as the transition log gives. A pair that is not an arrow of the
    spec is drawn as a [drift] edge, and a state that the spec lacks as a
    [drift] node.

    Layers by longest path after the back edges of a depth first search from the
    initial state are turned around. Edge labels and long edges get places of
    their own in the columns between the layers, so labels and edges do not
    cover nodes. The order in each column comes from barycenter sweeps and
    adjacent swaps that reduce crossings; heights come from the median of the
    neighbours. *)

val to_string : t -> string
(** One line for the size, each node and each edge, with positions rounded to
    whole pixels. For golden tests and to compare hosts. *)
