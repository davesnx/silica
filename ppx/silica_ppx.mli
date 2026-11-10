(** Registers the Silica JSX transformation with ppxlib. The only API is the SVG
    attribute name rule and its table, exposed for the test in test/ppx. *)

module Svg_names : sig
  val camel_case_allow_list : string list
  (** The SVG attribute names that keep their upper-case letters, such as
      [viewBox]. *)

  val printed_name : string -> string
  (** The printed name of an SVG attribute label: an allowed camelCase name
      stays, an [xlink] or [xml] label gets a colon, and any other name is
      dashed. A last [_] is dropped. *)

  val violation : string -> string option
  (** [Some reason] when a printed name breaks the rule above, [None] when it is
      good. *)
end

val svg_attributes : unit -> (string * string) list
(** Every SVG attribute of the JSX table, as pairs of the JSX label and the
    printed name, sorted. *)
