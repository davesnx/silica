(** Stories: named examples of a component, for a component browser such as the
    one of the Silica site.

    A story is a plain value: a name, the source text that a reader sees, the
    controls that change its props, and a function that renders it. Keep the
    source honest: embed the text of the file that defines [render] with a dune
    rule, so that the shown code is the compiled code.

    {[
      let disabled =
        {
          Story.name = "Disabled";
          source = Sources.switch_disabled;
          controls = [ Story.toggle "disabled" ~label:"Disabled" true ];
          render =
            (fun ~id values ->
              Switch.make
                (Switch.props ~id ~label:"Wi-Fi"
                   ~disabled:(Story.bool values "disabled")
                   ()
                )
            );
        }
    ]}

    The values of the controls are data: a parent machine keeps them (one
    {!type-values} per story) and gives them to [render]. {!val-controls} draws
    the controls with silica-ui {!Switch} and {!Select}. *)

type control =
  | Toggle of { name : string; label : string; default : bool }
  | Choice of {
      name : string;
      label : string;
      options : (string * string) list;  (** Pairs of a value and its label. *)
      default : string;
    }

val toggle : string -> label:string -> bool -> control
(** [toggle name ~label default]. *)

val choice :
  string -> label:string -> (string * string) list -> string -> control
(** [choice name ~label options default]. Raises [Invalid_argument] when
    [default] is not one of the values of [options]. *)

type values
(** The value of each control of a story. Comparable with [=]. *)

type t = {
  name : string;  (** Shown as the heading of the story. *)
  source : string;  (** The code that a reader sees. *)
  controls : control list;
  render : 'e. id:string -> values -> 'e Silica.Node.t;
      (** The story. [id] is unique in the page: pass it, or ids made from it,
          to the components, so that one story can show more than once. *)
}

val slug : t -> string
(** The name in lower case, with [-] for each run of other characters than ASCII
    letters and digits: an anchor for the story. *)

val defaults : t -> values
(** The default of each control. *)

val bool : values -> string -> bool
(** The value of a {!Toggle}. Raises [Not_found] when no toggle has this name.
*)

val string : values -> string -> string
(** The value of a {!Choice}. Raises [Not_found] when no choice has this name.
*)

val set_bool : values -> string -> bool -> values
val set_string : values -> string -> string -> values

val controls : id:string -> t -> values -> (values -> 'e) -> 'e Silica.Node.t
(** The controls of the story with these values, as a [<fieldset>] of a
    {!Switch} for each toggle and a {!Select} for each choice. Each change gives
    the new values to the function. Empty when the story has no controls. *)

val render : id:string -> t -> values -> 'e Silica.Node.t
(** [render ~id story values] is [story.render ~id values]. *)
