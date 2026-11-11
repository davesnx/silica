(** A machine together with the view of its states. Each rendered component runs
    its own machine while it is rendered, keyed by its position, its machine id
    and an optional key. The parent gives props to the child, and the child
    gives output to the parent through function props.

    A component with props is written as a module with a [make] function, so
    that it works as a tag in [.mlx]:

    {[
      let component = Component.make_with_props machine view

      let make ?key ~id ~on_pick () =
        Component.node_with_props ?key component ~props:id
          ~output:(fun event _ ->
            match event with Picked hit -> [ on_pick hit ] | _ -> []
        )
    ]}

    and is used as [<Row id on_pick=(fun hit -> Pick hit) />]. *)

type ('p, 's, 'e) t = ('p, 's, 'e) Node.component = {
  machine : ('p, 's, 'e) Machine.t;
  view : 'p -> 's -> 'e Node.t;
}

val make : ('p, 's, 'e) Machine.t -> ('s -> 'e Node.t) -> ('p, 's, 'e) t
(** [make machine view] is a component whose view does not read the props.
    Define it once, at the top level. *)

val make_with_props :
  ('p, 's, 'e) Machine.t -> ('p -> 's -> 'e Node.t) -> ('p, 's, 'e) t
(** [make_with_props machine view]: the view gets the props of the moment. *)

val node :
  ?key:string ->
  ?output:('ce -> 's -> 'e list) ->
  ?on_final:('s -> 'e list) ->
  (unit, 's, 'ce) t ->
  'e Node.t
(** The node that renders a component with no props (a machine made by
    {!Machine.make}). See {!Node.child}. *)

val node_with_props :
  ?key:string ->
  props:'p ->
  ?output:('ce -> 's -> 'e list) ->
  ?on_final:('s -> 'e list) ->
  ('p, 's, 'ce) t ->
  'e Node.t
(** The node that renders the component, with its props ([()] for a machine made
    by {!Machine.make}). See {!Node.child}. *)
