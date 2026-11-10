(** A machine together with the view of its states. Each rendered component runs
    its own machine while it is rendered, keyed by its position and an optional
    key. The parent gives props to the child as an event, and the child gives
    output to the parent through function props.

    A component with props is written as a module with a [make] function, so
    that it works as a tag in [.mlx]:

    {[
      let component = Component.make machine view

      let make ?key ~id ~on_pick () =
        Component.node ?key component ~props:(Props id) ~output:(fun event _ ->
            match event with Picked hit -> [ on_pick hit ] | _ -> []
        )
    ]}

    and is used as [<Row id on_pick=(fun hit -> Pick hit) />]. *)

type ('s, 'e) t = ('s, 'e) Node.component = {
  machine : ('s, 'e) Machine.t;
  view : 's -> 'e Node.t;
}

val make : ('s, 'e) Machine.t -> ('s -> 'e Node.t) -> ('s, 'e) t
(** [make machine view] is a component. Define it once, at the top level. *)

val node :
  ?key:string ->
  ?props:'ce ->
  ?output:('ce -> 's -> 'e list) ->
  ?on_final:('s -> 'e list) ->
  ('s, 'ce) t ->
  'e Node.t
(** The node that renders the component. See {!Node.child}. *)
