val machine :
  id:string ->
  init:'s ->
  ?final:('s -> bool) ->
  ('s -> 'e -> ('s * 'e Silica.Action.t list) option) ->
  (unit, 's, 'e) Silica.Machine.t
(** A machine whose data is all the state. The step returns the new data and the
    actions, or [None] when the event is unhandled. When [final] holds for the
    data, the machine is in a final state and ignores later events. Without
    [final] the spec has one state and no edge. *)
