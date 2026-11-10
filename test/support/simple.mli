val machine :
  id:string ->
  init:'s ->
  ?final:('s -> bool) ->
  ('s -> 'e -> ('s * 'e Silica.Cmd.t list) option) ->
  ('s, 'e) Silica.Machine.t
(** A machine whose data is all the state. The step returns the new data and the
    commands, or [None] when the event is unhandled. When [final] holds for the
    data, the machine is in a final state and ignores later events. Without
    [final] the spec has one state and no edge. *)
