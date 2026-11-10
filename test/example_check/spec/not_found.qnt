// Generated from the spec of the machine "not_found". Do not edit.
module not_found {
  type State = | NotFound
  type Event = OnStart | OnAny

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | NotFound => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(NotFound)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = NotFound,
    running' = owned(NotFound),
    last' = OnStart,
  }

  action not_found_to_not_found_on_any = all { state == NotFound, enter(NotFound, OnAny) }

  action step = any {
    not_found_to_not_found_on_any,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
