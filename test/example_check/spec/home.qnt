// Generated from the spec of the machine "home". Do not edit.
module home {
  type State = | Home
  type Event = OnStart | OnAny

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Home => Set()
  }

  pure val finals: Set[State] = Set(Home)
  pure val movable: Set[State] = Set()

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Home,
    running' = owned(Home),
    last' = OnStart,
  }

  action step = all {
    state' = state,
    running' = running,
    last' = last,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
