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

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Home)

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

  action home_to_home_on_any = all { state == Home, enter(Home, OnAny) }

  action step = any {
    home_to_home_on_any,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
