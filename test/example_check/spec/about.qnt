// Generated from the spec of the machine "about". Do not edit.
module about {
  type State = | About
  type Event = OnStart | OnAny | OnGoBack

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | About => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(About)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = About,
    running' = owned(About),
    last' = OnStart,
  }

  action about_to_about_on_any = all { state == About, enter(About, OnAny) }
  action about_stays_on_go_back = all { state == About, state' = state, running' = running, last' = OnGoBack }

  action step = any {
    about_to_about_on_any,
    about_stays_on_go_back,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
