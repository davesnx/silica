// Generated from the spec of the machine "votes". Do not edit.
module votes {
  type State = | Watching
  type Event = OnStart | OnAny | OnReconnect | OnUpdate

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Watching => Set("watching/invoke live:votes")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Watching)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Watching,
    running' = owned(Watching),
    last' = OnStart,
  }

  action watching_to_watching_on_reconnect = all { state == Watching, enter(Watching, OnReconnect) }
  action watching_stays_on_update = all { state == Watching, state' = state, running' = running, last' = OnUpdate }

  action step = any {
    watching_to_watching_on_reconnect,
    watching_stays_on_update,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
