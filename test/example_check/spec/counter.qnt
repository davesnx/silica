// Generated from the spec of the machine "counter". Do not edit.
module counter {
  type State = | Counter
  type Event = OnStart | OnAny | OnDecr | OnIncr | OnReset

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Counter => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Counter)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Counter,
    running' = owned(Counter),
    last' = OnStart,
  }

  action counter_stays_on_decr = all { state == Counter, state' = state, running' = running, last' = OnDecr }
  action counter_stays_on_incr = all { state == Counter, state' = state, running' = running, last' = OnIncr }
  action counter_stays_on_reset = all { state == Counter, state' = state, running' = running, last' = OnReset }

  action step = any {
    counter_stays_on_decr,
    counter_stays_on_incr,
    counter_stays_on_reset,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
