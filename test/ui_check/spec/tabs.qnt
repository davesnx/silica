// Generated from the spec of the machine "tabs". Do not edit.
module tabs {
  type State = | Ready
  type Event = OnStart | OnAny | OnFirst | OnLast | OnNext | OnPrevious | OnProps | OnSelectA | OnSelectC

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Ready => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Ready)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Ready,
    running' = owned(Ready),
    last' = OnStart,
  }

  action ready_stays_on_first = all { state == Ready, state' = state, running' = running, last' = OnFirst }
  action ready_stays_on_last = all { state == Ready, state' = state, running' = running, last' = OnLast }
  action ready_stays_on_next = all { state == Ready, state' = state, running' = running, last' = OnNext }
  action ready_stays_on_previous = all { state == Ready, state' = state, running' = running, last' = OnPrevious }
  action ready_stays_on_props = all { state == Ready, state' = state, running' = running, last' = OnProps }
  action ready_stays_on_select_a = all { state == Ready, state' = state, running' = running, last' = OnSelectA }
  action ready_stays_on_select_c = all { state == Ready, state' = state, running' = running, last' = OnSelectC }

  action step = any {
    ready_stays_on_first,
    ready_stays_on_last,
    ready_stays_on_next,
    ready_stays_on_previous,
    ready_stays_on_props,
    ready_stays_on_select_a,
    ready_stays_on_select_c,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
