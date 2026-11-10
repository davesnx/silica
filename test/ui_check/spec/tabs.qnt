// Generated from the spec of the machine "tabs". Do not edit.
module tabs {
  type State = | Idle | Ready
  type Event = OnStart | OnAny | OnFirst | OnLast | OnNext | OnPrevious | OnPropsA | OnPropsC | OnPropsVerticalA | OnSelectA | OnSelectC

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Ready => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Ready)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Idle,
    running' = owned(Idle),
    last' = OnStart,
  }

  action idle_to_ready_on_props_a = all { state == Idle, enter(Ready, OnPropsA) }
  action idle_to_ready_on_props_c = all { state == Idle, enter(Ready, OnPropsC) }
  action idle_to_ready_on_props_vertical_a = all { state == Idle, enter(Ready, OnPropsVerticalA) }
  action ready_stays_on_first = all { state == Ready, state' = state, running' = running, last' = OnFirst }
  action ready_stays_on_last = all { state == Ready, state' = state, running' = running, last' = OnLast }
  action ready_stays_on_next = all { state == Ready, state' = state, running' = running, last' = OnNext }
  action ready_stays_on_previous = all { state == Ready, state' = state, running' = running, last' = OnPrevious }
  action ready_stays_on_props_a = all { state == Ready, state' = state, running' = running, last' = OnPropsA }
  action ready_stays_on_props_c = all { state == Ready, state' = state, running' = running, last' = OnPropsC }
  action ready_stays_on_props_vertical_a = all { state == Ready, state' = state, running' = running, last' = OnPropsVerticalA }
  action ready_stays_on_select_a = all { state == Ready, state' = state, running' = running, last' = OnSelectA }
  action ready_stays_on_select_c = all { state == Ready, state' = state, running' = running, last' = OnSelectC }

  action step = any {
    idle_to_ready_on_props_a,
    idle_to_ready_on_props_c,
    idle_to_ready_on_props_vertical_a,
    ready_stays_on_first,
    ready_stays_on_last,
    ready_stays_on_next,
    ready_stays_on_previous,
    ready_stays_on_props_a,
    ready_stays_on_props_c,
    ready_stays_on_props_vertical_a,
    ready_stays_on_select_a,
    ready_stays_on_select_c,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
