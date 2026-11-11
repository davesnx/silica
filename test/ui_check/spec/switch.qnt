// Generated from the spec of the machine "switch". Do not edit.
module switch {
  type State = | Unchecked | Checked
  type Event = OnStart | OnAny | OnProps | OnToggle

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Unchecked => Set()
    | Checked => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Unchecked, Checked)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Unchecked,
    running' = owned(Unchecked),
    last' = OnStart,
  }

  action checked_to_unchecked_on_props = all { state == Checked, enter(Unchecked, OnProps) }
  action checked_to_unchecked_on_toggle = all { state == Checked, enter(Unchecked, OnToggle) }
  action unchecked_to_checked_on_props = all { state == Unchecked, enter(Checked, OnProps) }
  action unchecked_to_checked_on_toggle = all { state == Unchecked, enter(Checked, OnToggle) }
  action checked_stays_on_props = all { state == Checked, state' = state, running' = running, last' = OnProps }
  action unchecked_stays_on_props = all { state == Unchecked, state' = state, running' = running, last' = OnProps }

  action step = any {
    checked_to_unchecked_on_props,
    checked_to_unchecked_on_toggle,
    unchecked_to_checked_on_props,
    unchecked_to_checked_on_toggle,
    checked_stays_on_props,
    unchecked_stays_on_props,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
