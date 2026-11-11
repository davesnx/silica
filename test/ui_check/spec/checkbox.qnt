// Generated from the spec of the machine "checkbox". Do not edit.
module checkbox {
  type State = | Unchecked | Checked | Indeterminate
  type Event = OnStart | OnAny | OnEnter | OnProps | OnToggle

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Unchecked => Set()
    | Checked => Set()
    | Indeterminate => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Unchecked, Checked, Indeterminate)

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

  action checked_to_indeterminate_on_props = all { state == Checked, enter(Indeterminate, OnProps) }
  action checked_to_unchecked_on_props = all { state == Checked, enter(Unchecked, OnProps) }
  action checked_to_unchecked_on_toggle = all { state == Checked, enter(Unchecked, OnToggle) }
  action indeterminate_to_checked_on_props = all { state == Indeterminate, enter(Checked, OnProps) }
  action indeterminate_to_checked_on_toggle = all { state == Indeterminate, enter(Checked, OnToggle) }
  action indeterminate_to_unchecked_on_props = all { state == Indeterminate, enter(Unchecked, OnProps) }
  action unchecked_to_checked_on_props = all { state == Unchecked, enter(Checked, OnProps) }
  action unchecked_to_checked_on_toggle = all { state == Unchecked, enter(Checked, OnToggle) }
  action unchecked_to_indeterminate_on_props = all { state == Unchecked, enter(Indeterminate, OnProps) }
  action checked_stays_on_enter = all { state == Checked, state' = state, running' = running, last' = OnEnter }
  action checked_stays_on_props = all { state == Checked, state' = state, running' = running, last' = OnProps }
  action indeterminate_stays_on_enter = all { state == Indeterminate, state' = state, running' = running, last' = OnEnter }
  action unchecked_stays_on_enter = all { state == Unchecked, state' = state, running' = running, last' = OnEnter }
  action unchecked_stays_on_props = all { state == Unchecked, state' = state, running' = running, last' = OnProps }

  action step = any {
    checked_to_indeterminate_on_props,
    checked_to_unchecked_on_props,
    checked_to_unchecked_on_toggle,
    indeterminate_to_checked_on_props,
    indeterminate_to_checked_on_toggle,
    indeterminate_to_unchecked_on_props,
    unchecked_to_checked_on_props,
    unchecked_to_checked_on_toggle,
    unchecked_to_indeterminate_on_props,
    checked_stays_on_enter,
    checked_stays_on_props,
    indeterminate_stays_on_enter,
    unchecked_stays_on_enter,
    unchecked_stays_on_props,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
