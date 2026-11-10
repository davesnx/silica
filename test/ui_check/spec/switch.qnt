// Generated from the spec of the machine "switch". Do not edit.
module switch {
  type State = | Idle | Unchecked | Checked
  type Event = OnStart | OnAny | OnPropsFalse | OnPropsFalseDisabled | OnPropsTrue | OnToggle

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Unchecked => Set()
    | Checked => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Unchecked, Checked)

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

  action checked_to_unchecked_on_props_false = all { state == Checked, enter(Unchecked, OnPropsFalse) }
  action checked_to_unchecked_on_props_false_disabled = all { state == Checked, enter(Unchecked, OnPropsFalseDisabled) }
  action checked_to_unchecked_on_toggle = all { state == Checked, enter(Unchecked, OnToggle) }
  action idle_to_checked_on_props_true = all { state == Idle, enter(Checked, OnPropsTrue) }
  action idle_to_unchecked_on_props_false = all { state == Idle, enter(Unchecked, OnPropsFalse) }
  action idle_to_unchecked_on_props_false_disabled = all { state == Idle, enter(Unchecked, OnPropsFalseDisabled) }
  action unchecked_to_checked_on_props_true = all { state == Unchecked, enter(Checked, OnPropsTrue) }
  action unchecked_to_checked_on_toggle = all { state == Unchecked, enter(Checked, OnToggle) }
  action checked_stays_on_props_false = all { state == Checked, state' = state, running' = running, last' = OnPropsFalse }
  action checked_stays_on_props_false_disabled = all { state == Checked, state' = state, running' = running, last' = OnPropsFalseDisabled }
  action checked_stays_on_props_true = all { state == Checked, state' = state, running' = running, last' = OnPropsTrue }
  action unchecked_stays_on_props_false = all { state == Unchecked, state' = state, running' = running, last' = OnPropsFalse }
  action unchecked_stays_on_props_false_disabled = all { state == Unchecked, state' = state, running' = running, last' = OnPropsFalseDisabled }
  action unchecked_stays_on_props_true = all { state == Unchecked, state' = state, running' = running, last' = OnPropsTrue }

  action step = any {
    checked_to_unchecked_on_props_false,
    checked_to_unchecked_on_props_false_disabled,
    checked_to_unchecked_on_toggle,
    idle_to_checked_on_props_true,
    idle_to_unchecked_on_props_false,
    idle_to_unchecked_on_props_false_disabled,
    unchecked_to_checked_on_props_true,
    unchecked_to_checked_on_toggle,
    checked_stays_on_props_false,
    checked_stays_on_props_false_disabled,
    checked_stays_on_props_true,
    unchecked_stays_on_props_false,
    unchecked_stays_on_props_false_disabled,
    unchecked_stays_on_props_true,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
