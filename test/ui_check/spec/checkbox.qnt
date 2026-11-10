// Generated from the spec of the machine "checkbox". Do not edit.
module checkbox {
  type State = | Idle | Unchecked | Checked | Indeterminate
  type Event = OnStart | OnAny | OnEnter | OnPropsFalse | OnPropsFalseDisabled | OnPropsMixed | OnPropsTrue | OnToggle

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Unchecked => Set()
    | Checked => Set()
    | Indeterminate => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Unchecked, Checked, Indeterminate)

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

  action checked_to_indeterminate_on_props_mixed = all { state == Checked, enter(Indeterminate, OnPropsMixed) }
  action checked_to_unchecked_on_props_false = all { state == Checked, enter(Unchecked, OnPropsFalse) }
  action checked_to_unchecked_on_props_false_disabled = all { state == Checked, enter(Unchecked, OnPropsFalseDisabled) }
  action checked_to_unchecked_on_toggle = all { state == Checked, enter(Unchecked, OnToggle) }
  action idle_to_checked_on_props_true = all { state == Idle, enter(Checked, OnPropsTrue) }
  action idle_to_indeterminate_on_props_mixed = all { state == Idle, enter(Indeterminate, OnPropsMixed) }
  action idle_to_unchecked_on_props_false = all { state == Idle, enter(Unchecked, OnPropsFalse) }
  action idle_to_unchecked_on_props_false_disabled = all { state == Idle, enter(Unchecked, OnPropsFalseDisabled) }
  action indeterminate_to_checked_on_props_true = all { state == Indeterminate, enter(Checked, OnPropsTrue) }
  action indeterminate_to_checked_on_toggle = all { state == Indeterminate, enter(Checked, OnToggle) }
  action indeterminate_to_unchecked_on_props_false = all { state == Indeterminate, enter(Unchecked, OnPropsFalse) }
  action indeterminate_to_unchecked_on_props_false_disabled = all { state == Indeterminate, enter(Unchecked, OnPropsFalseDisabled) }
  action unchecked_to_checked_on_props_true = all { state == Unchecked, enter(Checked, OnPropsTrue) }
  action unchecked_to_checked_on_toggle = all { state == Unchecked, enter(Checked, OnToggle) }
  action unchecked_to_indeterminate_on_props_mixed = all { state == Unchecked, enter(Indeterminate, OnPropsMixed) }
  action checked_stays_on_enter = all { state == Checked, state' = state, running' = running, last' = OnEnter }
  action checked_stays_on_props_false = all { state == Checked, state' = state, running' = running, last' = OnPropsFalse }
  action checked_stays_on_props_false_disabled = all { state == Checked, state' = state, running' = running, last' = OnPropsFalseDisabled }
  action checked_stays_on_props_mixed = all { state == Checked, state' = state, running' = running, last' = OnPropsMixed }
  action checked_stays_on_props_true = all { state == Checked, state' = state, running' = running, last' = OnPropsTrue }
  action indeterminate_stays_on_enter = all { state == Indeterminate, state' = state, running' = running, last' = OnEnter }
  action indeterminate_stays_on_props_mixed = all { state == Indeterminate, state' = state, running' = running, last' = OnPropsMixed }
  action unchecked_stays_on_enter = all { state == Unchecked, state' = state, running' = running, last' = OnEnter }
  action unchecked_stays_on_props_false = all { state == Unchecked, state' = state, running' = running, last' = OnPropsFalse }
  action unchecked_stays_on_props_false_disabled = all { state == Unchecked, state' = state, running' = running, last' = OnPropsFalseDisabled }
  action unchecked_stays_on_props_mixed = all { state == Unchecked, state' = state, running' = running, last' = OnPropsMixed }
  action unchecked_stays_on_props_true = all { state == Unchecked, state' = state, running' = running, last' = OnPropsTrue }

  action step = any {
    checked_to_indeterminate_on_props_mixed,
    checked_to_unchecked_on_props_false,
    checked_to_unchecked_on_props_false_disabled,
    checked_to_unchecked_on_toggle,
    idle_to_checked_on_props_true,
    idle_to_indeterminate_on_props_mixed,
    idle_to_unchecked_on_props_false,
    idle_to_unchecked_on_props_false_disabled,
    indeterminate_to_checked_on_props_true,
    indeterminate_to_checked_on_toggle,
    indeterminate_to_unchecked_on_props_false,
    indeterminate_to_unchecked_on_props_false_disabled,
    unchecked_to_checked_on_props_true,
    unchecked_to_checked_on_toggle,
    unchecked_to_indeterminate_on_props_mixed,
    checked_stays_on_enter,
    checked_stays_on_props_false,
    checked_stays_on_props_false_disabled,
    checked_stays_on_props_mixed,
    checked_stays_on_props_true,
    indeterminate_stays_on_enter,
    indeterminate_stays_on_props_mixed,
    unchecked_stays_on_enter,
    unchecked_stays_on_props_false,
    unchecked_stays_on_props_false_disabled,
    unchecked_stays_on_props_mixed,
    unchecked_stays_on_props_true,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
