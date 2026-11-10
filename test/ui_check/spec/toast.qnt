// Generated from the spec of the machine "toast". Do not edit.
module toast {
  type State = | Idle | Empty | Running | Paused | Expired
  type Event = OnStart | OnAny | OnCloseA | OnCloseB | OnFocusFalse | OnFocusTrue | OnHoverFalse | OnHoverTrue | OnProps | OnPropsA | OnPropsAB | OnTick

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Empty => Set()
    | Running => Set("running/after 100ms")
    | Paused => Set()
    | Expired => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Empty, Running, Paused, Expired)

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

  action empty_to_paused_on_props_a = all { state == Empty, enter(Paused, OnPropsA) }
  action empty_to_paused_on_props_a_b = all { state == Empty, enter(Paused, OnPropsAB) }
  action empty_to_running_on_props_a = all { state == Empty, enter(Running, OnPropsA) }
  action empty_to_running_on_props_a_b = all { state == Empty, enter(Running, OnPropsAB) }
  action expired_to_empty_on_props = all { state == Expired, enter(Empty, OnProps) }
  action expired_to_paused_on_props_a = all { state == Expired, enter(Paused, OnPropsA) }
  action expired_to_paused_on_props_a_b = all { state == Expired, enter(Paused, OnPropsAB) }
  action expired_to_running_on_props_a = all { state == Expired, enter(Running, OnPropsA) }
  action expired_to_running_on_props_a_b = all { state == Expired, enter(Running, OnPropsAB) }
  action idle_to_empty_on_props = all { state == Idle, enter(Empty, OnProps) }
  action idle_to_running_on_props_a = all { state == Idle, enter(Running, OnPropsA) }
  action idle_to_running_on_props_a_b = all { state == Idle, enter(Running, OnPropsAB) }
  action paused_to_empty_on_props = all { state == Paused, enter(Empty, OnProps) }
  action paused_to_running_on_close_a = all { state == Paused, enter(Running, OnCloseA) }
  action paused_to_running_on_close_b = all { state == Paused, enter(Running, OnCloseB) }
  action paused_to_running_on_focus_false = all { state == Paused, enter(Running, OnFocusFalse) }
  action paused_to_running_on_hover_false = all { state == Paused, enter(Running, OnHoverFalse) }
  action running_to_empty_on_props = all { state == Running, enter(Empty, OnProps) }
  action running_to_expired_on_tick = all { state == Running, enter(Expired, OnTick) }
  action running_to_paused_on_focus_true = all { state == Running, enter(Paused, OnFocusTrue) }
  action running_to_paused_on_hover_true = all { state == Running, enter(Paused, OnHoverTrue) }
  action running_to_running_on_props_a = all { state == Running, enter(Running, OnPropsA) }
  action running_to_running_on_props_a_b = all { state == Running, enter(Running, OnPropsAB) }
  action running_to_running_on_tick = all { state == Running, enter(Running, OnTick) }
  action empty_stays_on_focus_false = all { state == Empty, state' = state, running' = running, last' = OnFocusFalse }
  action empty_stays_on_focus_true = all { state == Empty, state' = state, running' = running, last' = OnFocusTrue }
  action empty_stays_on_hover_false = all { state == Empty, state' = state, running' = running, last' = OnHoverFalse }
  action empty_stays_on_hover_true = all { state == Empty, state' = state, running' = running, last' = OnHoverTrue }
  action empty_stays_on_props = all { state == Empty, state' = state, running' = running, last' = OnProps }
  action expired_stays_on_close_a = all { state == Expired, state' = state, running' = running, last' = OnCloseA }
  action expired_stays_on_close_b = all { state == Expired, state' = state, running' = running, last' = OnCloseB }
  action expired_stays_on_focus_false = all { state == Expired, state' = state, running' = running, last' = OnFocusFalse }
  action expired_stays_on_focus_true = all { state == Expired, state' = state, running' = running, last' = OnFocusTrue }
  action expired_stays_on_hover_false = all { state == Expired, state' = state, running' = running, last' = OnHoverFalse }
  action expired_stays_on_hover_true = all { state == Expired, state' = state, running' = running, last' = OnHoverTrue }
  action expired_stays_on_props_a = all { state == Expired, state' = state, running' = running, last' = OnPropsA }
  action expired_stays_on_props_a_b = all { state == Expired, state' = state, running' = running, last' = OnPropsAB }
  action paused_stays_on_close_a = all { state == Paused, state' = state, running' = running, last' = OnCloseA }
  action paused_stays_on_close_b = all { state == Paused, state' = state, running' = running, last' = OnCloseB }
  action paused_stays_on_focus_false = all { state == Paused, state' = state, running' = running, last' = OnFocusFalse }
  action paused_stays_on_focus_true = all { state == Paused, state' = state, running' = running, last' = OnFocusTrue }
  action paused_stays_on_hover_false = all { state == Paused, state' = state, running' = running, last' = OnHoverFalse }
  action paused_stays_on_hover_true = all { state == Paused, state' = state, running' = running, last' = OnHoverTrue }
  action paused_stays_on_props_a = all { state == Paused, state' = state, running' = running, last' = OnPropsA }
  action paused_stays_on_props_a_b = all { state == Paused, state' = state, running' = running, last' = OnPropsAB }
  action running_stays_on_close_a = all { state == Running, state' = state, running' = running, last' = OnCloseA }
  action running_stays_on_close_b = all { state == Running, state' = state, running' = running, last' = OnCloseB }
  action running_stays_on_props_a = all { state == Running, state' = state, running' = running, last' = OnPropsA }
  action running_stays_on_props_a_b = all { state == Running, state' = state, running' = running, last' = OnPropsAB }

  action step = any {
    empty_to_paused_on_props_a,
    empty_to_paused_on_props_a_b,
    empty_to_running_on_props_a,
    empty_to_running_on_props_a_b,
    expired_to_empty_on_props,
    expired_to_paused_on_props_a,
    expired_to_paused_on_props_a_b,
    expired_to_running_on_props_a,
    expired_to_running_on_props_a_b,
    idle_to_empty_on_props,
    idle_to_running_on_props_a,
    idle_to_running_on_props_a_b,
    paused_to_empty_on_props,
    paused_to_running_on_close_a,
    paused_to_running_on_close_b,
    paused_to_running_on_focus_false,
    paused_to_running_on_hover_false,
    running_to_empty_on_props,
    running_to_expired_on_tick,
    running_to_paused_on_focus_true,
    running_to_paused_on_hover_true,
    running_to_running_on_props_a,
    running_to_running_on_props_a_b,
    running_to_running_on_tick,
    empty_stays_on_focus_false,
    empty_stays_on_focus_true,
    empty_stays_on_hover_false,
    empty_stays_on_hover_true,
    empty_stays_on_props,
    expired_stays_on_close_a,
    expired_stays_on_close_b,
    expired_stays_on_focus_false,
    expired_stays_on_focus_true,
    expired_stays_on_hover_false,
    expired_stays_on_hover_true,
    expired_stays_on_props_a,
    expired_stays_on_props_a_b,
    paused_stays_on_close_a,
    paused_stays_on_close_b,
    paused_stays_on_focus_false,
    paused_stays_on_focus_true,
    paused_stays_on_hover_false,
    paused_stays_on_hover_true,
    paused_stays_on_props_a,
    paused_stays_on_props_a_b,
    running_stays_on_close_a,
    running_stays_on_close_b,
    running_stays_on_props_a,
    running_stays_on_props_a_b,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
