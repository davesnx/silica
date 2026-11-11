// Generated from the spec of the machine "toast". Do not edit.
module toast {
  type State = | Empty | Expired | Paused | Running
  type Event = OnStart | OnAny | OnCloseA | OnCloseB | OnFocusFalse | OnFocusTrue | OnHoverFalse | OnHoverTrue | OnProps | OnTick

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Empty => Set()
    | Expired => Set()
    | Paused => Set()
    | Running => Set("running/after 100ms")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Empty, Expired, Paused, Running)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Empty,
    running' = owned(Empty),
    last' = OnStart,
  }

  action empty_to_paused_on_props = all { state == Empty, enter(Paused, OnProps) }
  action empty_to_running_on_props = all { state == Empty, enter(Running, OnProps) }
  action expired_to_empty_on_props = all { state == Expired, enter(Empty, OnProps) }
  action expired_to_paused_on_props = all { state == Expired, enter(Paused, OnProps) }
  action expired_to_running_on_props = all { state == Expired, enter(Running, OnProps) }
  action paused_to_empty_on_props = all { state == Paused, enter(Empty, OnProps) }
  action paused_to_running_on_close_a = all { state == Paused, enter(Running, OnCloseA) }
  action paused_to_running_on_close_b = all { state == Paused, enter(Running, OnCloseB) }
  action paused_to_running_on_focus_false = all { state == Paused, enter(Running, OnFocusFalse) }
  action paused_to_running_on_hover_false = all { state == Paused, enter(Running, OnHoverFalse) }
  action running_to_empty_on_props = all { state == Running, enter(Empty, OnProps) }
  action running_to_expired_on_tick = all { state == Running, enter(Expired, OnTick) }
  action running_to_paused_on_focus_true = all { state == Running, enter(Paused, OnFocusTrue) }
  action running_to_paused_on_hover_true = all { state == Running, enter(Paused, OnHoverTrue) }
  action running_to_running_on_props = all { state == Running, enter(Running, OnProps) }
  action running_to_running_on_tick = all { state == Running, enter(Running, OnTick) }
  action empty_stays_on_focus_false = all { state == Empty, state' = state, running' = running, last' = OnFocusFalse }
  action empty_stays_on_focus_true = all { state == Empty, state' = state, running' = running, last' = OnFocusTrue }
  action empty_stays_on_hover_false = all { state == Empty, state' = state, running' = running, last' = OnHoverFalse }
  action empty_stays_on_hover_true = all { state == Empty, state' = state, running' = running, last' = OnHoverTrue }
  action expired_stays_on_close_a = all { state == Expired, state' = state, running' = running, last' = OnCloseA }
  action expired_stays_on_close_b = all { state == Expired, state' = state, running' = running, last' = OnCloseB }
  action expired_stays_on_focus_false = all { state == Expired, state' = state, running' = running, last' = OnFocusFalse }
  action expired_stays_on_focus_true = all { state == Expired, state' = state, running' = running, last' = OnFocusTrue }
  action expired_stays_on_hover_false = all { state == Expired, state' = state, running' = running, last' = OnHoverFalse }
  action expired_stays_on_hover_true = all { state == Expired, state' = state, running' = running, last' = OnHoverTrue }
  action paused_stays_on_close_a = all { state == Paused, state' = state, running' = running, last' = OnCloseA }
  action paused_stays_on_close_b = all { state == Paused, state' = state, running' = running, last' = OnCloseB }
  action paused_stays_on_focus_false = all { state == Paused, state' = state, running' = running, last' = OnFocusFalse }
  action paused_stays_on_focus_true = all { state == Paused, state' = state, running' = running, last' = OnFocusTrue }
  action paused_stays_on_hover_false = all { state == Paused, state' = state, running' = running, last' = OnHoverFalse }
  action paused_stays_on_hover_true = all { state == Paused, state' = state, running' = running, last' = OnHoverTrue }
  action paused_stays_on_props = all { state == Paused, state' = state, running' = running, last' = OnProps }
  action running_stays_on_close_a = all { state == Running, state' = state, running' = running, last' = OnCloseA }
  action running_stays_on_close_b = all { state == Running, state' = state, running' = running, last' = OnCloseB }
  action running_stays_on_focus_false = all { state == Running, state' = state, running' = running, last' = OnFocusFalse }
  action running_stays_on_hover_false = all { state == Running, state' = state, running' = running, last' = OnHoverFalse }

  action step = any {
    empty_to_paused_on_props,
    empty_to_running_on_props,
    expired_to_empty_on_props,
    expired_to_paused_on_props,
    expired_to_running_on_props,
    paused_to_empty_on_props,
    paused_to_running_on_close_a,
    paused_to_running_on_close_b,
    paused_to_running_on_focus_false,
    paused_to_running_on_hover_false,
    running_to_empty_on_props,
    running_to_expired_on_tick,
    running_to_paused_on_focus_true,
    running_to_paused_on_hover_true,
    running_to_running_on_props,
    running_to_running_on_tick,
    empty_stays_on_focus_false,
    empty_stays_on_focus_true,
    empty_stays_on_hover_false,
    empty_stays_on_hover_true,
    expired_stays_on_close_a,
    expired_stays_on_close_b,
    expired_stays_on_focus_false,
    expired_stays_on_focus_true,
    expired_stays_on_hover_false,
    expired_stays_on_hover_true,
    paused_stays_on_close_a,
    paused_stays_on_close_b,
    paused_stays_on_focus_false,
    paused_stays_on_focus_true,
    paused_stays_on_hover_false,
    paused_stays_on_hover_true,
    paused_stays_on_props,
    running_stays_on_close_a,
    running_stays_on_close_b,
    running_stays_on_focus_false,
    running_stays_on_hover_false,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
