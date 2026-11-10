// Generated from the spec of the machine "tooltip". Do not edit.
module tooltip {
  type State = | Idle | Closed | Opening | Open | Closing
  type Event = OnStart | OnAny | OnCancel | OnCloseEscapeKey | OnCloseTriggerFocus | OnCloseTriggerPress | OnElapsed0 | OnElapsed300 | OnElapsed600 | OnFocus | OnHover | OnLeave | OnProps0300 | OnProps6000 | OnResume

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Closed => Set()
    | Opening => Set("opening/after 0ms", "opening/after 300ms", "opening/after 600ms", "opening/after 1000ms")
    | Open => Set("open/hold dismiss", "open/hold anchor")
    | Closing => Set("closing/after 0ms", "closing/after 300ms", "closing/after 600ms", "closing/after 1000ms", "closing/hold dismiss", "closing/hold anchor")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Closed, Opening, Open, Closing)

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

  action closed_to_open_on_focus = all { state == Closed, enter(Open, OnFocus) }
  action closed_to_open_on_hover = all { state == Closed, enter(Open, OnHover) }
  action closed_to_opening_on_hover = all { state == Closed, enter(Opening, OnHover) }
  action closing_to_closed_on_close_escape_key = all { state == Closing, enter(Closed, OnCloseEscapeKey) }
  action closing_to_closed_on_close_trigger_focus = all { state == Closing, enter(Closed, OnCloseTriggerFocus) }
  action closing_to_closed_on_close_trigger_press = all { state == Closing, enter(Closed, OnCloseTriggerPress) }
  action closing_to_closed_on_elapsed_0 = all { state == Closing, enter(Closed, OnElapsed0) }
  action closing_to_closed_on_elapsed_300 = all { state == Closing, enter(Closed, OnElapsed300) }
  action closing_to_closing_on_props_0_300 = all { state == Closing, enter(Closing, OnProps0300) }
  action closing_to_closing_on_props_600_0 = all { state == Closing, enter(Closing, OnProps6000) }
  action closing_to_open_on_resume = all { state == Closing, enter(Open, OnResume) }
  action idle_to_closed_on_props_0_300 = all { state == Idle, enter(Closed, OnProps0300) }
  action idle_to_closed_on_props_600_0 = all { state == Idle, enter(Closed, OnProps6000) }
  action open_to_closed_on_close_escape_key = all { state == Open, enter(Closed, OnCloseEscapeKey) }
  action open_to_closed_on_close_trigger_focus = all { state == Open, enter(Closed, OnCloseTriggerFocus) }
  action open_to_closed_on_close_trigger_press = all { state == Open, enter(Closed, OnCloseTriggerPress) }
  action open_to_closed_on_leave = all { state == Open, enter(Closed, OnLeave) }
  action open_to_closing_on_leave = all { state == Open, enter(Closing, OnLeave) }
  action open_to_open_on_any = all { state == Open, enter(Open, OnAny) }
  action opening_to_closed_on_cancel = all { state == Opening, enter(Closed, OnCancel) }
  action opening_to_open_on_elapsed_0 = all { state == Opening, enter(Open, OnElapsed0) }
  action opening_to_open_on_elapsed_600 = all { state == Opening, enter(Open, OnElapsed600) }
  action opening_to_opening_on_props_0_300 = all { state == Opening, enter(Opening, OnProps0300) }
  action opening_to_opening_on_props_600_0 = all { state == Opening, enter(Opening, OnProps6000) }
  action closed_stays_on_props_0_300 = all { state == Closed, state' = state, running' = running, last' = OnProps0300 }
  action closed_stays_on_props_600_0 = all { state == Closed, state' = state, running' = running, last' = OnProps6000 }
  action closing_stays_on_props_0_300 = all { state == Closing, state' = state, running' = running, last' = OnProps0300 }
  action closing_stays_on_props_600_0 = all { state == Closing, state' = state, running' = running, last' = OnProps6000 }
  action open_stays_on_props_0_300 = all { state == Open, state' = state, running' = running, last' = OnProps0300 }
  action open_stays_on_props_600_0 = all { state == Open, state' = state, running' = running, last' = OnProps6000 }
  action opening_stays_on_props_0_300 = all { state == Opening, state' = state, running' = running, last' = OnProps0300 }
  action opening_stays_on_props_600_0 = all { state == Opening, state' = state, running' = running, last' = OnProps6000 }

  action step = any {
    closed_to_open_on_focus,
    closed_to_open_on_hover,
    closed_to_opening_on_hover,
    closing_to_closed_on_close_escape_key,
    closing_to_closed_on_close_trigger_focus,
    closing_to_closed_on_close_trigger_press,
    closing_to_closed_on_elapsed_0,
    closing_to_closed_on_elapsed_300,
    closing_to_closing_on_props_0_300,
    closing_to_closing_on_props_600_0,
    closing_to_open_on_resume,
    idle_to_closed_on_props_0_300,
    idle_to_closed_on_props_600_0,
    open_to_closed_on_close_escape_key,
    open_to_closed_on_close_trigger_focus,
    open_to_closed_on_close_trigger_press,
    open_to_closed_on_leave,
    open_to_closing_on_leave,
    open_to_open_on_any,
    opening_to_closed_on_cancel,
    opening_to_open_on_elapsed_0,
    opening_to_open_on_elapsed_600,
    opening_to_opening_on_props_0_300,
    opening_to_opening_on_props_600_0,
    closed_stays_on_props_0_300,
    closed_stays_on_props_600_0,
    closing_stays_on_props_0_300,
    closing_stays_on_props_600_0,
    open_stays_on_props_0_300,
    open_stays_on_props_600_0,
    opening_stays_on_props_0_300,
    opening_stays_on_props_600_0,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
