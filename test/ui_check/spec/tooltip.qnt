// Generated from the spec of the machine "tooltip". Do not edit.
module tooltip {
  type State = | Closed | Closing | Open | Opening
  type Event = OnStart | OnAny | OnCancel | OnCloseEscapeKey | OnCloseTriggerFocus | OnCloseTriggerPress | OnElapsed0 | OnElapsed300 | OnElapsed600 | OnFocus | OnHover | OnLeave | OnProps | OnResume

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Closed => Set()
    | Closing => Set("closing/after 300ms", "closing/hold dismiss", "closing/hold anchor")
    | Open => Set("open/hold dismiss", "open/hold anchor")
    | Opening => Set("opening/after 600ms")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Closed, Closing, Open, Opening)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Closed,
    running' = owned(Closed),
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
  action closing_to_open_on_resume = all { state == Closing, enter(Open, OnResume) }
  action open_to_closed_on_close_escape_key = all { state == Open, enter(Closed, OnCloseEscapeKey) }
  action open_to_closed_on_close_trigger_focus = all { state == Open, enter(Closed, OnCloseTriggerFocus) }
  action open_to_closed_on_close_trigger_press = all { state == Open, enter(Closed, OnCloseTriggerPress) }
  action open_to_closed_on_leave = all { state == Open, enter(Closed, OnLeave) }
  action open_to_closing_on_leave = all { state == Open, enter(Closing, OnLeave) }
  action opening_to_closed_on_cancel = all { state == Opening, enter(Closed, OnCancel) }
  action opening_to_open_on_elapsed_0 = all { state == Opening, enter(Open, OnElapsed0) }
  action opening_to_open_on_elapsed_600 = all { state == Opening, enter(Open, OnElapsed600) }
  action closing_stays_on_props = all { state == Closing, state' = state, running' = running, last' = OnProps }
  action opening_stays_on_props = all { state == Opening, state' = state, running' = running, last' = OnProps }

  action step = any {
    closed_to_open_on_focus,
    closed_to_open_on_hover,
    closed_to_opening_on_hover,
    closing_to_closed_on_close_escape_key,
    closing_to_closed_on_close_trigger_focus,
    closing_to_closed_on_close_trigger_press,
    closing_to_closed_on_elapsed_0,
    closing_to_closed_on_elapsed_300,
    closing_to_open_on_resume,
    open_to_closed_on_close_escape_key,
    open_to_closed_on_close_trigger_focus,
    open_to_closed_on_close_trigger_press,
    open_to_closed_on_leave,
    open_to_closing_on_leave,
    opening_to_closed_on_cancel,
    opening_to_open_on_elapsed_0,
    opening_to_open_on_elapsed_600,
    closing_stays_on_props,
    opening_stays_on_props,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
