// Generated from the spec of the machine "filter-popover". Do not edit.
module filter_popover {
  type State = | Closed | Open
  type Event = OnStart | OnAny | OnApply | OnCloseRequestClosePress | OnCloseRequestEscapeKey | OnCloseRequestOutsidePress | OnCloseRequestTriggerPress | OnOpenRequestTriggerPress | OnProps | OnQuery

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Closed => Set()
    | Open => Set("open/hold dismiss", "open/hold anchor")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Closed, Open)

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

  action closed_to_open_on_open_request_trigger_press = all { state == Closed, enter(Open, OnOpenRequestTriggerPress) }
  action closed_to_open_on_props = all { state == Closed, enter(Open, OnProps) }
  action open_to_closed_on_apply = all { state == Open, enter(Closed, OnApply) }
  action open_to_closed_on_close_request_close_press = all { state == Open, enter(Closed, OnCloseRequestClosePress) }
  action open_to_closed_on_close_request_escape_key = all { state == Open, enter(Closed, OnCloseRequestEscapeKey) }
  action open_to_closed_on_close_request_outside_press = all { state == Open, enter(Closed, OnCloseRequestOutsidePress) }
  action open_to_closed_on_close_request_trigger_press = all { state == Open, enter(Closed, OnCloseRequestTriggerPress) }
  action open_to_closed_on_props = all { state == Open, enter(Closed, OnProps) }
  action open_to_open_on_props = all { state == Open, enter(Open, OnProps) }
  action open_stays_on_query = all { state == Open, state' = state, running' = running, last' = OnQuery }

  action step = any {
    closed_to_open_on_open_request_trigger_press,
    closed_to_open_on_props,
    open_to_closed_on_apply,
    open_to_closed_on_close_request_close_press,
    open_to_closed_on_close_request_escape_key,
    open_to_closed_on_close_request_outside_press,
    open_to_closed_on_close_request_trigger_press,
    open_to_closed_on_props,
    open_to_open_on_props,
    open_stays_on_query,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
