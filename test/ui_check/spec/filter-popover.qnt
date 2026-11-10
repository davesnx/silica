// Generated from the spec of the machine "filter-popover". Do not edit.
module filter_popover {
  type State = | Idle | Closed | Open
  type Event = OnStart | OnAny | OnApply | OnCloseClosePress | OnCloseEscapeKey | OnCloseOutsidePress | OnCloseTriggerPress | OnOpenTriggerPress | OnProps | OnPropsClosed | OnPropsOpen | OnQuery

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Closed => Set()
    | Open => Set("open/hold dismiss", "open/hold anchor")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Closed, Open)

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

  action closed_to_open_on_open_trigger_press = all { state == Closed, enter(Open, OnOpenTriggerPress) }
  action closed_to_open_on_props_open = all { state == Closed, enter(Open, OnPropsOpen) }
  action idle_to_closed_on_props = all { state == Idle, enter(Closed, OnProps) }
  action idle_to_closed_on_props_closed = all { state == Idle, enter(Closed, OnPropsClosed) }
  action idle_to_open_on_props_open = all { state == Idle, enter(Open, OnPropsOpen) }
  action open_to_closed_on_apply = all { state == Open, enter(Closed, OnApply) }
  action open_to_closed_on_close_close_press = all { state == Open, enter(Closed, OnCloseClosePress) }
  action open_to_closed_on_close_escape_key = all { state == Open, enter(Closed, OnCloseEscapeKey) }
  action open_to_closed_on_close_outside_press = all { state == Open, enter(Closed, OnCloseOutsidePress) }
  action open_to_closed_on_close_trigger_press = all { state == Open, enter(Closed, OnCloseTriggerPress) }
  action open_to_closed_on_props_closed = all { state == Open, enter(Closed, OnPropsClosed) }
  action open_to_open_on_props = all { state == Open, enter(Open, OnProps) }
  action open_to_open_on_props_open = all { state == Open, enter(Open, OnPropsOpen) }
  action closed_stays_on_props = all { state == Closed, state' = state, running' = running, last' = OnProps }
  action closed_stays_on_props_closed = all { state == Closed, state' = state, running' = running, last' = OnPropsClosed }
  action closed_stays_on_props_open = all { state == Closed, state' = state, running' = running, last' = OnPropsOpen }
  action open_stays_on_props = all { state == Open, state' = state, running' = running, last' = OnProps }
  action open_stays_on_props_closed = all { state == Open, state' = state, running' = running, last' = OnPropsClosed }
  action open_stays_on_props_open = all { state == Open, state' = state, running' = running, last' = OnPropsOpen }
  action open_stays_on_query = all { state == Open, state' = state, running' = running, last' = OnQuery }

  action step = any {
    closed_to_open_on_open_trigger_press,
    closed_to_open_on_props_open,
    idle_to_closed_on_props,
    idle_to_closed_on_props_closed,
    idle_to_open_on_props_open,
    open_to_closed_on_apply,
    open_to_closed_on_close_close_press,
    open_to_closed_on_close_escape_key,
    open_to_closed_on_close_outside_press,
    open_to_closed_on_close_trigger_press,
    open_to_closed_on_props_closed,
    open_to_open_on_props,
    open_to_open_on_props_open,
    closed_stays_on_props,
    closed_stays_on_props_closed,
    closed_stays_on_props_open,
    open_stays_on_props,
    open_stays_on_props_closed,
    open_stays_on_props_open,
    open_stays_on_query,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
