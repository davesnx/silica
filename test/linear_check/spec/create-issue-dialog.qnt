// Generated from the spec of the machine "create-issue-dialog". Do not edit.
module create_issue_dialog {
  type State = | Idle | Closed | Open
  type Event = OnStart | OnAny | OnCancel | OnCloseEscapeKey | OnCloseOutsidePress | OnPropsBusy | OnPropsClosed | OnPropsOpen | OnSubmit

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Closed => Set()
    | Open => Set("open/hold focus_trap", "open/hold scroll_lock", "open/hold dismiss")
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

  action closed_to_open_on_props_busy = all { state == Closed, enter(Open, OnPropsBusy) }
  action closed_to_open_on_props_open = all { state == Closed, enter(Open, OnPropsOpen) }
  action idle_to_closed_on_props_closed = all { state == Idle, enter(Closed, OnPropsClosed) }
  action idle_to_open_on_props_busy = all { state == Idle, enter(Open, OnPropsBusy) }
  action idle_to_open_on_props_open = all { state == Idle, enter(Open, OnPropsOpen) }
  action open_to_closed_on_close_escape_key = all { state == Open, enter(Closed, OnCloseEscapeKey) }
  action open_to_closed_on_close_outside_press = all { state == Open, enter(Closed, OnCloseOutsidePress) }
  action open_to_closed_on_props_closed = all { state == Open, enter(Closed, OnPropsClosed) }
  action open_to_open_on_any = all { state == Open, enter(Open, OnAny) }
  action closed_stays_on_props_busy = all { state == Closed, state' = state, running' = running, last' = OnPropsBusy }
  action closed_stays_on_props_closed = all { state == Closed, state' = state, running' = running, last' = OnPropsClosed }
  action closed_stays_on_props_open = all { state == Closed, state' = state, running' = running, last' = OnPropsOpen }
  action open_stays_on_cancel = all { state == Open, state' = state, running' = running, last' = OnCancel }
  action open_stays_on_props_busy = all { state == Open, state' = state, running' = running, last' = OnPropsBusy }
  action open_stays_on_props_open = all { state == Open, state' = state, running' = running, last' = OnPropsOpen }
  action open_stays_on_submit = all { state == Open, state' = state, running' = running, last' = OnSubmit }

  action step = any {
    closed_to_open_on_props_busy,
    closed_to_open_on_props_open,
    idle_to_closed_on_props_closed,
    idle_to_open_on_props_busy,
    idle_to_open_on_props_open,
    open_to_closed_on_close_escape_key,
    open_to_closed_on_close_outside_press,
    open_to_closed_on_props_closed,
    open_to_open_on_any,
    closed_stays_on_props_busy,
    closed_stays_on_props_closed,
    closed_stays_on_props_open,
    open_stays_on_cancel,
    open_stays_on_props_busy,
    open_stays_on_props_open,
    open_stays_on_submit,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
