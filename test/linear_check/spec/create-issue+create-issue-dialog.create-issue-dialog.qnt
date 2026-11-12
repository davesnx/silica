// Generated from the spec of the machine "create-issue-dialog". Do not edit.
module create_issue_dialog {
  type State = | Closed | Open
  type Event = OnStart | OnAny | OnCancel | OnCloseRequestEscapeKey | OnCloseRequestOutsidePress | OnDescription | OnProps | OnSubmit | OnTitle

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Closed => Set()
    | Open => Set("open/hold focus_trap", "open/hold scroll_lock", "open/hold dismiss")
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

  action closed_to_open_on_props = all { state == Closed, enter(Open, OnProps) }
  action open_to_closed_on_close_request_escape_key = all { state == Open, enter(Closed, OnCloseRequestEscapeKey) }
  action open_to_closed_on_close_request_outside_press = all { state == Open, enter(Closed, OnCloseRequestOutsidePress) }
  action open_to_closed_on_props = all { state == Open, enter(Closed, OnProps) }
  action open_stays_on_cancel = all { state == Open, state' = state, running' = running, last' = OnCancel }
  action open_stays_on_close_request_escape_key = all { state == Open, state' = state, running' = running, last' = OnCloseRequestEscapeKey }
  action open_stays_on_close_request_outside_press = all { state == Open, state' = state, running' = running, last' = OnCloseRequestOutsidePress }
  action open_stays_on_description = all { state == Open, state' = state, running' = running, last' = OnDescription }
  action open_stays_on_submit = all { state == Open, state' = state, running' = running, last' = OnSubmit }
  action open_stays_on_title = all { state == Open, state' = state, running' = running, last' = OnTitle }

  action step = any {
    closed_to_open_on_props,
    open_to_closed_on_close_request_escape_key,
    open_to_closed_on_close_request_outside_press,
    open_to_closed_on_props,
    open_stays_on_cancel,
    open_stays_on_close_request_escape_key,
    open_stays_on_close_request_outside_press,
    open_stays_on_description,
    open_stays_on_submit,
    open_stays_on_title,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
