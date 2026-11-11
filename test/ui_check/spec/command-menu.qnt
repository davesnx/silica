// Generated from the spec of the machine "command-menu". Do not edit.
module command_menu {
  type State = | Closed | Open
  type Event = OnStart | OnAny | OnCloseEscapeKey | OnCloseOutsidePress | OnProps | OnRun

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
  action open_to_closed_on_close_escape_key = all { state == Open, enter(Closed, OnCloseEscapeKey) }
  action open_to_closed_on_close_outside_press = all { state == Open, enter(Closed, OnCloseOutsidePress) }
  action open_to_closed_on_props = all { state == Open, enter(Closed, OnProps) }
  action open_to_closed_on_run = all { state == Open, enter(Closed, OnRun) }

  action step = any {
    closed_to_open_on_props,
    open_to_closed_on_close_escape_key,
    open_to_closed_on_close_outside_press,
    open_to_closed_on_props,
    open_to_closed_on_run,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
