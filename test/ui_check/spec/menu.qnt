// Generated from the spec of the machine "menu". Do not edit.
module menu {
  type State = | Closed | Open
  type Event = OnStart | OnAny | OnChoose | OnCloseEscapeKey | OnCloseFocusOut | OnCloseOutsidePress | OnHighlight | OnMoveFirst | OnMoveLast | OnMoveLetter | OnMoveNext | OnMovePrevious | OnOpenFirstListNavigation | OnOpenFirstTriggerPress | OnOpenLastListNavigation | OnProps | OnTriggerPress

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

  action closed_to_open_on_open_first_list_navigation = all { state == Closed, enter(Open, OnOpenFirstListNavigation) }
  action closed_to_open_on_open_first_trigger_press = all { state == Closed, enter(Open, OnOpenFirstTriggerPress) }
  action closed_to_open_on_open_last_list_navigation = all { state == Closed, enter(Open, OnOpenLastListNavigation) }
  action closed_to_open_on_props = all { state == Closed, enter(Open, OnProps) }
  action closed_to_open_on_trigger_press = all { state == Closed, enter(Open, OnTriggerPress) }
  action open_to_closed_on_choose = all { state == Open, enter(Closed, OnChoose) }
  action open_to_closed_on_close_escape_key = all { state == Open, enter(Closed, OnCloseEscapeKey) }
  action open_to_closed_on_close_focus_out = all { state == Open, enter(Closed, OnCloseFocusOut) }
  action open_to_closed_on_close_outside_press = all { state == Open, enter(Closed, OnCloseOutsidePress) }
  action open_to_closed_on_props = all { state == Open, enter(Closed, OnProps) }
  action open_to_closed_on_trigger_press = all { state == Open, enter(Closed, OnTriggerPress) }
  action open_stays_on_highlight = all { state == Open, state' = state, running' = running, last' = OnHighlight }
  action open_stays_on_move_first = all { state == Open, state' = state, running' = running, last' = OnMoveFirst }
  action open_stays_on_move_last = all { state == Open, state' = state, running' = running, last' = OnMoveLast }
  action open_stays_on_move_letter = all { state == Open, state' = state, running' = running, last' = OnMoveLetter }
  action open_stays_on_move_next = all { state == Open, state' = state, running' = running, last' = OnMoveNext }
  action open_stays_on_move_previous = all { state == Open, state' = state, running' = running, last' = OnMovePrevious }

  action step = any {
    closed_to_open_on_open_first_list_navigation,
    closed_to_open_on_open_first_trigger_press,
    closed_to_open_on_open_last_list_navigation,
    closed_to_open_on_props,
    closed_to_open_on_trigger_press,
    open_to_closed_on_choose,
    open_to_closed_on_close_escape_key,
    open_to_closed_on_close_focus_out,
    open_to_closed_on_close_outside_press,
    open_to_closed_on_props,
    open_to_closed_on_trigger_press,
    open_stays_on_highlight,
    open_stays_on_move_first,
    open_stays_on_move_last,
    open_stays_on_move_letter,
    open_stays_on_move_next,
    open_stays_on_move_previous,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
