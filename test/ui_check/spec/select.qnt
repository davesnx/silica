// Generated from the spec of the machine "select". Do not edit.
module select {
  type State = | Idle | Closed | Open
  type Event = OnStart | OnAny | OnCloseEscapeKey | OnCloseFocusOut | OnCloseItemPress | OnCloseOutsidePress | OnHighlight | OnMoveFirst | OnMoveLast | OnMoveLetter | OnMoveNext | OnMovePrevious | OnOpenListNavigation | OnOpenTriggerPress | OnProps | OnPropsValue | OnSelectFocusOut | OnSelectItemPress | OnTriggerPress

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

  action closed_to_open_on_open_list_navigation = all { state == Closed, enter(Open, OnOpenListNavigation) }
  action closed_to_open_on_open_trigger_press = all { state == Closed, enter(Open, OnOpenTriggerPress) }
  action closed_to_open_on_trigger_press = all { state == Closed, enter(Open, OnTriggerPress) }
  action idle_to_closed_on_props = all { state == Idle, enter(Closed, OnProps) }
  action idle_to_closed_on_props_value = all { state == Idle, enter(Closed, OnPropsValue) }
  action open_to_closed_on_close_escape_key = all { state == Open, enter(Closed, OnCloseEscapeKey) }
  action open_to_closed_on_close_focus_out = all { state == Open, enter(Closed, OnCloseFocusOut) }
  action open_to_closed_on_close_item_press = all { state == Open, enter(Closed, OnCloseItemPress) }
  action open_to_closed_on_close_outside_press = all { state == Open, enter(Closed, OnCloseOutsidePress) }
  action open_to_closed_on_select_focus_out = all { state == Open, enter(Closed, OnSelectFocusOut) }
  action open_to_closed_on_select_item_press = all { state == Open, enter(Closed, OnSelectItemPress) }
  action open_to_closed_on_trigger_press = all { state == Open, enter(Closed, OnTriggerPress) }
  action open_to_open_on_any = all { state == Open, enter(Open, OnAny) }
  action closed_stays_on_props = all { state == Closed, state' = state, running' = running, last' = OnProps }
  action closed_stays_on_props_value = all { state == Closed, state' = state, running' = running, last' = OnPropsValue }
  action open_stays_on_highlight = all { state == Open, state' = state, running' = running, last' = OnHighlight }
  action open_stays_on_move_first = all { state == Open, state' = state, running' = running, last' = OnMoveFirst }
  action open_stays_on_move_last = all { state == Open, state' = state, running' = running, last' = OnMoveLast }
  action open_stays_on_move_letter = all { state == Open, state' = state, running' = running, last' = OnMoveLetter }
  action open_stays_on_move_next = all { state == Open, state' = state, running' = running, last' = OnMoveNext }
  action open_stays_on_move_previous = all { state == Open, state' = state, running' = running, last' = OnMovePrevious }
  action open_stays_on_props = all { state == Open, state' = state, running' = running, last' = OnProps }
  action open_stays_on_props_value = all { state == Open, state' = state, running' = running, last' = OnPropsValue }

  action step = any {
    closed_to_open_on_open_list_navigation,
    closed_to_open_on_open_trigger_press,
    closed_to_open_on_trigger_press,
    idle_to_closed_on_props,
    idle_to_closed_on_props_value,
    open_to_closed_on_close_escape_key,
    open_to_closed_on_close_focus_out,
    open_to_closed_on_close_item_press,
    open_to_closed_on_close_outside_press,
    open_to_closed_on_select_focus_out,
    open_to_closed_on_select_item_press,
    open_to_closed_on_trigger_press,
    open_to_open_on_any,
    closed_stays_on_props,
    closed_stays_on_props_value,
    open_stays_on_highlight,
    open_stays_on_move_first,
    open_stays_on_move_last,
    open_stays_on_move_letter,
    open_stays_on_move_next,
    open_stays_on_move_previous,
    open_stays_on_props,
    open_stays_on_props_value,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
