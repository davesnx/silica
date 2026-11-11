// Generated from the spec of the machine "combobox". Do not edit.
module combobox {
  type State = | Closed | Inline | Open
  type Event = OnStart | OnAny | OnChoose | OnChooseHighlighted | OnEscape | OnFocusOut | OnHighlight | OnMoveFirst | OnMoveLast | OnMoveNext | OnMovePrevious | OnOutsidePress | OnProps | OnShow | OnTyped

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Closed => Set()
    | Inline => Set()
    | Open => Set("open/hold dismiss", "open/hold anchor")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Closed, Inline, Open)

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

  action closed_to_inline_on_props = all { state == Closed, enter(Inline, OnProps) }
  action closed_to_open_on_move_next = all { state == Closed, enter(Open, OnMoveNext) }
  action closed_to_open_on_move_previous = all { state == Closed, enter(Open, OnMovePrevious) }
  action closed_to_open_on_show = all { state == Closed, enter(Open, OnShow) }
  action closed_to_open_on_typed = all { state == Closed, enter(Open, OnTyped) }
  action inline_to_closed_on_props = all { state == Inline, enter(Closed, OnProps) }
  action open_to_closed_on_choose = all { state == Open, enter(Closed, OnChoose) }
  action open_to_closed_on_choose_highlighted = all { state == Open, enter(Closed, OnChooseHighlighted) }
  action open_to_closed_on_escape = all { state == Open, enter(Closed, OnEscape) }
  action open_to_closed_on_focus_out = all { state == Open, enter(Closed, OnFocusOut) }
  action open_to_closed_on_outside_press = all { state == Open, enter(Closed, OnOutsidePress) }
  action open_to_inline_on_props = all { state == Open, enter(Inline, OnProps) }
  action closed_stays_on_escape = all { state == Closed, state' = state, running' = running, last' = OnEscape }
  action closed_stays_on_props = all { state == Closed, state' = state, running' = running, last' = OnProps }
  action inline_stays_on_choose = all { state == Inline, state' = state, running' = running, last' = OnChoose }
  action inline_stays_on_choose_highlighted = all { state == Inline, state' = state, running' = running, last' = OnChooseHighlighted }
  action inline_stays_on_escape = all { state == Inline, state' = state, running' = running, last' = OnEscape }
  action inline_stays_on_highlight = all { state == Inline, state' = state, running' = running, last' = OnHighlight }
  action inline_stays_on_move_first = all { state == Inline, state' = state, running' = running, last' = OnMoveFirst }
  action inline_stays_on_move_last = all { state == Inline, state' = state, running' = running, last' = OnMoveLast }
  action inline_stays_on_move_next = all { state == Inline, state' = state, running' = running, last' = OnMoveNext }
  action inline_stays_on_move_previous = all { state == Inline, state' = state, running' = running, last' = OnMovePrevious }
  action inline_stays_on_typed = all { state == Inline, state' = state, running' = running, last' = OnTyped }
  action open_stays_on_highlight = all { state == Open, state' = state, running' = running, last' = OnHighlight }
  action open_stays_on_move_first = all { state == Open, state' = state, running' = running, last' = OnMoveFirst }
  action open_stays_on_move_last = all { state == Open, state' = state, running' = running, last' = OnMoveLast }
  action open_stays_on_move_next = all { state == Open, state' = state, running' = running, last' = OnMoveNext }
  action open_stays_on_move_previous = all { state == Open, state' = state, running' = running, last' = OnMovePrevious }
  action open_stays_on_props = all { state == Open, state' = state, running' = running, last' = OnProps }
  action open_stays_on_typed = all { state == Open, state' = state, running' = running, last' = OnTyped }

  action step = any {
    closed_to_inline_on_props,
    closed_to_open_on_move_next,
    closed_to_open_on_move_previous,
    closed_to_open_on_show,
    closed_to_open_on_typed,
    inline_to_closed_on_props,
    open_to_closed_on_choose,
    open_to_closed_on_choose_highlighted,
    open_to_closed_on_escape,
    open_to_closed_on_focus_out,
    open_to_closed_on_outside_press,
    open_to_inline_on_props,
    closed_stays_on_escape,
    closed_stays_on_props,
    inline_stays_on_choose,
    inline_stays_on_choose_highlighted,
    inline_stays_on_escape,
    inline_stays_on_highlight,
    inline_stays_on_move_first,
    inline_stays_on_move_last,
    inline_stays_on_move_next,
    inline_stays_on_move_previous,
    inline_stays_on_typed,
    open_stays_on_highlight,
    open_stays_on_move_first,
    open_stays_on_move_last,
    open_stays_on_move_next,
    open_stays_on_move_previous,
    open_stays_on_props,
    open_stays_on_typed,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
