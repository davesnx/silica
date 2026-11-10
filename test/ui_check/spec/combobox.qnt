// Generated from the spec of the machine "combobox". Do not edit.
module combobox {
  type State = | Idle | Closed | Open | Inline
  type Event = OnStart | OnAny | OnEnter | OnEscape | OnFirst | OnFocusOut | OnHighlight | OnLast | OnNext | OnOutside | OnPress | OnPrevious | OnProps | OnPropsInline | OnShow | OnType

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Closed => Set()
    | Open => Set("open/hold dismiss", "open/hold anchor")
    | Inline => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Closed, Open, Inline)

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

  action closed_to_inline_on_props_inline = all { state == Closed, enter(Inline, OnPropsInline) }
  action closed_to_open_on_next = all { state == Closed, enter(Open, OnNext) }
  action closed_to_open_on_previous = all { state == Closed, enter(Open, OnPrevious) }
  action closed_to_open_on_show = all { state == Closed, enter(Open, OnShow) }
  action closed_to_open_on_type = all { state == Closed, enter(Open, OnType) }
  action idle_to_closed_on_props = all { state == Idle, enter(Closed, OnProps) }
  action idle_to_inline_on_props_inline = all { state == Idle, enter(Inline, OnPropsInline) }
  action inline_to_closed_on_props = all { state == Inline, enter(Closed, OnProps) }
  action open_to_closed_on_enter = all { state == Open, enter(Closed, OnEnter) }
  action open_to_closed_on_escape = all { state == Open, enter(Closed, OnEscape) }
  action open_to_closed_on_focus_out = all { state == Open, enter(Closed, OnFocusOut) }
  action open_to_closed_on_outside = all { state == Open, enter(Closed, OnOutside) }
  action open_to_closed_on_press = all { state == Open, enter(Closed, OnPress) }
  action open_to_inline_on_props_inline = all { state == Open, enter(Inline, OnPropsInline) }
  action open_to_open_on_any = all { state == Open, enter(Open, OnAny) }
  action closed_stays_on_escape = all { state == Closed, state' = state, running' = running, last' = OnEscape }
  action closed_stays_on_props = all { state == Closed, state' = state, running' = running, last' = OnProps }
  action inline_stays_on_enter = all { state == Inline, state' = state, running' = running, last' = OnEnter }
  action inline_stays_on_escape = all { state == Inline, state' = state, running' = running, last' = OnEscape }
  action inline_stays_on_first = all { state == Inline, state' = state, running' = running, last' = OnFirst }
  action inline_stays_on_highlight = all { state == Inline, state' = state, running' = running, last' = OnHighlight }
  action inline_stays_on_last = all { state == Inline, state' = state, running' = running, last' = OnLast }
  action inline_stays_on_next = all { state == Inline, state' = state, running' = running, last' = OnNext }
  action inline_stays_on_press = all { state == Inline, state' = state, running' = running, last' = OnPress }
  action inline_stays_on_previous = all { state == Inline, state' = state, running' = running, last' = OnPrevious }
  action inline_stays_on_props_inline = all { state == Inline, state' = state, running' = running, last' = OnPropsInline }
  action inline_stays_on_type = all { state == Inline, state' = state, running' = running, last' = OnType }
  action open_stays_on_first = all { state == Open, state' = state, running' = running, last' = OnFirst }
  action open_stays_on_highlight = all { state == Open, state' = state, running' = running, last' = OnHighlight }
  action open_stays_on_last = all { state == Open, state' = state, running' = running, last' = OnLast }
  action open_stays_on_next = all { state == Open, state' = state, running' = running, last' = OnNext }
  action open_stays_on_previous = all { state == Open, state' = state, running' = running, last' = OnPrevious }
  action open_stays_on_props = all { state == Open, state' = state, running' = running, last' = OnProps }
  action open_stays_on_type = all { state == Open, state' = state, running' = running, last' = OnType }

  action step = any {
    closed_to_inline_on_props_inline,
    closed_to_open_on_next,
    closed_to_open_on_previous,
    closed_to_open_on_show,
    closed_to_open_on_type,
    idle_to_closed_on_props,
    idle_to_inline_on_props_inline,
    inline_to_closed_on_props,
    open_to_closed_on_enter,
    open_to_closed_on_escape,
    open_to_closed_on_focus_out,
    open_to_closed_on_outside,
    open_to_closed_on_press,
    open_to_inline_on_props_inline,
    open_to_open_on_any,
    closed_stays_on_escape,
    closed_stays_on_props,
    inline_stays_on_enter,
    inline_stays_on_escape,
    inline_stays_on_first,
    inline_stays_on_highlight,
    inline_stays_on_last,
    inline_stays_on_next,
    inline_stays_on_press,
    inline_stays_on_previous,
    inline_stays_on_props_inline,
    inline_stays_on_type,
    open_stays_on_first,
    open_stays_on_highlight,
    open_stays_on_last,
    open_stays_on_next,
    open_stays_on_previous,
    open_stays_on_props,
    open_stays_on_type,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
