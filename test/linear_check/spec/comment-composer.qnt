// Generated from the spec of the machine "comment-composer". Do not edit.
module comment_composer {
  type State = | Empty | Drafting | Sending | Failed
  type Event = OnStart | OnAny | OnClear | OnProps | OnSend | OnSentError | OnSentOk | OnTyped

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Empty => Set()
    | Drafting => Set()
    | Sending => Set("sending/invoke /api/comments/add")
    | Failed => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Empty, Drafting, Sending, Failed)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Empty,
    running' = owned(Empty),
    last' = OnStart,
  }

  action drafting_to_empty_on_clear = all { state == Drafting, enter(Empty, OnClear) }
  action drafting_to_empty_on_props = all { state == Drafting, enter(Empty, OnProps) }
  action drafting_to_empty_on_typed = all { state == Drafting, enter(Empty, OnTyped) }
  action drafting_to_sending_on_send = all { state == Drafting, enter(Sending, OnSend) }
  action empty_to_drafting_on_typed = all { state == Empty, enter(Drafting, OnTyped) }
  action empty_to_empty_on_any = all { state == Empty, enter(Empty, OnAny) }
  action failed_to_drafting_on_typed = all { state == Failed, enter(Drafting, OnTyped) }
  action failed_to_empty_on_clear = all { state == Failed, enter(Empty, OnClear) }
  action failed_to_empty_on_props = all { state == Failed, enter(Empty, OnProps) }
  action failed_to_empty_on_typed = all { state == Failed, enter(Empty, OnTyped) }
  action failed_to_sending_on_send = all { state == Failed, enter(Sending, OnSend) }
  action sending_to_empty_on_props = all { state == Sending, enter(Empty, OnProps) }
  action sending_to_empty_on_sent_ok = all { state == Sending, enter(Empty, OnSentOk) }
  action sending_to_failed_on_sent_error = all { state == Sending, enter(Failed, OnSentError) }
  action sending_to_sending_on_any = all { state == Sending, enter(Sending, OnAny) }
  action drafting_stays_on_typed = all { state == Drafting, state' = state, running' = running, last' = OnTyped }
  action empty_stays_on_props = all { state == Empty, state' = state, running' = running, last' = OnProps }
  action empty_stays_on_typed = all { state == Empty, state' = state, running' = running, last' = OnTyped }

  action step = any {
    drafting_to_empty_on_clear,
    drafting_to_empty_on_props,
    drafting_to_empty_on_typed,
    drafting_to_sending_on_send,
    empty_to_drafting_on_typed,
    empty_to_empty_on_any,
    failed_to_drafting_on_typed,
    failed_to_empty_on_clear,
    failed_to_empty_on_props,
    failed_to_empty_on_typed,
    failed_to_sending_on_send,
    sending_to_empty_on_props,
    sending_to_empty_on_sent_ok,
    sending_to_failed_on_sent_error,
    sending_to_sending_on_any,
    drafting_stays_on_typed,
    empty_stays_on_props,
    empty_stays_on_typed,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
