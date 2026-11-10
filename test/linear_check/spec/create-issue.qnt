// Generated from the spec of the machine "create-issue". Do not edit.
module create_issue {
  type State = | Closed | Editing | Submitting | Failed
  type Event = OnStart | OnAny | OnCancel | OnCreatedError | OnCreatedOk | OnDescription | OnPriority | OnProps | OnStatus | OnSubmit | OnTitle

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Closed => Set()
    | Editing => Set()
    | Submitting => Set("submitting/invoke /api/issues/create")
    | Failed => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Closed, Editing, Submitting, Failed)

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

  action closed_to_editing_on_props = all { state == Closed, enter(Editing, OnProps) }
  action editing_to_closed_on_cancel = all { state == Editing, enter(Closed, OnCancel) }
  action editing_to_closed_on_props = all { state == Editing, enter(Closed, OnProps) }
  action editing_to_submitting_on_submit = all { state == Editing, enter(Submitting, OnSubmit) }
  action failed_to_closed_on_cancel = all { state == Failed, enter(Closed, OnCancel) }
  action failed_to_closed_on_props = all { state == Failed, enter(Closed, OnProps) }
  action failed_to_editing_on_description = all { state == Failed, enter(Editing, OnDescription) }
  action failed_to_editing_on_priority = all { state == Failed, enter(Editing, OnPriority) }
  action failed_to_editing_on_status = all { state == Failed, enter(Editing, OnStatus) }
  action failed_to_editing_on_title = all { state == Failed, enter(Editing, OnTitle) }
  action failed_to_submitting_on_submit = all { state == Failed, enter(Submitting, OnSubmit) }
  action submitting_to_closed_on_created_ok = all { state == Submitting, enter(Closed, OnCreatedOk) }
  action submitting_to_failed_on_created_error = all { state == Submitting, enter(Failed, OnCreatedError) }
  action submitting_to_submitting_on_any = all { state == Submitting, enter(Submitting, OnAny) }
  action closed_stays_on_props = all { state == Closed, state' = state, running' = running, last' = OnProps }
  action editing_stays_on_description = all { state == Editing, state' = state, running' = running, last' = OnDescription }
  action editing_stays_on_priority = all { state == Editing, state' = state, running' = running, last' = OnPriority }
  action editing_stays_on_props = all { state == Editing, state' = state, running' = running, last' = OnProps }
  action editing_stays_on_status = all { state == Editing, state' = state, running' = running, last' = OnStatus }
  action editing_stays_on_submit = all { state == Editing, state' = state, running' = running, last' = OnSubmit }
  action editing_stays_on_title = all { state == Editing, state' = state, running' = running, last' = OnTitle }
  action failed_stays_on_props = all { state == Failed, state' = state, running' = running, last' = OnProps }

  action step = any {
    closed_to_editing_on_props,
    editing_to_closed_on_cancel,
    editing_to_closed_on_props,
    editing_to_submitting_on_submit,
    failed_to_closed_on_cancel,
    failed_to_closed_on_props,
    failed_to_editing_on_description,
    failed_to_editing_on_priority,
    failed_to_editing_on_status,
    failed_to_editing_on_title,
    failed_to_submitting_on_submit,
    submitting_to_closed_on_created_ok,
    submitting_to_failed_on_created_error,
    submitting_to_submitting_on_any,
    closed_stays_on_props,
    editing_stays_on_description,
    editing_stays_on_priority,
    editing_stays_on_props,
    editing_stays_on_status,
    editing_stays_on_submit,
    editing_stays_on_title,
    failed_stays_on_props,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
