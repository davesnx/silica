// Generated from the spec of the machine "comment". Do not edit.
module comment {
  type State = | Idle | Viewing | Editing | Saving
  type Event = OnStart | OnAny | OnCancel | OnEdit | OnPropsOther | OnPropsOwn | OnSave | OnSavedError | OnSavedOk | OnTyped

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Viewing => Set()
    | Editing => Set()
    | Saving => Set("saving/invoke /api/comments/edit")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Viewing, Editing, Saving)

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

  action editing_to_saving_on_save = all { state == Editing, enter(Saving, OnSave) }
  action editing_to_viewing_on_cancel = all { state == Editing, enter(Viewing, OnCancel) }
  action editing_to_viewing_on_props_other = all { state == Editing, enter(Viewing, OnPropsOther) }
  action idle_to_viewing_on_props_other = all { state == Idle, enter(Viewing, OnPropsOther) }
  action idle_to_viewing_on_props_own = all { state == Idle, enter(Viewing, OnPropsOwn) }
  action saving_to_editing_on_saved_error = all { state == Saving, enter(Editing, OnSavedError) }
  action saving_to_saving_on_any = all { state == Saving, enter(Saving, OnAny) }
  action saving_to_viewing_on_props_other = all { state == Saving, enter(Viewing, OnPropsOther) }
  action saving_to_viewing_on_saved_ok = all { state == Saving, enter(Viewing, OnSavedOk) }
  action viewing_to_editing_on_edit = all { state == Viewing, enter(Editing, OnEdit) }
  action editing_stays_on_props_own = all { state == Editing, state' = state, running' = running, last' = OnPropsOwn }
  action editing_stays_on_save = all { state == Editing, state' = state, running' = running, last' = OnSave }
  action editing_stays_on_typed = all { state == Editing, state' = state, running' = running, last' = OnTyped }
  action saving_stays_on_props_own = all { state == Saving, state' = state, running' = running, last' = OnPropsOwn }
  action viewing_stays_on_props_other = all { state == Viewing, state' = state, running' = running, last' = OnPropsOther }
  action viewing_stays_on_props_own = all { state == Viewing, state' = state, running' = running, last' = OnPropsOwn }

  action step = any {
    editing_to_saving_on_save,
    editing_to_viewing_on_cancel,
    editing_to_viewing_on_props_other,
    idle_to_viewing_on_props_other,
    idle_to_viewing_on_props_own,
    saving_to_editing_on_saved_error,
    saving_to_saving_on_any,
    saving_to_viewing_on_props_other,
    saving_to_viewing_on_saved_ok,
    viewing_to_editing_on_edit,
    editing_stays_on_props_own,
    editing_stays_on_save,
    editing_stays_on_typed,
    saving_stays_on_props_own,
    viewing_stays_on_props_other,
    viewing_stays_on_props_own,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
