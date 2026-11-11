// Generated from the spec of the machine "comment". Do not edit.
module comment {
  type State = | Viewing | Edited | Editing | Saving
  type Event = OnStart | OnAny | OnCancel | OnEdit | OnSave | OnSavedError | OnSavedOk | OnTyped

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Viewing => Set()
    | Edited => Set()
    | Editing => Set()
    | Saving => Set("saving/invoke /api/comments/edit")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Viewing, Edited, Editing, Saving)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Viewing,
    running' = owned(Viewing),
    last' = OnStart,
  }

  action edited_to_editing_on_edit = all { state == Edited, enter(Editing, OnEdit) }
  action editing_to_saving_on_save = all { state == Editing, enter(Saving, OnSave) }
  action editing_to_viewing_on_cancel = all { state == Editing, enter(Viewing, OnCancel) }
  action saving_to_edited_on_saved_ok = all { state == Saving, enter(Edited, OnSavedOk) }
  action saving_to_editing_on_saved_error = all { state == Saving, enter(Editing, OnSavedError) }
  action viewing_to_editing_on_edit = all { state == Viewing, enter(Editing, OnEdit) }
  action editing_stays_on_typed = all { state == Editing, state' = state, running' = running, last' = OnTyped }

  action step = any {
    edited_to_editing_on_edit,
    editing_to_saving_on_save,
    editing_to_viewing_on_cancel,
    saving_to_edited_on_saved_ok,
    saving_to_editing_on_saved_error,
    viewing_to_editing_on_edit,
    editing_stays_on_typed,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
