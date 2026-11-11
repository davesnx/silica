// Generated from the spec of the machine "save". Do not edit.
module save {
  type State = | Idle | Failed | Saved | Saving
  type Event = OnStart | OnAny | OnAnswerError | OnAnswerOk | OnReset | OnSave

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Failed => Set()
    | Saved => Set("saved/after 2000ms")
    | Saving => Set("saving/invoke /api/save")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Failed, Saved, Saving)

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

  action failed_to_saving_on_save = all { state == Failed, enter(Saving, OnSave) }
  action idle_to_saving_on_save = all { state == Idle, enter(Saving, OnSave) }
  action saved_to_idle_on_reset = all { state == Saved, enter(Idle, OnReset) }
  action saving_to_failed_on_answer_error = all { state == Saving, enter(Failed, OnAnswerError) }
  action saving_to_saved_on_answer_ok = all { state == Saving, enter(Saved, OnAnswerOk) }

  action step = any {
    failed_to_saving_on_save,
    idle_to_saving_on_save,
    saved_to_idle_on_reset,
    saving_to_failed_on_answer_error,
    saving_to_saved_on_answer_ok,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
