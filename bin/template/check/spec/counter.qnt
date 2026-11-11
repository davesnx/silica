// Generated from the spec of the machine "counter". Do not edit.
module counter {
  type State = | Counting | SaveFailed | Saved | Saving
  type Event = OnStart | OnAny | OnAnsweredError | OnAnsweredOk | OnAnsweredRejected | OnDecrement | OnIncrement | OnSave

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Counting => Set()
    | SaveFailed => Set()
    | Saved => Set()
    | Saving => Set("saving/invoke /api/save")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Counting, SaveFailed, Saved, Saving)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Counting,
    running' = owned(Counting),
    last' = OnStart,
  }

  action counting_to_saving_on_save = all { state == Counting, enter(Saving, OnSave) }
  action save_failed_to_counting_on_decrement = all { state == SaveFailed, enter(Counting, OnDecrement) }
  action save_failed_to_counting_on_increment = all { state == SaveFailed, enter(Counting, OnIncrement) }
  action save_failed_to_saving_on_save = all { state == SaveFailed, enter(Saving, OnSave) }
  action saved_to_counting_on_decrement = all { state == Saved, enter(Counting, OnDecrement) }
  action saved_to_counting_on_increment = all { state == Saved, enter(Counting, OnIncrement) }
  action saving_to_save_failed_on_answered_error = all { state == Saving, enter(SaveFailed, OnAnsweredError) }
  action saving_to_save_failed_on_answered_rejected = all { state == Saving, enter(SaveFailed, OnAnsweredRejected) }
  action saving_to_saved_on_answered_ok = all { state == Saving, enter(Saved, OnAnsweredOk) }
  action counting_stays_on_decrement = all { state == Counting, state' = state, running' = running, last' = OnDecrement }
  action counting_stays_on_increment = all { state == Counting, state' = state, running' = running, last' = OnIncrement }

  action step = any {
    counting_to_saving_on_save,
    save_failed_to_counting_on_decrement,
    save_failed_to_counting_on_increment,
    save_failed_to_saving_on_save,
    saved_to_counting_on_decrement,
    saved_to_counting_on_increment,
    saving_to_save_failed_on_answered_error,
    saving_to_save_failed_on_answered_rejected,
    saving_to_saved_on_answered_ok,
    counting_stays_on_decrement,
    counting_stays_on_increment,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
