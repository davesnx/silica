// Generated from the spec of the machine "edit". Do not edit.
module edit {
  type State = | Clean | Asking | Dirty
  type Event = OnStart | OnAny | OnAttempt | OnDraft | OnLeave | OnSave | OnStay

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Clean => Set()
    | Asking => Set("asking/hold block")
    | Dirty => Set("dirty/hold block")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Clean, Asking, Dirty)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Clean,
    running' = owned(Clean),
    last' = OnStart,
  }

  action asking_to_clean_on_leave = all { state == Asking, enter(Clean, OnLeave) }
  action asking_to_dirty_on_stay = all { state == Asking, enter(Dirty, OnStay) }
  action clean_to_dirty_on_draft = all { state == Clean, enter(Dirty, OnDraft) }
  action dirty_to_asking_on_attempt = all { state == Dirty, enter(Asking, OnAttempt) }
  action dirty_to_clean_on_draft = all { state == Dirty, enter(Clean, OnDraft) }
  action dirty_to_clean_on_save = all { state == Dirty, enter(Clean, OnSave) }
  action asking_stays_on_attempt = all { state == Asking, state' = state, running' = running, last' = OnAttempt }
  action asking_stays_on_save = all { state == Asking, state' = state, running' = running, last' = OnSave }
  action clean_stays_on_save = all { state == Clean, state' = state, running' = running, last' = OnSave }
  action dirty_stays_on_draft = all { state == Dirty, state' = state, running' = running, last' = OnDraft }

  action step = any {
    asking_to_clean_on_leave,
    asking_to_dirty_on_stay,
    clean_to_dirty_on_draft,
    dirty_to_asking_on_attempt,
    dirty_to_clean_on_draft,
    dirty_to_clean_on_save,
    asking_stays_on_attempt,
    asking_stays_on_save,
    clean_stays_on_save,
    dirty_stays_on_draft,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
