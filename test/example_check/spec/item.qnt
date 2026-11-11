// Generated from the spec of the machine "item". Do not edit.
module item {
  type State = | Loading | Failed | Loaded | Missing
  type Event = OnStart | OnAny | OnProps | OnSettledError | OnSettledOk | OnSettledRejected

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Loading => Set("loading/invoke /api/item")
    | Failed => Set()
    | Loaded => Set()
    | Missing => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Loading, Failed, Loaded, Missing)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Loading,
    running' = owned(Loading),
    last' = OnStart,
  }

  action failed_to_loading_on_props = all { state == Failed, enter(Loading, OnProps) }
  action loaded_to_loading_on_props = all { state == Loaded, enter(Loading, OnProps) }
  action loading_to_failed_on_settled_error = all { state == Loading, enter(Failed, OnSettledError) }
  action loading_to_loaded_on_settled_ok = all { state == Loading, enter(Loaded, OnSettledOk) }
  action loading_to_missing_on_settled_rejected = all { state == Loading, enter(Missing, OnSettledRejected) }
  action missing_to_loading_on_props = all { state == Missing, enter(Loading, OnProps) }
  action loading_stays_on_props = all { state == Loading, state' = state, running' = running, last' = OnProps }

  action step = any {
    failed_to_loading_on_props,
    loaded_to_loading_on_props,
    loading_to_failed_on_settled_error,
    loading_to_loaded_on_settled_ok,
    loading_to_missing_on_settled_rejected,
    missing_to_loading_on_props,
    loading_stays_on_props,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
