// Generated from the spec of the machine "me". Do not edit.
module me {
  type State = | Loading | Failed | Ready
  type Event = OnStart | OnAny | OnLoadedError | OnLoadedOk

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Loading => Set("loading/invoke /api/me")
    | Failed => Set()
    | Ready => Set()
  }

  pure val finals: Set[State] = Set(Failed, Ready)
  pure val movable: Set[State] = Set(Loading)

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

  action loading_to_failed_on_loaded_error = all { state == Loading, enter(Failed, OnLoadedError) }
  action loading_to_ready_on_loaded_ok = all { state == Loading, enter(Ready, OnLoadedOk) }

  action step = any {
    loading_to_failed_on_loaded_error,
    loading_to_ready_on_loaded_ok,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
