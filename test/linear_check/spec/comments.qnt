// Generated from the spec of the machine "comments". Do not edit.
module comments {
  type State = | Idle | Loading | Ready | Failed
  type Event = OnStart | OnAny | OnLoadedError | OnLoadedOk | OnProps | OnRetry | OnUpdated

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Loading => Set("loading/invoke /api/comments")
    | Ready => Set("ready/invoke live:comments")
    | Failed => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Loading, Ready, Failed)

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

  action failed_to_loading_on_props = all { state == Failed, enter(Loading, OnProps) }
  action failed_to_loading_on_retry = all { state == Failed, enter(Loading, OnRetry) }
  action idle_to_loading_on_props = all { state == Idle, enter(Loading, OnProps) }
  action loading_to_failed_on_loaded_error = all { state == Loading, enter(Failed, OnLoadedError) }
  action loading_to_loading_on_props = all { state == Loading, enter(Loading, OnProps) }
  action loading_to_ready_on_loaded_ok = all { state == Loading, enter(Ready, OnLoadedOk) }
  action ready_to_loading_on_props = all { state == Ready, enter(Loading, OnProps) }
  action ready_to_ready_on_any = all { state == Ready, enter(Ready, OnAny) }
  action ready_stays_on_updated = all { state == Ready, state' = state, running' = running, last' = OnUpdated }

  action step = any {
    failed_to_loading_on_props,
    failed_to_loading_on_retry,
    idle_to_loading_on_props,
    loading_to_failed_on_loaded_error,
    loading_to_loading_on_props,
    loading_to_ready_on_loaded_ok,
    ready_to_loading_on_props,
    ready_to_ready_on_any,
    ready_stays_on_updated,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
