// Generated from the spec of the machine "vote". Do not edit.
module vote {
  type State = | Idle | Loading | Ready | Failed
  type Event = OnStart | OnAny | OnLoad | OnRetry | OnSettledError | OnSettledOk

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Loading => Set("loading/invoke /api/vote")
    | Ready => Set()
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

  action failed_to_idle_on_any = all { state == Failed, enter(Idle, OnAny) }
  action failed_to_loading_on_load = all { state == Failed, enter(Loading, OnLoad) }
  action failed_to_loading_on_retry = all { state == Failed, enter(Loading, OnRetry) }
  action idle_to_idle_on_any = all { state == Idle, enter(Idle, OnAny) }
  action idle_to_loading_on_load = all { state == Idle, enter(Loading, OnLoad) }
  action loading_to_failed_on_settled_error = all { state == Loading, enter(Failed, OnSettledError) }
  action loading_to_idle_on_any = all { state == Loading, enter(Idle, OnAny) }
  action loading_to_loading_on_load = all { state == Loading, enter(Loading, OnLoad) }
  action loading_to_ready_on_settled_ok = all { state == Loading, enter(Ready, OnSettledOk) }
  action ready_to_idle_on_any = all { state == Ready, enter(Idle, OnAny) }
  action ready_to_loading_on_load = all { state == Ready, enter(Loading, OnLoad) }

  action step = any {
    failed_to_idle_on_any,
    failed_to_loading_on_load,
    failed_to_loading_on_retry,
    idle_to_idle_on_any,
    idle_to_loading_on_load,
    loading_to_failed_on_settled_error,
    loading_to_idle_on_any,
    loading_to_loading_on_load,
    loading_to_ready_on_settled_ok,
    ready_to_idle_on_any,
    ready_to_loading_on_load,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
