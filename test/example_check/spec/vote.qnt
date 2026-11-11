// Generated from the spec of the machine "vote". Do not edit.
module vote {
  type State = | Idle | Failed | Loading | Ready | Rejected
  type Event = OnStart | OnAny | OnLoad | OnRetry | OnSettledError | OnSettledOk | OnSettledRejected

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Failed => Set()
    | Loading => Set("loading/invoke /api/vote")
    | Ready => Set()
    | Rejected => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Failed, Loading, Ready, Rejected)

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

  action failed_to_loading_on_load = all { state == Failed, enter(Loading, OnLoad) }
  action failed_to_loading_on_retry = all { state == Failed, enter(Loading, OnRetry) }
  action idle_to_loading_on_load = all { state == Idle, enter(Loading, OnLoad) }
  action loading_to_failed_on_settled_error = all { state == Loading, enter(Failed, OnSettledError) }
  action loading_to_loading_on_load = all { state == Loading, enter(Loading, OnLoad) }
  action loading_to_ready_on_settled_ok = all { state == Loading, enter(Ready, OnSettledOk) }
  action loading_to_rejected_on_settled_rejected = all { state == Loading, enter(Rejected, OnSettledRejected) }
  action ready_to_loading_on_load = all { state == Ready, enter(Loading, OnLoad) }
  action rejected_to_loading_on_load = all { state == Rejected, enter(Loading, OnLoad) }
  action rejected_to_loading_on_retry = all { state == Rejected, enter(Loading, OnRetry) }

  action step = any {
    failed_to_loading_on_load,
    failed_to_loading_on_retry,
    idle_to_loading_on_load,
    loading_to_failed_on_settled_error,
    loading_to_loading_on_load,
    loading_to_ready_on_settled_ok,
    loading_to_rejected_on_settled_rejected,
    ready_to_loading_on_load,
    rejected_to_loading_on_load,
    rejected_to_loading_on_retry,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
