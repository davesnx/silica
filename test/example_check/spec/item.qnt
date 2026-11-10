// Generated from the spec of the machine "item". Do not edit.
module item {
  type State = | Idle | Loading | Loaded | Failed
  type Event = OnStart | OnAny | OnProps | OnSettledError | OnSettledOk

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Loading => Set("loading/invoke /api/item")
    | Loaded => Set()
    | Failed => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Loading, Loaded, Failed)

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

  action failed_to_failed_on_any = all { state == Failed, enter(Failed, OnAny) }
  action failed_to_loading_on_props = all { state == Failed, enter(Loading, OnProps) }
  action idle_to_idle_on_any = all { state == Idle, enter(Idle, OnAny) }
  action idle_to_loading_on_props = all { state == Idle, enter(Loading, OnProps) }
  action loaded_to_loaded_on_any = all { state == Loaded, enter(Loaded, OnAny) }
  action loaded_to_loading_on_props = all { state == Loaded, enter(Loading, OnProps) }
  action loading_to_failed_on_settled_error = all { state == Loading, enter(Failed, OnSettledError) }
  action loading_to_loaded_on_settled_ok = all { state == Loading, enter(Loaded, OnSettledOk) }
  action loading_to_loading_on_props = all { state == Loading, enter(Loading, OnProps) }

  action step = any {
    failed_to_failed_on_any,
    failed_to_loading_on_props,
    idle_to_idle_on_any,
    idle_to_loading_on_props,
    loaded_to_loaded_on_any,
    loaded_to_loading_on_props,
    loading_to_failed_on_settled_error,
    loading_to_loaded_on_settled_ok,
    loading_to_loading_on_props,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
