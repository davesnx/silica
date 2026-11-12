// Generated from the spec of the machine "issue-list". Do not edit.
module issue_list {
  type State = | Loading | Failed | Ready
  type Event = OnStart | OnAny | OnCreateKey | OnCursorNext | OnCursorPrevious | OnFilterOpenTrue | OnLoadedError | OnLoadedOk | OnMenuOpenTrue | OnReconnect | OnReconnected | OnRetry | OnRowFocused | OnToggleGroup | OnUpdated

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Loading => Set("loading/invoke /api/issues")
    | Failed => Set()
    | Ready => Set("ready/after 0ms", "ready/invoke live:issues", "ready/hold keys")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Loading, Failed, Ready)

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

  action failed_to_loading_on_retry = all { state == Failed, enter(Loading, OnRetry) }
  action loading_to_failed_on_loaded_error = all { state == Loading, enter(Failed, OnLoadedError) }
  action loading_to_ready_on_loaded_ok = all { state == Loading, enter(Ready, OnLoadedOk) }
  action ready_stays_on_create_key = all { state == Ready, state' = state, running' = running, last' = OnCreateKey }
  action ready_stays_on_cursor_next = all { state == Ready, state' = state, running' = running, last' = OnCursorNext }
  action ready_stays_on_cursor_previous = all { state == Ready, state' = state, running' = running, last' = OnCursorPrevious }
  action ready_stays_on_filter_open_true = all { state == Ready, state' = state, running' = running, last' = OnFilterOpenTrue }
  action ready_stays_on_menu_open_true = all { state == Ready, state' = state, running' = running, last' = OnMenuOpenTrue }
  action ready_stays_on_reconnect = all { state == Ready, state' = state, running' = running, last' = OnReconnect }
  action ready_stays_on_reconnected = all { state == Ready, state' = state, running' = running, last' = OnReconnected }
  action ready_stays_on_row_focused = all { state == Ready, state' = state, running' = running, last' = OnRowFocused }
  action ready_stays_on_toggle_group = all { state == Ready, state' = state, running' = running, last' = OnToggleGroup }
  action ready_stays_on_updated = all { state == Ready, state' = state, running' = running, last' = OnUpdated }

  action step = any {
    failed_to_loading_on_retry,
    loading_to_failed_on_loaded_error,
    loading_to_ready_on_loaded_ok,
    ready_stays_on_create_key,
    ready_stays_on_cursor_next,
    ready_stays_on_cursor_previous,
    ready_stays_on_filter_open_true,
    ready_stays_on_menu_open_true,
    ready_stays_on_reconnect,
    ready_stays_on_reconnected,
    ready_stays_on_row_focused,
    ready_stays_on_toggle_group,
    ready_stays_on_updated,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
