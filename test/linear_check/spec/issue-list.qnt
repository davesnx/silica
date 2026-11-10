// Generated from the spec of the machine "issue-list". Do not edit.
module issue_list {
  type State = | Loading | Ready | Failed
  type Event = OnStart | OnAny | OnCommand | OnConflict | OnCreateKey | OnCursorNext | OnCursorPrevious | OnFilterChanged | OnFilterOpenFalse | OnFilterOpenTrue | OnGroupingPicked | OnLoadedError | OnLoadedOk | OnMenuOpenFalse | OnMenuOpenTrue | OnOrderingPicked | OnPropsDefault | OnPropsFiltered | OnReconnect | OnRetry | OnRowFocused | OnSaveFailed | OnToastClosed | OnToggleGroup | OnUpdated

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Loading => Set("loading/invoke /api/issues")
    | Ready => Set("ready/invoke live:issues", "ready/hold keys")
    | Failed => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Loading, Ready, Failed)

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
  action loading_to_loading_on_any = all { state == Loading, enter(Loading, OnAny) }
  action loading_to_ready_on_loaded_ok = all { state == Loading, enter(Ready, OnLoadedOk) }
  action ready_to_ready_on_reconnect = all { state == Ready, enter(Ready, OnReconnect) }
  action failed_stays_on_props_default = all { state == Failed, state' = state, running' = running, last' = OnPropsDefault }
  action failed_stays_on_props_filtered = all { state == Failed, state' = state, running' = running, last' = OnPropsFiltered }
  action loading_stays_on_props_default = all { state == Loading, state' = state, running' = running, last' = OnPropsDefault }
  action loading_stays_on_props_filtered = all { state == Loading, state' = state, running' = running, last' = OnPropsFiltered }
  action ready_stays_on_command = all { state == Ready, state' = state, running' = running, last' = OnCommand }
  action ready_stays_on_conflict = all { state == Ready, state' = state, running' = running, last' = OnConflict }
  action ready_stays_on_create_key = all { state == Ready, state' = state, running' = running, last' = OnCreateKey }
  action ready_stays_on_cursor_next = all { state == Ready, state' = state, running' = running, last' = OnCursorNext }
  action ready_stays_on_cursor_previous = all { state == Ready, state' = state, running' = running, last' = OnCursorPrevious }
  action ready_stays_on_filter_changed = all { state == Ready, state' = state, running' = running, last' = OnFilterChanged }
  action ready_stays_on_filter_open_false = all { state == Ready, state' = state, running' = running, last' = OnFilterOpenFalse }
  action ready_stays_on_filter_open_true = all { state == Ready, state' = state, running' = running, last' = OnFilterOpenTrue }
  action ready_stays_on_grouping_picked = all { state == Ready, state' = state, running' = running, last' = OnGroupingPicked }
  action ready_stays_on_menu_open_false = all { state == Ready, state' = state, running' = running, last' = OnMenuOpenFalse }
  action ready_stays_on_menu_open_true = all { state == Ready, state' = state, running' = running, last' = OnMenuOpenTrue }
  action ready_stays_on_ordering_picked = all { state == Ready, state' = state, running' = running, last' = OnOrderingPicked }
  action ready_stays_on_props_default = all { state == Ready, state' = state, running' = running, last' = OnPropsDefault }
  action ready_stays_on_props_filtered = all { state == Ready, state' = state, running' = running, last' = OnPropsFiltered }
  action ready_stays_on_row_focused = all { state == Ready, state' = state, running' = running, last' = OnRowFocused }
  action ready_stays_on_save_failed = all { state == Ready, state' = state, running' = running, last' = OnSaveFailed }
  action ready_stays_on_toast_closed = all { state == Ready, state' = state, running' = running, last' = OnToastClosed }
  action ready_stays_on_toggle_group = all { state == Ready, state' = state, running' = running, last' = OnToggleGroup }
  action ready_stays_on_updated = all { state == Ready, state' = state, running' = running, last' = OnUpdated }

  action step = any {
    failed_to_loading_on_retry,
    loading_to_failed_on_loaded_error,
    loading_to_loading_on_any,
    loading_to_ready_on_loaded_ok,
    ready_to_ready_on_reconnect,
    failed_stays_on_props_default,
    failed_stays_on_props_filtered,
    loading_stays_on_props_default,
    loading_stays_on_props_filtered,
    ready_stays_on_command,
    ready_stays_on_conflict,
    ready_stays_on_create_key,
    ready_stays_on_cursor_next,
    ready_stays_on_cursor_previous,
    ready_stays_on_filter_changed,
    ready_stays_on_filter_open_false,
    ready_stays_on_filter_open_true,
    ready_stays_on_grouping_picked,
    ready_stays_on_menu_open_false,
    ready_stays_on_menu_open_true,
    ready_stays_on_ordering_picked,
    ready_stays_on_props_default,
    ready_stays_on_props_filtered,
    ready_stays_on_row_focused,
    ready_stays_on_save_failed,
    ready_stays_on_toast_closed,
    ready_stays_on_toggle_group,
    ready_stays_on_updated,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
