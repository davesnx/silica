// Generated from the spec of the machine "issue-detail". Do not edit.
module issue_detail {
  type State = | Loading | Failed | NotFound | Ready
  type Event = OnStart | OnAny | OnCancelEdit | OnCommand | OnCreateKey | OnDraftChanged | OnEdit | OnKeepEditing | OnLoadedError | OnLoadedOk | OnMenuOpenFalse | OnMenuOpenTrue | OnPick | OnPickerClosed | OnPickerOpen | OnProps | OnReconnect | OnReconnected | OnRetry | OnSave | OnSavedConflict | OnSavedError | OnSavedOk | OnToastClosed | OnUpdated

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Loading => Set("loading/invoke /api/issues")
    | Failed => Set()
    | NotFound => Set()
    | Ready => Set("ready/invoke live:issues", "ready/hold keys")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Loading, Failed, NotFound, Ready)

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
  action failed_to_loading_on_retry = all { state == Failed, enter(Loading, OnRetry) }
  action loading_to_failed_on_loaded_error = all { state == Loading, enter(Failed, OnLoadedError) }
  action loading_to_loading_on_props = all { state == Loading, enter(Loading, OnProps) }
  action loading_to_not_found_on_loaded_ok = all { state == Loading, enter(NotFound, OnLoadedOk) }
  action loading_to_ready_on_loaded_ok = all { state == Loading, enter(Ready, OnLoadedOk) }
  action not_found_to_loading_on_props = all { state == NotFound, enter(Loading, OnProps) }
  action ready_to_loading_on_props = all { state == Ready, enter(Loading, OnProps) }
  action ready_stays_on_cancel_edit = all { state == Ready, state' = state, running' = running, last' = OnCancelEdit }
  action ready_stays_on_command = all { state == Ready, state' = state, running' = running, last' = OnCommand }
  action ready_stays_on_create_key = all { state == Ready, state' = state, running' = running, last' = OnCreateKey }
  action ready_stays_on_draft_changed = all { state == Ready, state' = state, running' = running, last' = OnDraftChanged }
  action ready_stays_on_edit = all { state == Ready, state' = state, running' = running, last' = OnEdit }
  action ready_stays_on_keep_editing = all { state == Ready, state' = state, running' = running, last' = OnKeepEditing }
  action ready_stays_on_menu_open_false = all { state == Ready, state' = state, running' = running, last' = OnMenuOpenFalse }
  action ready_stays_on_menu_open_true = all { state == Ready, state' = state, running' = running, last' = OnMenuOpenTrue }
  action ready_stays_on_pick = all { state == Ready, state' = state, running' = running, last' = OnPick }
  action ready_stays_on_picker_closed = all { state == Ready, state' = state, running' = running, last' = OnPickerClosed }
  action ready_stays_on_picker_open = all { state == Ready, state' = state, running' = running, last' = OnPickerOpen }
  action ready_stays_on_reconnect = all { state == Ready, state' = state, running' = running, last' = OnReconnect }
  action ready_stays_on_reconnected = all { state == Ready, state' = state, running' = running, last' = OnReconnected }
  action ready_stays_on_save = all { state == Ready, state' = state, running' = running, last' = OnSave }
  action ready_stays_on_saved_conflict = all { state == Ready, state' = state, running' = running, last' = OnSavedConflict }
  action ready_stays_on_saved_error = all { state == Ready, state' = state, running' = running, last' = OnSavedError }
  action ready_stays_on_saved_ok = all { state == Ready, state' = state, running' = running, last' = OnSavedOk }
  action ready_stays_on_toast_closed = all { state == Ready, state' = state, running' = running, last' = OnToastClosed }
  action ready_stays_on_updated = all { state == Ready, state' = state, running' = running, last' = OnUpdated }

  action step = any {
    failed_to_loading_on_props,
    failed_to_loading_on_retry,
    loading_to_failed_on_loaded_error,
    loading_to_loading_on_props,
    loading_to_not_found_on_loaded_ok,
    loading_to_ready_on_loaded_ok,
    not_found_to_loading_on_props,
    ready_to_loading_on_props,
    ready_stays_on_cancel_edit,
    ready_stays_on_command,
    ready_stays_on_create_key,
    ready_stays_on_draft_changed,
    ready_stays_on_edit,
    ready_stays_on_keep_editing,
    ready_stays_on_menu_open_false,
    ready_stays_on_menu_open_true,
    ready_stays_on_pick,
    ready_stays_on_picker_closed,
    ready_stays_on_picker_open,
    ready_stays_on_reconnect,
    ready_stays_on_reconnected,
    ready_stays_on_save,
    ready_stays_on_saved_conflict,
    ready_stays_on_saved_error,
    ready_stays_on_saved_ok,
    ready_stays_on_toast_closed,
    ready_stays_on_updated,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
