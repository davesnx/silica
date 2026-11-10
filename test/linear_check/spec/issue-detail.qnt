// Generated from the spec of the machine "issue-detail". Do not edit.
module issue_detail {
  type State = | Idle | Loading | Ready | NotFound | Failed
  type Event = OnStart | OnAny | OnCancelEdit | OnCommand | OnConflict | OnCreateKey | OnDraft | OnEdit | OnKeepEditing | OnLoadedError | OnLoadedOk | OnMenuOpenFalse | OnMenuOpenTrue | OnPick | OnPickerClosed | OnPickerOpen | OnProps | OnReconnect | OnRetry | OnSave | OnSaveFailed | OnSaved | OnToastClosed | OnUpdated

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Loading => Set("loading/invoke /api/issues")
    | Ready => Set("ready/invoke live:issues", "ready/hold keys")
    | NotFound => Set()
    | Failed => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Loading, Ready, NotFound, Failed)

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
  action loading_to_not_found_on_loaded_ok = all { state == Loading, enter(NotFound, OnLoadedOk) }
  action loading_to_ready_on_loaded_ok = all { state == Loading, enter(Ready, OnLoadedOk) }
  action not_found_to_loading_on_props = all { state == NotFound, enter(Loading, OnProps) }
  action ready_to_loading_on_props = all { state == Ready, enter(Loading, OnProps) }
  action ready_to_ready_on_reconnect = all { state == Ready, enter(Ready, OnReconnect) }
  action ready_stays_on_cancel_edit = all { state == Ready, state' = state, running' = running, last' = OnCancelEdit }
  action ready_stays_on_command = all { state == Ready, state' = state, running' = running, last' = OnCommand }
  action ready_stays_on_conflict = all { state == Ready, state' = state, running' = running, last' = OnConflict }
  action ready_stays_on_create_key = all { state == Ready, state' = state, running' = running, last' = OnCreateKey }
  action ready_stays_on_draft = all { state == Ready, state' = state, running' = running, last' = OnDraft }
  action ready_stays_on_edit = all { state == Ready, state' = state, running' = running, last' = OnEdit }
  action ready_stays_on_keep_editing = all { state == Ready, state' = state, running' = running, last' = OnKeepEditing }
  action ready_stays_on_menu_open_false = all { state == Ready, state' = state, running' = running, last' = OnMenuOpenFalse }
  action ready_stays_on_menu_open_true = all { state == Ready, state' = state, running' = running, last' = OnMenuOpenTrue }
  action ready_stays_on_pick = all { state == Ready, state' = state, running' = running, last' = OnPick }
  action ready_stays_on_picker_closed = all { state == Ready, state' = state, running' = running, last' = OnPickerClosed }
  action ready_stays_on_picker_open = all { state == Ready, state' = state, running' = running, last' = OnPickerOpen }
  action ready_stays_on_save = all { state == Ready, state' = state, running' = running, last' = OnSave }
  action ready_stays_on_save_failed = all { state == Ready, state' = state, running' = running, last' = OnSaveFailed }
  action ready_stays_on_saved = all { state == Ready, state' = state, running' = running, last' = OnSaved }
  action ready_stays_on_toast_closed = all { state == Ready, state' = state, running' = running, last' = OnToastClosed }
  action ready_stays_on_updated = all { state == Ready, state' = state, running' = running, last' = OnUpdated }

  action step = any {
    failed_to_loading_on_props,
    failed_to_loading_on_retry,
    idle_to_loading_on_props,
    loading_to_failed_on_loaded_error,
    loading_to_loading_on_props,
    loading_to_not_found_on_loaded_ok,
    loading_to_ready_on_loaded_ok,
    not_found_to_loading_on_props,
    ready_to_loading_on_props,
    ready_to_ready_on_reconnect,
    ready_stays_on_cancel_edit,
    ready_stays_on_command,
    ready_stays_on_conflict,
    ready_stays_on_create_key,
    ready_stays_on_draft,
    ready_stays_on_edit,
    ready_stays_on_keep_editing,
    ready_stays_on_menu_open_false,
    ready_stays_on_menu_open_true,
    ready_stays_on_pick,
    ready_stays_on_picker_closed,
    ready_stays_on_picker_open,
    ready_stays_on_save,
    ready_stays_on_save_failed,
    ready_stays_on_saved,
    ready_stays_on_toast_closed,
    ready_stays_on_updated,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
