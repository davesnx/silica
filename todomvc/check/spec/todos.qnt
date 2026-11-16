// Generated from the spec of the machine "todos". Do not edit.
module todos {
  type State = | Empty | Editing | List
  type Event = OnStart | OnAny | OnAdd | OnCancelEdit | OnClearCompleted | OnDestroy | OnDraft | OnEdit | OnEditDraft | OnSaveEdit | OnToggle | OnToggleAll

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Empty => Set()
    | Editing => Set()
    | List => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Empty, Editing, List)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Empty,
    running' = owned(Empty),
    last' = OnStart,
  }

  action editing_to_empty_on_clear_completed = all { state == Editing, enter(Empty, OnClearCompleted) }
  action editing_to_empty_on_destroy = all { state == Editing, enter(Empty, OnDestroy) }
  action editing_to_empty_on_draft = all { state == Editing, enter(Empty, OnDraft) }
  action editing_to_empty_on_save_edit = all { state == Editing, enter(Empty, OnSaveEdit) }
  action editing_to_empty_on_toggle_all = all { state == Editing, enter(Empty, OnToggleAll) }
  action editing_to_list_on_add = all { state == Editing, enter(List, OnAdd) }
  action editing_to_list_on_cancel_edit = all { state == Editing, enter(List, OnCancelEdit) }
  action editing_to_list_on_clear_completed = all { state == Editing, enter(List, OnClearCompleted) }
  action editing_to_list_on_destroy = all { state == Editing, enter(List, OnDestroy) }
  action editing_to_list_on_draft = all { state == Editing, enter(List, OnDraft) }
  action editing_to_list_on_save_edit = all { state == Editing, enter(List, OnSaveEdit) }
  action editing_to_list_on_toggle = all { state == Editing, enter(List, OnToggle) }
  action editing_to_list_on_toggle_all = all { state == Editing, enter(List, OnToggleAll) }
  action empty_to_list_on_add = all { state == Empty, enter(List, OnAdd) }
  action list_to_editing_on_edit = all { state == List, enter(Editing, OnEdit) }
  action list_to_empty_on_clear_completed = all { state == List, enter(Empty, OnClearCompleted) }
  action list_to_empty_on_destroy = all { state == List, enter(Empty, OnDestroy) }
  action editing_stays_on_edit = all { state == Editing, state' = state, running' = running, last' = OnEdit }
  action editing_stays_on_edit_draft = all { state == Editing, state' = state, running' = running, last' = OnEditDraft }
  action empty_stays_on_draft = all { state == Empty, state' = state, running' = running, last' = OnDraft }
  action list_stays_on_add = all { state == List, state' = state, running' = running, last' = OnAdd }
  action list_stays_on_clear_completed = all { state == List, state' = state, running' = running, last' = OnClearCompleted }
  action list_stays_on_destroy = all { state == List, state' = state, running' = running, last' = OnDestroy }
  action list_stays_on_draft = all { state == List, state' = state, running' = running, last' = OnDraft }
  action list_stays_on_toggle = all { state == List, state' = state, running' = running, last' = OnToggle }
  action list_stays_on_toggle_all = all { state == List, state' = state, running' = running, last' = OnToggleAll }

  action step = any {
    editing_to_empty_on_clear_completed,
    editing_to_empty_on_destroy,
    editing_to_empty_on_draft,
    editing_to_empty_on_save_edit,
    editing_to_empty_on_toggle_all,
    editing_to_list_on_add,
    editing_to_list_on_cancel_edit,
    editing_to_list_on_clear_completed,
    editing_to_list_on_destroy,
    editing_to_list_on_draft,
    editing_to_list_on_save_edit,
    editing_to_list_on_toggle,
    editing_to_list_on_toggle_all,
    empty_to_list_on_add,
    list_to_editing_on_edit,
    list_to_empty_on_clear_completed,
    list_to_empty_on_destroy,
    editing_stays_on_edit,
    editing_stays_on_edit_draft,
    empty_stays_on_draft,
    list_stays_on_add,
    list_stays_on_clear_completed,
    list_stays_on_destroy,
    list_stays_on_draft,
    list_stays_on_toggle,
    list_stays_on_toggle_all,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
