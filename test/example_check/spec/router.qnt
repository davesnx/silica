// Generated from the spec of the machine "router". Do not edit.
module router {
  type State = | S1 | ItemInt | About | Edit | Locked | S6
  type Event = OnStart | OnAny | OnUrlChanged

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | S1 => Set()
    | ItemInt => Set()
    | About => Set()
    | Edit => Set()
    | Locked => Set()
    | S6 => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(S1, ItemInt, About, Edit, Locked, S6)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = S1,
    running' = owned(S1),
    last' = OnStart,
  }

  action s1_to_s1_on_any = all { state == S1, enter(S1, OnAny) }
  action s1_to_s6_on_url_changed = all { state == S1, enter(S6, OnUrlChanged) }
  action s1_to_about_on_url_changed = all { state == S1, enter(About, OnUrlChanged) }
  action s1_to_edit_on_url_changed = all { state == S1, enter(Edit, OnUrlChanged) }
  action s1_to_item_int_on_url_changed = all { state == S1, enter(ItemInt, OnUrlChanged) }
  action s1_to_locked_on_any = all { state == S1, enter(Locked, OnAny) }
  action s6_to_s1_on_url_changed = all { state == S6, enter(S1, OnUrlChanged) }
  action s6_to_s6_on_any = all { state == S6, enter(S6, OnAny) }
  action s6_to_about_on_url_changed = all { state == S6, enter(About, OnUrlChanged) }
  action s6_to_edit_on_url_changed = all { state == S6, enter(Edit, OnUrlChanged) }
  action s6_to_item_int_on_url_changed = all { state == S6, enter(ItemInt, OnUrlChanged) }
  action s6_to_locked_on_any = all { state == S6, enter(Locked, OnAny) }
  action about_to_s1_on_url_changed = all { state == About, enter(S1, OnUrlChanged) }
  action about_to_s6_on_url_changed = all { state == About, enter(S6, OnUrlChanged) }
  action about_to_about_on_any = all { state == About, enter(About, OnAny) }
  action about_to_edit_on_url_changed = all { state == About, enter(Edit, OnUrlChanged) }
  action about_to_item_int_on_url_changed = all { state == About, enter(ItemInt, OnUrlChanged) }
  action about_to_locked_on_any = all { state == About, enter(Locked, OnAny) }
  action edit_to_s1_on_url_changed = all { state == Edit, enter(S1, OnUrlChanged) }
  action edit_to_s6_on_url_changed = all { state == Edit, enter(S6, OnUrlChanged) }
  action edit_to_about_on_url_changed = all { state == Edit, enter(About, OnUrlChanged) }
  action edit_to_edit_on_any = all { state == Edit, enter(Edit, OnAny) }
  action edit_to_item_int_on_url_changed = all { state == Edit, enter(ItemInt, OnUrlChanged) }
  action edit_to_locked_on_any = all { state == Edit, enter(Locked, OnAny) }
  action item_int_to_s1_on_url_changed = all { state == ItemInt, enter(S1, OnUrlChanged) }
  action item_int_to_s6_on_url_changed = all { state == ItemInt, enter(S6, OnUrlChanged) }
  action item_int_to_about_on_url_changed = all { state == ItemInt, enter(About, OnUrlChanged) }
  action item_int_to_edit_on_url_changed = all { state == ItemInt, enter(Edit, OnUrlChanged) }
  action item_int_to_item_int_on_any = all { state == ItemInt, enter(ItemInt, OnAny) }
  action item_int_to_locked_on_any = all { state == ItemInt, enter(Locked, OnAny) }
  action locked_to_s1_on_any = all { state == Locked, enter(S1, OnAny) }
  action locked_to_s6_on_any = all { state == Locked, enter(S6, OnAny) }
  action locked_to_about_on_any = all { state == Locked, enter(About, OnAny) }
  action locked_to_edit_on_any = all { state == Locked, enter(Edit, OnAny) }
  action locked_to_item_int_on_any = all { state == Locked, enter(ItemInt, OnAny) }
  action locked_to_locked_on_any = all { state == Locked, enter(Locked, OnAny) }
  action s1_stays_on_url_changed = all { state == S1, state' = state, running' = running, last' = OnUrlChanged }
  action s6_stays_on_url_changed = all { state == S6, state' = state, running' = running, last' = OnUrlChanged }
  action about_stays_on_url_changed = all { state == About, state' = state, running' = running, last' = OnUrlChanged }
  action edit_stays_on_url_changed = all { state == Edit, state' = state, running' = running, last' = OnUrlChanged }
  action item_int_stays_on_url_changed = all { state == ItemInt, state' = state, running' = running, last' = OnUrlChanged }

  action step = any {
    s1_to_s1_on_any,
    s1_to_s6_on_url_changed,
    s1_to_about_on_url_changed,
    s1_to_edit_on_url_changed,
    s1_to_item_int_on_url_changed,
    s1_to_locked_on_any,
    s6_to_s1_on_url_changed,
    s6_to_s6_on_any,
    s6_to_about_on_url_changed,
    s6_to_edit_on_url_changed,
    s6_to_item_int_on_url_changed,
    s6_to_locked_on_any,
    about_to_s1_on_url_changed,
    about_to_s6_on_url_changed,
    about_to_about_on_any,
    about_to_edit_on_url_changed,
    about_to_item_int_on_url_changed,
    about_to_locked_on_any,
    edit_to_s1_on_url_changed,
    edit_to_s6_on_url_changed,
    edit_to_about_on_url_changed,
    edit_to_edit_on_any,
    edit_to_item_int_on_url_changed,
    edit_to_locked_on_any,
    item_int_to_s1_on_url_changed,
    item_int_to_s6_on_url_changed,
    item_int_to_about_on_url_changed,
    item_int_to_edit_on_url_changed,
    item_int_to_item_int_on_any,
    item_int_to_locked_on_any,
    locked_to_s1_on_any,
    locked_to_s6_on_any,
    locked_to_about_on_any,
    locked_to_edit_on_any,
    locked_to_item_int_on_any,
    locked_to_locked_on_any,
    s1_stays_on_url_changed,
    s6_stays_on_url_changed,
    about_stays_on_url_changed,
    edit_stays_on_url_changed,
    item_int_stays_on_url_changed,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
