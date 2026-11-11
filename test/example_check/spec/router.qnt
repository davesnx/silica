// Generated from the spec of the machine "router". Do not edit.
module router {
  type State = | S1 | S2 | About | Edit | ItemInt
  type Event = OnStart | OnAny | OnUrlChanged

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | S1 => Set()
    | S2 => Set()
    | About => Set()
    | Edit => Set()
    | ItemInt => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(S1, S2, About, Edit, ItemInt)

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

  action s1_to_s2_on_url_changed = all { state == S1, enter(S2, OnUrlChanged) }
  action s1_to_about_on_url_changed = all { state == S1, enter(About, OnUrlChanged) }
  action s1_to_edit_on_url_changed = all { state == S1, enter(Edit, OnUrlChanged) }
  action s1_to_item_int_on_url_changed = all { state == S1, enter(ItemInt, OnUrlChanged) }
  action s2_to_s1_on_url_changed = all { state == S2, enter(S1, OnUrlChanged) }
  action s2_to_about_on_url_changed = all { state == S2, enter(About, OnUrlChanged) }
  action s2_to_edit_on_url_changed = all { state == S2, enter(Edit, OnUrlChanged) }
  action s2_to_item_int_on_url_changed = all { state == S2, enter(ItemInt, OnUrlChanged) }
  action about_to_s1_on_url_changed = all { state == About, enter(S1, OnUrlChanged) }
  action about_to_s2_on_url_changed = all { state == About, enter(S2, OnUrlChanged) }
  action about_to_edit_on_url_changed = all { state == About, enter(Edit, OnUrlChanged) }
  action about_to_item_int_on_url_changed = all { state == About, enter(ItemInt, OnUrlChanged) }
  action edit_to_s1_on_url_changed = all { state == Edit, enter(S1, OnUrlChanged) }
  action edit_to_s2_on_url_changed = all { state == Edit, enter(S2, OnUrlChanged) }
  action edit_to_about_on_url_changed = all { state == Edit, enter(About, OnUrlChanged) }
  action edit_to_item_int_on_url_changed = all { state == Edit, enter(ItemInt, OnUrlChanged) }
  action item_int_to_s1_on_url_changed = all { state == ItemInt, enter(S1, OnUrlChanged) }
  action item_int_to_s2_on_url_changed = all { state == ItemInt, enter(S2, OnUrlChanged) }
  action item_int_to_about_on_url_changed = all { state == ItemInt, enter(About, OnUrlChanged) }
  action item_int_to_edit_on_url_changed = all { state == ItemInt, enter(Edit, OnUrlChanged) }
  action s1_stays_on_url_changed = all { state == S1, state' = state, running' = running, last' = OnUrlChanged }
  action s2_stays_on_url_changed = all { state == S2, state' = state, running' = running, last' = OnUrlChanged }
  action about_stays_on_url_changed = all { state == About, state' = state, running' = running, last' = OnUrlChanged }
  action edit_stays_on_url_changed = all { state == Edit, state' = state, running' = running, last' = OnUrlChanged }
  action item_int_stays_on_url_changed = all { state == ItemInt, state' = state, running' = running, last' = OnUrlChanged }

  action step = any {
    s1_to_s2_on_url_changed,
    s1_to_about_on_url_changed,
    s1_to_edit_on_url_changed,
    s1_to_item_int_on_url_changed,
    s2_to_s1_on_url_changed,
    s2_to_about_on_url_changed,
    s2_to_edit_on_url_changed,
    s2_to_item_int_on_url_changed,
    about_to_s1_on_url_changed,
    about_to_s2_on_url_changed,
    about_to_edit_on_url_changed,
    about_to_item_int_on_url_changed,
    edit_to_s1_on_url_changed,
    edit_to_s2_on_url_changed,
    edit_to_about_on_url_changed,
    edit_to_item_int_on_url_changed,
    item_int_to_s1_on_url_changed,
    item_int_to_s2_on_url_changed,
    item_int_to_about_on_url_changed,
    item_int_to_edit_on_url_changed,
    s1_stays_on_url_changed,
    s2_stays_on_url_changed,
    about_stays_on_url_changed,
    edit_stays_on_url_changed,
    item_int_stays_on_url_changed,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
