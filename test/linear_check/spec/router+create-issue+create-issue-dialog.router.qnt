// Generated from the spec of the machine "router". Do not edit.
module router {
  type State = | NewStatusAssigneePriorityLabelGroupOrder
  type Event = OnStart | OnAny | OnUrlChanged

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | NewStatusAssigneePriorityLabelGroupOrder => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(NewStatusAssigneePriorityLabelGroupOrder)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = NewStatusAssigneePriorityLabelGroupOrder,
    running' = owned(NewStatusAssigneePriorityLabelGroupOrder),
    last' = OnStart,
  }

  action new_status_assignee_priority_label_group_order_stays_on_url_changed = all { state == NewStatusAssigneePriorityLabelGroupOrder, state' = state, running' = running, last' = OnUrlChanged }

  action step = any {
    new_status_assignee_priority_label_group_order_stays_on_url_changed,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
