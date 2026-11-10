// Generated from the spec of the machine "todo". Do not edit.
module todo {
  type State = | Todo
  type Event = OnStart | OnAny | OnAdd | OnDelete | OnDraft | OnToggle

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Todo => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Todo)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Todo,
    running' = owned(Todo),
    last' = OnStart,
  }

  action todo_stays_on_add = all { state == Todo, state' = state, running' = running, last' = OnAdd }
  action todo_stays_on_delete = all { state == Todo, state' = state, running' = running, last' = OnDelete }
  action todo_stays_on_draft = all { state == Todo, state' = state, running' = running, last' = OnDraft }
  action todo_stays_on_toggle = all { state == Todo, state' = state, running' = running, last' = OnToggle }

  action step = any {
    todo_stays_on_add,
    todo_stays_on_delete,
    todo_stays_on_draft,
    todo_stays_on_toggle,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
