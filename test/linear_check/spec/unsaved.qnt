// Generated from the spec of the machine "unsaved". Do not edit.
module unsaved {
  type State = | Held | Asking
  type Event = OnStart | OnAny | OnAttemptPush | OnAttemptTraverse | OnDiscard | OnKeep

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Held => Set("held/hold block")
    | Asking => Set("asking/hold block")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Held, Asking)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = Held,
    running' = owned(Held),
    last' = OnStart,
  }

  action asking_to_held_on_discard = all { state == Asking, enter(Held, OnDiscard) }
  action asking_to_held_on_keep = all { state == Asking, enter(Held, OnKeep) }
  action held_to_asking_on_attempt_push = all { state == Held, enter(Asking, OnAttemptPush) }
  action held_to_asking_on_attempt_traverse = all { state == Held, enter(Asking, OnAttemptTraverse) }
  action asking_stays_on_attempt_push = all { state == Asking, state' = state, running' = running, last' = OnAttemptPush }
  action asking_stays_on_attempt_traverse = all { state == Asking, state' = state, running' = running, last' = OnAttemptTraverse }

  action step = any {
    asking_to_held_on_discard,
    asking_to_held_on_keep,
    held_to_asking_on_attempt_push,
    held_to_asking_on_attempt_traverse,
    asking_stays_on_attempt_push,
    asking_stays_on_attempt_traverse,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
