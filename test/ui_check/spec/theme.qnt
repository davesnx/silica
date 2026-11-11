// Generated from the spec of the machine "theme". Do not edit.
module theme {
  type State = | System | Fixed
  type Event = OnStart | OnAny | OnChooseHighContrast | OnChooseLight | OnChooseSystem | OnProps | OnSystemDarkFalse | OnSystemDarkTrue

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | System => Set("system/hold media")
    | Fixed => Set()
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(System, Fixed)

  action enter(s: State, e: Event): bool = all {
    state' = s,
    running' = owned(s),
    last' = e,
  }

  action init = all {
    state' = System,
    running' = owned(System),
    last' = OnStart,
  }

  action fixed_to_system_on_choose_system = all { state == Fixed, enter(System, OnChooseSystem) }
  action fixed_to_system_on_props = all { state == Fixed, enter(System, OnProps) }
  action system_to_fixed_on_choose_high_contrast = all { state == System, enter(Fixed, OnChooseHighContrast) }
  action system_to_fixed_on_choose_light = all { state == System, enter(Fixed, OnChooseLight) }
  action system_to_fixed_on_props = all { state == System, enter(Fixed, OnProps) }
  action fixed_stays_on_choose_high_contrast = all { state == Fixed, state' = state, running' = running, last' = OnChooseHighContrast }
  action fixed_stays_on_choose_light = all { state == Fixed, state' = state, running' = running, last' = OnChooseLight }
  action fixed_stays_on_props = all { state == Fixed, state' = state, running' = running, last' = OnProps }
  action system_stays_on_choose_system = all { state == System, state' = state, running' = running, last' = OnChooseSystem }
  action system_stays_on_props = all { state == System, state' = state, running' = running, last' = OnProps }
  action system_stays_on_system_dark_false = all { state == System, state' = state, running' = running, last' = OnSystemDarkFalse }
  action system_stays_on_system_dark_true = all { state == System, state' = state, running' = running, last' = OnSystemDarkTrue }

  action step = any {
    fixed_to_system_on_choose_system,
    fixed_to_system_on_props,
    system_to_fixed_on_choose_high_contrast,
    system_to_fixed_on_choose_light,
    system_to_fixed_on_props,
    fixed_stays_on_choose_high_contrast,
    fixed_stays_on_choose_light,
    fixed_stays_on_props,
    system_stays_on_choose_system,
    system_stays_on_props,
    system_stays_on_system_dark_false,
    system_stays_on_system_dark_true,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
