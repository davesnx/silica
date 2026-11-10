// Generated from the spec of the machine "theme". Do not edit.
module theme {
  type State = | Idle | Fixed | System
  type Event = OnStart | OnAny | OnChooseHighContrast | OnChooseLight | OnChooseSystem | OnPropsOcean | OnPropsSystem | OnSystemDarkFalse | OnSystemDarkTrue

  var state: State
  var running: Set[str]
  var last: Event

  pure def owned(s: State): Set[str] = match s {
    | Idle => Set()
    | Fixed => Set()
    | System => Set("system/hold media")
  }

  pure val finals: Set[State] = Set()
  pure val movable: Set[State] = Set(Idle, Fixed, System)

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

  action fixed_to_system_on_choose_system = all { state == Fixed, enter(System, OnChooseSystem) }
  action fixed_to_system_on_props_system = all { state == Fixed, enter(System, OnPropsSystem) }
  action idle_to_fixed_on_props_ocean = all { state == Idle, enter(Fixed, OnPropsOcean) }
  action idle_to_system_on_props_system = all { state == Idle, enter(System, OnPropsSystem) }
  action system_to_fixed_on_choose_high_contrast = all { state == System, enter(Fixed, OnChooseHighContrast) }
  action system_to_fixed_on_choose_light = all { state == System, enter(Fixed, OnChooseLight) }
  action system_to_fixed_on_props_ocean = all { state == System, enter(Fixed, OnPropsOcean) }
  action system_to_system_on_any = all { state == System, enter(System, OnAny) }
  action fixed_stays_on_choose_high_contrast = all { state == Fixed, state' = state, running' = running, last' = OnChooseHighContrast }
  action fixed_stays_on_choose_light = all { state == Fixed, state' = state, running' = running, last' = OnChooseLight }
  action fixed_stays_on_props_ocean = all { state == Fixed, state' = state, running' = running, last' = OnPropsOcean }
  action fixed_stays_on_props_system = all { state == Fixed, state' = state, running' = running, last' = OnPropsSystem }
  action system_stays_on_choose_system = all { state == System, state' = state, running' = running, last' = OnChooseSystem }
  action system_stays_on_props_ocean = all { state == System, state' = state, running' = running, last' = OnPropsOcean }
  action system_stays_on_props_system = all { state == System, state' = state, running' = running, last' = OnPropsSystem }
  action system_stays_on_system_dark_false = all { state == System, state' = state, running' = running, last' = OnSystemDarkFalse }
  action system_stays_on_system_dark_true = all { state == System, state' = state, running' = running, last' = OnSystemDarkTrue }

  action step = any {
    fixed_to_system_on_choose_system,
    fixed_to_system_on_props_system,
    idle_to_fixed_on_props_ocean,
    idle_to_system_on_props_system,
    system_to_fixed_on_choose_high_contrast,
    system_to_fixed_on_choose_light,
    system_to_fixed_on_props_ocean,
    system_to_system_on_any,
    fixed_stays_on_choose_high_contrast,
    fixed_stays_on_choose_light,
    fixed_stays_on_props_ocean,
    fixed_stays_on_props_system,
    system_stays_on_choose_system,
    system_stays_on_props_ocean,
    system_stays_on_props_system,
    system_stays_on_system_dark_false,
    system_stays_on_system_dark_true,
  }

  val resources_owned = running.subseteq(owned(state))
  val can_move = finals.contains(state) or movable.contains(state)
  val inv = resources_owned and can_move
}
