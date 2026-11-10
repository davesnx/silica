module Theme = Silica_ui.Theme

type warning = { warning : string; warning_surface : string }

let with_warning theme { warning; warning_surface } =
  Theme.make ~name:(Theme.name theme) ~label:(Theme.label theme)
    ~scheme:(Theme.scheme theme)
    ~extra:[ ("warning", warning); ("warning-surface", warning_surface) ]
    (Theme.tokens theme)

let light =
  with_warning Theme.light { warning = "#b45309"; warning_surface = "#fffbeb" }

let dark =
  with_warning Theme.dark { warning = "#fbbf24"; warning_surface = "#451a03" }

let high_contrast =
  with_warning Theme.high_contrast
    { warning = "#ffd700"; warning_surface = "#000000" }

let ocean =
  with_warning Theme.ocean { warning = "#fcd34d"; warning_surface = "#3a2a05" }

let set = Theme.set ~light ~dark [ light; dark; high_contrast; ocean ]
let choice cookie = Theme.choice_of_cookie set cookie
