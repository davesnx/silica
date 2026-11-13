let mount_dialog root =
  Silica_browser.mount ~root:(Silica_browser.by_id root) Dialog_events.component
let mount_checkbox root =
  Silica_browser.mount
    ~root:(Silica_browser.by_id root)
    Checkbox_events.component
let mount_switch root =
  Silica_browser.mount ~root:(Silica_browser.by_id root) Switch_events.component
let mount_tabs root =
  Silica_browser.mount ~root:(Silica_browser.by_id root) Tabs_events.component
let mount_toast root =
  Silica_browser.mount ~root:(Silica_browser.by_id root) Toast_events.component
let mount_combobox root =
  Silica_browser.mount
    ~root:(Silica_browser.by_id root)
    Combobox_events.component

let mount_command_menu root =
  Silica_browser.mount
    ~root:(Silica_browser.by_id root)
    Command_menu_events.component
let mount_menu root =
  Silica_browser.mount ~root:(Silica_browser.by_id root) Menu_events.component
let mount_select root =
  Silica_browser.mount ~root:(Silica_browser.by_id root) Select_events.component
let mount_popover root =
  Silica_browser.mount
    ~root:(Silica_browser.by_id root)
    Popover_events.component
let mount_tooltip root =
  Silica_browser.mount
    ~root:(Silica_browser.by_id root)
    Tooltip_events.component
