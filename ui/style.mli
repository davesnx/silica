(** Default classes for the parts of each component, written with the theme
    tokens of [silica-ui.css] ([bg-ui-surface], [text-ui-text], ...). An app
    that imports [silica-ui.css] in its Tailwind input gets the CSS for them,
    because that file has an [@source] line for this folder. *)

val button : string
(** Classes for a plain button. *)

val dialog : Dialog.classes

val popover : Popover.classes

val tooltip : Tooltip.classes

val menu : Menu.classes

val select : Select.classes

val combobox : Combobox.classes

val command_menu : Command_menu.classes

val checkbox : Checkbox.classes

val switch : Switch.classes

val tabs : Tabs.classes

val tab_panel : string
(** Classes for a panel of {!Tabs}. *)

val toast : Toast.classes
