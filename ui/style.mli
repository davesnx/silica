(** Default classes for the parts of each component, written with the theme
    tokens of [silica-ui.css] ([bg-ui-surface], [text-ui-text], ...). An app
    that imports [silica-ui.css] in its Tailwind input gets the CSS for them,
    because that file has an [@source] line for this folder. *)

val button : string
(** Classes for a plain button. *)

val dialog : Dialog.classes
(** Classes for {!Dialog}. *)

val popover : Popover.classes
(** Classes for {!Popover}. *)

val tooltip : Tooltip.classes
(** Classes for {!Tooltip}. *)

val menu : Menu.classes
(** Classes for {!Menu}. *)

val select : Select.classes
(** Classes for {!Select}. *)

val combobox : Combobox.classes
(** Classes for {!Combobox}. *)

val command_menu : Command_menu.classes
(** Classes for {!Command_menu}. *)

val checkbox : Checkbox.classes
(** Classes for {!Checkbox}. *)

val switch : Switch.classes
(** Classes for {!Switch}. *)

val tabs : Tabs.classes
(** Classes for {!Tabs}. *)

val tab_panel : string
(** Classes for a panel of {!Tabs}. *)

val toast : Toast.classes
(** Classes for {!Toast}. *)
