# API: the library silica-ui

## Silica_ui.Dialog

A modal dialog (WAI-ARIA dialog pattern; Base UI Dialog and Alert Dialog).

The popup is painted in a portal at the end of the body, over a backdrop. While it is open, focus stays inside it, the page does not scroll, Escape and a press outside close it (unless `dismissible` is `false`; an alert dialog ignores outside presses). On close, focus goes to the trigger, or, with no trigger, back to the element that had it before.

The content is the caller's: `component` takes its view, and the props carry its data. The dialog gives the content events to the parent, and closes when `closes` says so.

Styling: classes per part (`classes`) and the attributes `data-open` and `data-closed` on the backdrop and the popup, `data-popup-open` on the trigger.

```ocaml
type classes = {
  trigger : string;
  backdrop : string;
  popup : string;
  title : string;
  description : string;
  close : string;
}
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type ('content, 'ce) props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  title : string;  (** The heading; the popup is labelled by it. *)
  description : string option;
  trigger : string option;
      (** The label of a trigger button; [None]: the parent opens it. *)
  close : string option;  (** The label of a close button in the popup. *)
  open_ : bool option;
      (** When this changes to [Some b], the dialog opens or closes. With [None]
          only the trigger opens it. A parent that sets it must follow the open
          changes it gets, or it cannot set the same value again. *)
  dismissible : bool;  (** Escape and an outside press close it. *)
  alert : bool;  (** [role="alertdialog"]: an outside press never closes it. *)
  classes : classes;
  content : 'content;
      (** The data the content shows. Compared with [=]: no functions. *)
}
```

```ocaml
type 'ce event =
  | Open_request of Popup.reason
  | Close_request of Popup.reason
  | Content of 'ce
```

```ocaml
type state = Closed | Open
```

```ocaml
type ('content, 'ce) t = {
  component : (('content, 'ce) props, state, 'ce event) Silica.Component.t;
      (** The machine with its view. The checker reads it. *)
  closes : 'ce -> bool;
}
```

A dialog for one kind of content.

```ocaml
val component :
  id:string ->
  closes:('ce -> bool) ->
  ?event_name:('ce -> string) ->
  ('content -> 'ce Silica.Node.t) ->
  ('content, 'ce) t
```

`component ~id ~closes view` is a dialog whose content is `view content`. `id` is the id of the machine, as in the spec and the checker. The dialog closes after a content event for which `closes` is `true`, with reason `Popup.Close_press`. `event_name` names the content events (see `Silica.Machine.event_name`): the dialog names its own events `"open_request <reason>"` and `"close_request <reason>"` (see `Popup.reason_name`), and a content event by `event_name`, or `"content"` without it.

```ocaml
val view :
  content:('content -> 'ce Silica.Node.t) ->
  ('content, 'ce) props ->
  state ->
  'ce event Silica.Node.t
```

The view of a state.

```ocaml
val props :
  id:string ->
  title:string ->
  ?description:string ->
  ?trigger:string ->
  ?close:string ->
  ?open_:bool ->
  ?dismissible:bool ->
  ?alert:bool ->
  ?classes:classes ->
  'content ->
  ('content, 'ce) props
```

`dismissible` defaults to `true`, `alert` to `false`.

```ocaml
val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_content:('ce -> 'e) ->
  ('content, 'ce) t ->
  ('content, 'ce) props ->
  'e Silica.Node.t
```

The node. `on_open_change` hears each open or close that the dialog did by itself (trigger, Escape, outside press, close part), not those that `open_` asked for.

## Silica_ui.Popover

A non-modal popup next to a trigger button (WAI-ARIA disclosure of a non-modal dialog; Base UI Popover).

The trigger opens and closes the popup. The popup is painted in a portal at the end of the body and kept next to the trigger. When it opens, focus moves into it. Focus is not trapped and the page still scrolls. Escape closes it and focuses the trigger; a press outside the popup and the trigger closes it and leaves focus where the press put it.

The content is the caller's, as in `Dialog`: `component` takes its view, and the props carry its data. The popover gives the content events to the parent, and closes when `closes` says so.

Styling: classes per part (`classes`), `data-open` and `data-closed` on the trigger and the popup, `data-popup-open` on the trigger, and the `data-side` and `data-align` attributes that the renderer writes on the popup.

```ocaml
type classes = {
  trigger : string;
  popup : string;
  title : string;
  close : string;
}
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type ('content, 'ce) props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  trigger : string;  (** The label of the trigger button. *)
  title : string option;
      (** A heading in the popup; the popup is labelled by it. *)
  close : string option;  (** The label of a close button in the popup. *)
  open_ : bool option;
      (** When this changes to [Some b], the popover opens or closes. With
          [None] only the trigger and the dismiss layer change it. A parent that
          sets it must follow the open changes it gets, or it cannot set the
          same value again. *)
  place : Silica.Place.t;  (** Where the popup goes next to the trigger. *)
  classes : classes;
  content : 'content;
      (** The data the content shows. Compared with [=]: no functions. *)
}
```

```ocaml
type 'ce event =
  | Open_request of Popup.reason
  | Close_request of Popup.reason
  | Content of 'ce
```

```ocaml
type state = Closed | Open
```

```ocaml
type ('content, 'ce) t = {
  component : (('content, 'ce) props, state, 'ce event) Silica.Component.t;
      (** The machine with its view. The checker reads it. *)
  closes : 'ce -> bool;
}
```

A popover for one kind of content.

```ocaml
val component :
  id:string ->
  closes:('ce -> bool) ->
  ?event_name:('ce -> string) ->
  ('content -> 'ce Silica.Node.t) ->
  ('content, 'ce) t
```

`component ~id ~closes view` is a popover whose content is `view content`. `id` is the id of the machine, as in the spec and the checker. The popover closes after a content event for which `closes` is `true`, with reason `Popup.Close_press`. `event_name` names the content events (see `Silica.Machine.event_name`): the popover names its own events `"open_request <reason>"` and `"close_request <reason>"` (see `Popup.reason_name`), and a content event by `event_name`, or `"content"` without it.

```ocaml
val view :
  content:('content -> 'ce Silica.Node.t) ->
  ('content, 'ce) props ->
  state ->
  'ce event Silica.Node.t
```

The view of a state.

```ocaml
val props :
  id:string ->
  trigger:string ->
  ?title:string ->
  ?close:string ->
  ?open_:bool ->
  ?place:Silica.Place.t ->
  ?classes:classes ->
  'content ->
  ('content, 'ce) props
```

`place` defaults to `Silica.Place.default`.

```ocaml
val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_content:('ce -> 'e) ->
  ('content, 'ce) t ->
  ('content, 'ce) props ->
  'e Silica.Node.t
```

The node. `on_open_change` hears each open or close that the popover did by itself (trigger, Escape, outside press, close part, content), not those that `open_` asked for.

## Silica_ui.Tooltip

A short text that describes a button (WAI-ARIA tooltip pattern; Base UI Tooltip).

The tooltip opens when the pointer rests on the trigger for `delay`, or at once when the trigger gets focus while the pointer is not waiting on it (a mouse press focuses the trigger, and must not show the tooltip). It closes `close_delay` after the pointer leaves the trigger and the popup, and at once on blur, Escape or a press on the trigger. While it closes, the pointer can move onto the popup and keep it open. The popup is painted in a portal at the end of the body, next to the trigger. It never takes focus. While it is open, the trigger is described by it (`aria-describedby`).

Styling: classes per part (`classes`), `data-open` and `data-closed` on the trigger and the popup, `data-popup-open` on the trigger, and the `data-side` and `data-align` attributes that the renderer writes on the popup.

```ocaml
type classes = { trigger : string; popup : string }
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type delay =
  | Instant  (** 0 ms: at once. *)
  | Short  (** 300 ms. *)
  | Medium  (** 600 ms. *)
  | Long
```

A delay of the tooltip. A timer of a state has a fixed duration, so the delays are a fixed set.

```ocaml
val delay_ms : delay -> int
```

The delay in milliseconds.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The text of the trigger button, its accessible name. *)
  tip : string;  (** The text of the tooltip. *)
  delay : delay;  (** From the pointer on the trigger to the open tooltip. *)
  close_delay : delay;
      (** From the pointer out of the trigger and the popup to the closed
          tooltip. *)
  place : Silica.Place.t;  (** Where the popup goes next to the trigger. *)
  classes : classes;
}
```

```ocaml
type event =
  | Hover  (** The pointer entered the closed trigger. *)
  | Focus  (** The trigger got focus. *)
  | Cancel  (** The pointer left, or pressed, the trigger before the delay. *)
  | Leave  (** The pointer left the open trigger or popup. *)
  | Resume  (** The pointer or focus came back while the tooltip closes. *)
  | Close of Popup.reason
  | Elapsed of delay
```

```ocaml
type state =
  | Closed
  | Opening  (** Hidden; waits for [delay]. *)
  | Open
  | Closing
```

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

The machine of the component. The checker reads it.

```ocaml
val component : (props, state, event) Silica.Component.t
```

The machine with its view.

```ocaml
val view : props -> state -> event Silica.Node.t
```

The view of a state.

```ocaml
val props :
  id:string ->
  label:string ->
  tip:string ->
  ?delay:delay ->
  ?close_delay:delay ->
  ?place:Silica.Place.t ->
  ?classes:classes ->
  unit ->
  props
```

`delay` defaults to `Medium`, `close_delay` to `Instant`, `place` to the top side, centered, 4 pixels away, 8 pixels from the edges of the viewport.

```ocaml
val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  props ->
  'e Silica.Node.t
```

The node. `on_open_change` hears each time the tooltip is shown or hidden, with the reason: `Trigger_hover`, `Trigger_focus` (focus and blur), `Trigger_press` or `Escape_key`.

## Silica_ui.Menu

A menu button (WAI-ARIA menu button pattern; Base UI Menu).

A trigger button opens a list of items, `role="menu"`, painted in a portal at the end of the body and kept below the trigger. Keyboard focus moves to the highlighted item: Enter, Space and ArrowDown on the trigger open the menu on the first enabled item, ArrowUp on the last. In the menu, ArrowDown and ArrowUp move (and wrap), Home and End go to the ends, a letter goes to the next item whose label starts with it, Enter and Space choose the item. Disabled items are skipped. Escape closes the menu and focuses the trigger, Tab closes it and lets focus move on, and a press outside closes it.

Styling: classes per part (`classes`); `data-open` and `data-closed` on the popup, `data-popup-open` on the trigger, `data-highlighted` and `data-disabled` on the items.

```ocaml
type item = {
  value : string;  (** Given to [on_select] when the item is chosen. *)
  label : string;
  disabled : bool;
}
```

```ocaml
type classes = { trigger : string; popup : string; item : string }
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The text of the trigger; the menu is labelled by it. *)
  items : item list;
  open_ : bool option;
      (** When this changes to [Some b], the menu opens or closes. With [None]
          only the user opens it. *)
  classes : classes;
}
```

```ocaml
type nav = Next | Previous | First | Last | Letter of char
```

```ocaml
type edge = Start | End
```

```ocaml
type event =
  | Trigger_press
  | Open_key of Popup.reason * edge
      (** A key on the closed trigger: open on the first ([Start]) or the last
          ([End]) enabled item. *)
  | Move of nav
  | Highlight of int  (** The pointer entered the item at this position. *)
  | Choose of int  (** The item at this position was chosen. *)
  | Close of Popup.reason
```

```ocaml
type state =
  | Closed
  | Open of int option
```

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

The machine of the component. The checker reads it.

```ocaml
val component : (props, state, event) Silica.Component.t
```

The machine with its view.

```ocaml
val view : props -> state -> event Silica.Node.t
```

The view of a state.

```ocaml
val item_id : string -> int -> string
```

`item_id id i` is the element id and ref name of the item at position `i` of the menu `id`.

```ocaml
val props :
  id:string ->
  label:string ->
  ?open_:bool ->
  ?classes:classes ->
  item list ->
  props
```

`open_` defaults to `None`: only the trigger opens the menu. `classes` defaults to `no_classes`.

```ocaml
val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_select:(string -> 'e) ->
  props ->
  'e Silica.Node.t
```

The node. `on_select` gets the value of each chosen item. `on_open_change` hears each open or close that the menu did by itself, not those that `open_` asked for.

## Silica_ui.Select

A select-only combobox (WAI-ARIA select-only combobox pattern; Base UI Select).

A trigger button, `role="combobox"`, shows the label of the selected option or a placeholder and opens a list of options, `role="listbox"`, painted in a portal at the end of the body and kept below the trigger. Keyboard focus stays on the trigger; `aria-activedescendant` names the highlighted option. Enter, Space, ArrowDown and ArrowUp on the closed trigger open the list on the selected option (or the first enabled one). While it is open, ArrowDown and ArrowUp move (no wrap), Home and End go to the ends, a letter goes to the next option whose label starts with it, Enter and Space select the highlighted option and close, Tab selects it, closes and lets focus move on, Escape and a press outside close with no change. Disabled options are skipped and cannot be selected.

With a `name`, a hidden input carries the value for a form.

Styling: classes per part (`classes`); `data-placeholder` on the trigger when no option is selected, `data-popup-open` on the trigger, `data-open` and `data-closed` on the list, `data-selected`, `data-highlighted` and `data-disabled` on the options.

```ocaml
type item = { value : string; label : string; disabled : bool }
```

```ocaml
type classes = { trigger : string; popup : string; option : string }
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The accessible name of the trigger and the list. *)
  items : item list;
  value : string option;
      (** When this changes, the select takes it as its value. *)
  placeholder : string;  (** Shown when no option is selected. *)
  name : string option;  (** The name of the hidden input for forms. *)
  classes : classes;
}
```

```ocaml
type nav = Next | Previous | First | Last | Letter of char
```

```ocaml
type event =
  | Trigger_press
  | Open_key of Popup.reason
  | Move of nav
  | Highlight of int  (** The pointer entered the option at this position. *)
  | Select of int * Popup.reason
      (** The option at this position was selected: by a press or Enter and
          Space ([Item_press]) or by Tab ([Focus_out]). *)
  | Close of Popup.reason
```

```ocaml
type state =
  | Closed of string option  (** The selected value. *)
  | Open of string option * int option
```

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

The machine of the component. The checker reads it.

```ocaml
val component : (props, state, event) Silica.Component.t
```

The machine with its view.

```ocaml
val view : props -> state -> event Silica.Node.t
```

The view of a state.

```ocaml
val option_id : string -> int -> string
```

`option_id id i` is the element id and ref name of the option at position `i` of the select `id`.

```ocaml
val props :
  id:string ->
  label:string ->
  ?value:string ->
  ?placeholder:string ->
  ?name:string ->
  ?classes:classes ->
  item list ->
  props
```

`placeholder` defaults to `""`.

```ocaml
val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_value_change:(string -> 'e) ->
  props ->
  'e Silica.Node.t
```

The node. `on_value_change` hears each value that the user selected and that differs from the current one, not the values that `value` asked for. `on_open_change` hears each open or close that the select did by itself.

## Silica_ui.Combobox

A text input with a list of options that filters as you type (WAI-ARIA combobox pattern with list autocomplete; Base UI Combobox).

Focus stays in the input. The highlighted option is shown by `aria-activedescendant` on the input and `data-highlighted` on the option. Typing filters the items by their label and keywords (case-insensitive substring) and opens the list. Keys in the input: ArrowDown opens the list and moves the highlight down, ArrowUp moves it up (both wrap), Alt+ArrowDown opens the list and does not move it, Enter chooses the highlighted option, Escape closes the list or, when it is closed, clears the text. An Escape that the combobox does not use reaches the dismiss layers around it. A press on an option chooses it. Choosing puts the label of the option in the input and closes the list.

Two modes (`inline` in `props`):

- popup: the list is painted in a portal, under the input, while open. A press outside the input and the list closes it.
- inline: the list is always painted under the input, in place, and the first match is always highlighted. For a list inside a dialog, such as `Command_menu`.

Styling: classes per part (`classes`), `data-open` or `data-closed` on the list, `data-highlighted`, `data-selected` and `data-disabled` on the options.

```ocaml
type item = {
  value : string;  (** What {!make} reports when the item is chosen. *)
  label : string;  (** The text of the option; the filter reads it. *)
  keywords : string list;  (** More words that the filter matches. *)
  disabled : bool;  (** Shown, but not highlighted or chosen. *)
}
```

```ocaml
val item : ?keywords:string list -> ?disabled:bool -> string -> string -> item
```

`item value label`.

```ocaml
val matches : string -> item -> bool
```

`matches query item`: the query is a substring of the label or of a keyword, ignoring case. The empty query matches every item.

```ocaml
type classes = {
  root : string;
  input : string;
  list : string;
  option : string;
  empty : string;
}
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The accessible name of the input ([aria-label]). *)
  placeholder : string;
  empty : string;  (** The text of the status shown when nothing matches. *)
  items : item list;
  inline : bool;  (** See the two modes above. *)
  classes : classes;
}
```

```ocaml
type data = {
  query : string;  (** The text of the input. *)
  highlighted : string option;  (** The value of the highlighted option. *)
  selected : string option;  (** The value of the last chosen option. *)
}
```

```ocaml
type event =
  | Typed of string  (** The text of the input changed. *)
  | Move of [ `Next | `Previous | `First | `Last ]
      (** ArrowDown or ArrowUp: open the list and move the highlight. Home or
          End, while an option is highlighted: highlight the first or the last
          option. *)
  | Show  (** Alt+ArrowDown: open the list. *)
  | Highlight of string  (** The pointer is on the option with this value. *)
  | Choose_highlighted  (** Enter. *)
  | Choose of string  (** A press on the option with this value. *)
  | Escape
  | Outside_press
  | Focus_out
```

```ocaml
type state =
  | Closed of data  (** Popup mode, list not shown. *)
  | Open of data  (** Popup mode, list shown. *)
  | Inline of data
```

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

The machine of the component. The checker reads it.

```ocaml
val component : (props, state, event) Silica.Component.t
```

The machine with its view.

```ocaml
val view : props -> state -> event Silica.Node.t
```

The view of a state.

```ocaml
val props :
  id:string ->
  label:string ->
  ?placeholder:string ->
  ?empty:string ->
  ?inline:bool ->
  ?classes:classes ->
  item list ->
  props
```

`placeholder` defaults to `""`, `empty` to `"No results"`, `inline` to `false`.

```ocaml
val make :
  ?key:string -> ?on_value_change:(string -> 'e) -> props -> 'e Silica.Node.t
```

The node. `on_value_change` hears the value of each option the user chose.

## Silica_ui.Command_menu

A command menu (command palette): a modal `Dialog` that holds an inline `Combobox` with the first match always highlighted. Base UI has no such component; this is the composition that its documentation suggests.

The parent opens it with `open_` (for example from a global shortcut such as Ctrl+K, with `Silica.Source.keys`) and follows `on_open_change`. When it opens, focus goes to the search input, with empty text. Enter or a press on an option runs the option: the menu reports its value and closes. Escape (with empty text; with text, the first Escape clears it) and a press outside close it. On close, focus goes back to the element that had it.

```ocaml
type classes = { dialog : Dialog.classes; combobox : Combobox.classes }
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  title : string;  (** The heading of the dialog and the name of the input. *)
  placeholder : string;
  empty : string;  (** The text shown when nothing matches. *)
  items : Combobox.item list;
  open_ : bool option;
      (** When this changes to [Some b], the menu opens or closes. The parent
          must follow the open changes it gets, as with {!Dialog}. *)
  classes : classes;
}
```

```ocaml
val dialog : (Combobox.props, string) Dialog.t
```

The dialog whose content is the combobox; its event is the value of the chosen option. Its machine is the machine of the command menu.

```ocaml
val props :
  id:string ->
  title:string ->
  ?placeholder:string ->
  ?empty:string ->
  ?open_:bool ->
  ?classes:classes ->
  Combobox.item list ->
  props
```

`placeholder` defaults to `""`, `empty` to `"No results"`.

```ocaml
val menu_props : props -> (Combobox.props, string) Dialog.props
```

The props of the dialog that `make` renders.

```ocaml
val make :
  ?key:string ->
  ?on_open_change:(bool -> Popup.reason -> 'e) ->
  ?on_run:(string -> 'e) ->
  props ->
  'e Silica.Node.t
```

The node. `on_run` hears the value of each option the user ran. `on_open_change` hears each open change the menu did by itself: a close after a run has the reason `Popup.Close_press`.

## Silica_ui.Checkbox

A checkbox (WAI-ARIA checkbox pattern; Base UI Checkbox).

A `<button type="button" role="checkbox">` whose text is the label, so the label is its accessible name and a press on it toggles the box. Space and a press toggle it; Enter does nothing (as in Base UI). With a `name`, a hidden input carries the value `"on"` in a form while the box is checked, as a native checkbox does; there is no input while it is not checked.

Styling: classes per part (`classes`) and the attributes `data-checked`, `data-unchecked`, `data-indeterminate` and `data-disabled` on the button. The indicator is a `<span data-ui="checkbox-indicator">`.

```ocaml
type classes = { root : string; indicator : string }
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The visible text and the accessible name. *)
  checked : bool;
      (** When this changes, the box takes it. A parent that sets it must follow
          the changes it gets, or it cannot set the same value again. *)
  indeterminate : bool;
      (** When this changes to [true], the box shows the mixed state
          ([aria-checked="mixed"]); when it changes to [false], it shows
          [checked]. A press on a mixed box checks it. *)
  disabled : bool;
  name : string option;  (** The name of the hidden input for forms. *)
  classes : classes;
}
```

```ocaml
type event = Toggle | Enter_key
```

```ocaml
type state = Unchecked | Checked | Indeterminate
```

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

The machine of the component. The checker reads it.

```ocaml
val component : (props, state, event) Silica.Component.t
```

The machine with its view.

```ocaml
val view : props -> state -> event Silica.Node.t
```

The view of a state.

```ocaml
val props :
  id:string ->
  label:string ->
  ?checked:bool ->
  ?indeterminate:bool ->
  ?disabled:bool ->
  ?name:string ->
  ?classes:classes ->
  unit ->
  props
```

`checked`, `indeterminate` and `disabled` default to `false`.

```ocaml
val make :
  ?key:string -> ?on_checked_change:(bool -> 'e) -> props -> 'e Silica.Node.t
```

The node. `on_checked_change` hears each change that a press made, not those that the props asked for.

## Silica_ui.Switch

A switch (WAI-ARIA switch pattern; Base UI Switch).

A `<button type="button" role="switch">` whose text is the label, so the label is its accessible name. A press, Space and Enter toggle it (the native button turns both keys into a press). With a `name`, a hidden input carries the value `"on"` in a form while the switch is on, as a native checkbox does; there is no input while it is off.

Styling: classes per part (`classes`) and the attributes `data-checked`, `data-unchecked` and `data-disabled` on the button and on the thumb, a `<span data-ui="switch-thumb">` in a `<span data-ui="switch-track">`.

```ocaml
type classes = { root : string; track : string; thumb : string }
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  label : string;  (** The visible text and the accessible name. *)
  checked : bool;
      (** When this changes, the switch takes it. A parent that sets it must
          follow the changes it gets, or it cannot set the same value again. *)
  disabled : bool;
  name : string option;  (** The name of the hidden input for forms. *)
  classes : classes;
}
```

```ocaml
type event = Toggle
```

```ocaml
type state = Unchecked | Checked
```

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

The machine of the component. The checker reads it.

```ocaml
val component : (props, state, event) Silica.Component.t
```

The machine with its view.

```ocaml
val view : props -> state -> event Silica.Node.t
```

The view of a state.

```ocaml
val props :
  id:string ->
  label:string ->
  ?checked:bool ->
  ?disabled:bool ->
  ?name:string ->
  ?classes:classes ->
  unit ->
  props
```

`checked` and `disabled` default to `false`.

```ocaml
val make :
  ?key:string -> ?on_checked_change:(bool -> 'e) -> props -> 'e Silica.Node.t
```

The node. `on_checked_change` hears each change that a press made, not those that the props asked for.

## Silica_ui.Tabs

Tabs with manual activation (WAI-ARIA tabs pattern; Base UI Tabs with `activateOnFocus = false`).

The component paints the tab list: a `role="tablist"` of `<button role="tab">`. One tab is the tab stop (`tabindex="0"`): the focused one. Arrow keys along the orientation move focus to the next or previous enabled tab, with wrap; Home and End move it to the first and last enabled tab. A press, Enter or Space (the native button turns both keys into a press) selects the focused tab. Moving focus does not select.

The caller paints the panels with `panel`, from the value it follows with `on_value_change`.

Element ids: the tab of value `v` is `id ^ "-tab-" ^ v` and its panel is `id ^ "-panel-" ^ v`, so values must be valid in an id.

Styling: classes per part (`classes`), `data-active` on the selected tab, `data-disabled` on disabled tabs, `data-orientation` on the list.

```ocaml
type orientation = Horizontal | Vertical
```

```ocaml
type tab = {
  value : string;  (** Unique among the tabs. *)
  label : string;  (** The text of the tab and its accessible name. *)
  disabled : bool;
}
```

```ocaml
type classes = { list : string; tab : string }
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  tabs : tab list;
  value : string;
      (** The selected tab. When this changes, the list takes it. A parent that
          sets it must follow the changes it gets, or it cannot set the same
          value again. *)
  orientation : orientation;
  classes : classes;
}
```

```ocaml
type move = Next | Previous | First | Last
```

```ocaml
type event =
  | Select of string  (** A press on the tab with this value. *)
  | Move of move
```

```ocaml
type model = { selected : string; focused : string  (** The tab stop. *) }
```

```ocaml
type state = Ready of model
```

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

The machine of the component. The checker reads it.

```ocaml
val component : (props, state, event) Silica.Component.t
```

The machine with its view.

```ocaml
val view : props -> state -> event Silica.Node.t
```

The view of a state.

```ocaml
val tab : ?disabled:bool -> value:string -> string -> tab
```

`tab ~value label`. `disabled` defaults to `false`.

```ocaml
val props :
  id:string ->
  ?orientation:orientation ->
  ?classes:classes ->
  value:string ->
  tab list ->
  props
```

`orientation` defaults to `Horizontal`.

```ocaml
val make :
  ?key:string -> ?on_value_change:(string -> 'e) -> props -> 'e Silica.Node.t
```

The node. `on_value_change` hears each selection that a press made, not those that `value` asked for, and not a press on the selected tab.

```ocaml
val panel :
  ?class_:string ->
  id:string ->
  value:string ->
  selected:string ->
  'e Silica.Node.t list ->
  'e Silica.Node.t
```

`panel ~id ~value ~selected children` is the `role="tabpanel"` of the tab `value` of the tabs `id`, labelled by its tab, `hidden` unless `value` is `selected`.

## Silica_ui.Toast

Toasts (Base UI Toast).

The parent owns the list of toasts and gives it in the props; the component paints them in a viewport (`role="region"`, `aria-live="polite"`) and tells the parent which toast to close with `on_close`. The parent removes it.

A toast closes on a press of its close button, on Escape while focus is in it, and by itself: while the list is not empty and the pointer and focus are not in the viewport, the oldest toast (the first of the list) closes after `timeout_ms`. Pointer or focus in the viewport pauses the time; when both leave, the time starts again from zero. A new list starts the time again too. The time counts in steps of `tick_ms`, so a toast closes up to one step after `timeout_ms`.

Element ids: the toast with key `k` is `id ^ "-toast-" ^ k`, its title `id ^ "-toast-" ^ k ^ "-title"`, its description `... ^ "-description"` and its close button `... ^ "-close"`.

Styling: classes per part (`classes`), `data-priority="low"` or `"high"` on each toast, `data-paused` on the viewport while the time is paused.

```ocaml
type priority = Low | High
```

```ocaml
type toast = {
  key : string;  (** Unique in the list. *)
  title : string;  (** The heading; the toast is labelled by it. *)
  description : string option;
  priority : priority;
      (** [High]: [role="alertdialog"]; [Low]: [role="dialog"]. *)
}
```

```ocaml
type classes = {
  viewport : string;
  toast : string;
  title : string;
  description : string;
  close : string;
}
```

```ocaml
val no_classes : classes
```

All empty.

```ocaml
type props = {
  id : string;  (** Unique in the page; the ids of the parts start with it. *)
  toasts : toast list;  (** Oldest first. *)
  timeout_ms : int;  (** More than 0. *)
  label : string;  (** The accessible name of the viewport. *)
  close_label : string;  (** The accessible name of each close button. *)
  classes : classes;
}
```

```ocaml
type event =
  | Close of string
      (** The close button or Escape in the toast with this key. *)
  | Tick
  | Hover of bool  (** The pointer enters ([true]) or leaves the viewport. *)
  | Focus of bool
```

```ocaml
type model = { hovered : bool; focused : bool }
```

```ocaml
type state =
  | Empty of model  (** No toast. *)
  | Running of model * int
      (** The time runs; the [int] is the time since it started, in ms. *)
  | Paused of model  (** The pointer or focus is in the viewport. *)
  | Expired of model
```

```ocaml
val tick_ms : int
```

The step of the time: 100.

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

The machine of the component. The checker reads it.

```ocaml
val component : (props, state, event) Silica.Component.t
```

The machine with its view.

```ocaml
val view : props -> state -> event Silica.Node.t
```

The view of a state.

```ocaml
val toast :
  ?description:string -> ?priority:priority -> key:string -> string -> toast
```

`toast ~key title`. `priority` defaults to `Low`.

```ocaml
val props :
  id:string ->
  ?timeout_ms:int ->
  ?label:string ->
  ?close_label:string ->
  ?classes:classes ->
  toast list ->
  props
```

`timeout_ms` defaults to 5000, `label` to `"Notifications"`, `close_label` to `"Close"`.

Raises `Invalid_argument` when `timeout_ms` is not more than 0.

```ocaml
val make : ?key:string -> ?on_close:(string -> 'e) -> props -> 'e Silica.Node.t
```

The node. `on_close` gets the key of each toast to remove.

## Silica_ui.Theme

Themes: named sets of design tokens, a choice between them, and the machine that applies the choice to the page.

A theme gives a value to each token of `tokens`. `stylesheet` turns a `set` of themes into CSS: one `[data-theme="name"]` block of CSS custom properties (`--ui-surface`, ...) and `color-scheme` per theme, a `[data-theme="system"]` block that follows `prefers-color-scheme`, and the Tailwind `@theme inline` map that makes utilities such as `bg-ui-surface` read those properties. An app writes the stylesheet to a file in a dune rule and imports it in its Tailwind input.

The theme component (`switcher` or `provider`) holds the `choice`. At the start and at each change it sets `data-theme` on the `<html>` element (`Silica.Action.Set_document_attribute`); a server render writes it into the first HTML. A change made in the page is stored in the cookie `cookie_name`, which the server and the browser read before the first props (`choice_of_cookie`), so the first HTML has the right theme. A switch changes one attribute: no other component renders again.

```
  let set = Theme.set ~light:Theme.light ~dark:Theme.dark Theme.shipped

  (* server page and browser entry *)
  let choice =
    Theme.choice_of_cookie set cookie
      (* in a view *)
      Theme.switcher
      ~on_change:(fun current -> Theme_changed current)
      (Theme.props ~id:"theme" set choice)
```

```ocaml
type scheme =
  | Light
  | Dark
```

```ocaml
type tokens = {
  surface : string;  (** The page background. *)
  raised : string;  (** The background of popups, dialogs and cards. *)
  text : string;  (** Text on [surface], [raised] and [highlight]. *)
  muted : string;  (** Secondary text on [surface] and [raised]. *)
  border : string;
  highlight : string;  (** The background of a highlighted or hovered item. *)
  accent : string;  (** The background of a main button, a checked control. *)
  accent_text : string;  (** Text on [accent]. *)
  danger : string;  (** Error text on [surface]. *)
  backdrop : string;  (** The layer behind a modal dialog. *)
  focus : string;  (** The focus ring. *)
  radius : string;  (** The corner radius of controls. *)
  shadow : string;  (** The shadow of popups. *)
  font : string;  (** The font family of the page. *)
  spacing : string;
      (** The unit of the spacing scale: Tailwind [p-4] is 4 times it. *)
}
```

The tokens of silica-ui. Each one is a CSS value, such as `"#ffffff"` or `"0.5rem"`. A theme must give all of them, so a new token is a compile error in each theme that does not have it. Tailwind names: `ui-<token>` for colours (`bg-ui-surface`, `text-ui-accent-text`), `rounded-ui`, `shadow-ui`, `font-ui`; `spacing` is the Tailwind `--spacing`.

```ocaml
type t
```

A theme.

```ocaml
val make :
  name:string ->
  ?label:string ->
  scheme:scheme ->
  ?extra:(string * string) list ->
  tokens ->
  t
```

`name` is the value of `data-theme` and of the cookie: lower case ASCII letters, digits and `-`, starting with a letter, not `"system"`. `label` is the text in a switcher (default: `name`). `extra` are the tokens of the app, as pairs of a name (same rules as `name`) and a value; the CSS property is `--<name>`. A value is not empty and may not hold a semicolon, a brace, an angle bracket, a backslash, a double quote or a line break; it holds an even count of single quotes.

Raises `Invalid_argument` for a bad name or value.

```ocaml
val name : t -> string
```

```ocaml
val label : t -> string
```

```ocaml
val scheme : t -> scheme
```

```ocaml
val tokens : t -> tokens
```

```ocaml
val extra : t -> (string * string) list
```

The app tokens, as given to `make`.

**Shipped themes**

```ocaml
val light : t
```

`"light"`: white surfaces, indigo accent.

```ocaml
val dark : t
```

`"dark"`: gray surfaces, indigo accent.

```ocaml
val high_contrast : t
```

`"high-contrast"`: black surfaces, white text and borders, yellow accent.

```ocaml
val ocean : t
```

`"ocean"`: a brand theme with a dark scheme, navy surfaces and a cyan accent.

```ocaml
val shipped : t list
```

`light`, `dark`, `high_contrast` and `ocean`.

**Sets**

```ocaml
type set
```

The themes of an app, with the pair that `System` uses.

```ocaml
val set : light:t -> dark:t -> t list -> set
```

`set ~light ~dark themes`. Under `System` the page uses `light` when the system setting is light and `dark` when it is dark.

Raises `Invalid_argument` when `themes` is empty, two themes have the same name, `light` or `dark` is not in `themes`, `light` does not have the scheme `Light` or `dark` the scheme `Dark`, or two themes have different `extra` token names. The message names the theme and the token.

```ocaml
val themes : set -> t list
```

In the order given to `set`.

```ocaml
val system_light : set -> t
```

```ocaml
val system_dark : set -> t
```

**Choice**

```ocaml
type choice =
  | System  (** Follow the system setting ([prefers-color-scheme]), live. *)
  | Fixed of t
```

```ocaml
val choice_name : choice -> string
```

`"system"`, or the name of the theme.

```ocaml
val choice_of_name : set -> string -> choice option
```

The choice with that name in the set.

```ocaml
val cookie_name : string
```

`"silica-theme"`.

```ocaml
val choice_of_cookie : set -> string option -> choice
```

The choice stored in the cookie `cookie_name`, given its value (from `Silica_server.cookie` or `Silica_browser.cookie`). No cookie, or a name that is not in the set (a theme that the app removed), gives `System`.

**CSS**

```ocaml
val stylesheet : set -> string
```

The CSS of the set: the Tailwind `@theme inline` map of `tokens`, one `[data-theme="name"]` block per theme with its `color-scheme` and a `--ui-<token>` or `--<extra>` property per token, and the `[data-theme="system"]` blocks of `System`. The output depends only on the set.

```ocaml
val scope : ?key:string -> t -> 'e Silica.Node.t list -> 'e Silica.Node.t
```

A `<div data-theme="name">` around the nodes: they use the theme, whatever the theme of the page. silica-ui CSS gives every element with `data-theme` the `surface` background, the `text` colour and the `font`. A popup that a component in the nodes opens (a `Silica.Node.portal` with no target) uses the theme too: the browser renderer gives its container in the body the same `data-theme`.

**Machine**

```ocaml
type props = { id : string; label : string; set : set; choice : choice }
```

`id` is the id of the switcher (unique in the page). `label` is its accessible name. `choice` is the choice at the start; when it changes, the component takes it as a change made in the page.

```ocaml
val props : id:string -> ?label:string -> set -> choice -> props
```

`label` defaults to `"Theme"`.

```ocaml
type current = {
  choice : choice;
  theme : t;  (** The theme in use: under [System], the light or dark one. *)
}
```

```ocaml
type event =
  | Choose of string  (** The name of a choice, from the switcher. *)
  | System_dark of bool
```

```ocaml
type state =
  | Fixed_theme of t  (** A fixed theme. Its state name is ["fixed"]. *)
  | System_scheme of bool option
```

```ocaml
val state_codec : set -> state Silica.Codec.t
```

`["Fixed_theme", name]` or `["System_scheme", dark]`, with `dark` a boolean or `null`, for `Silica.Machine.register_codecs` on `machine`. A theme name that the set does not have does not decode.

```ocaml
val machine : (props, state, event) Silica.Machine.t
```

In `System_scheme` it holds `Source.media "(prefers-color-scheme: dark)"`. Each change of choice sets `data-theme`; a change made in the page (a `Choose`, or props with another choice) also sets the cookie for one year.

```ocaml
val current : props -> state -> current option
```

`None` under `System_scheme` until the system setting is known (always in a server render).

```ocaml
val switcher_component : (props, state, event) Silica.Component.t
```

The machine with a `Select` of `"System"` and the label of each theme.

```ocaml
val provider_component : (props, state, event) Silica.Component.t
```

The machine with an empty view.

```ocaml
val switcher :
  ?key:string -> ?on_change:(current -> 'e) -> props -> 'e Silica.Node.t
```

The theme component with its switcher. `on_change` hears the current theme after each `Choose` and each change of the system setting under `System`, including the first one. Render one theme component per page.

```ocaml
val provider :
  ?key:string -> ?on_change:(current -> 'e) -> props -> 'e Silica.Node.t
```

The theme component with no view, for an app that draws its own switcher: the app keeps the choice and gives it in the props.

## Silica_ui.Popup

Parts that every popup component shares: the reason of an open change, the refs and ids of the trigger and the popup, the ARIA attributes of the trigger, the `data-*` state attributes and the dismiss layer.

```ocaml
type reason =
  | Trigger_press  (** The trigger was pressed. *)
  | Trigger_hover  (** The pointer rested on the trigger. *)
  | Trigger_focus  (** The trigger got keyboard focus. *)
  | Outside_press  (** A press started outside the popup and the trigger. *)
  | Escape_key
  | Close_press  (** A close part, or content that closes the popup. *)
  | Item_press  (** An item of a list was chosen. *)
  | Focus_out  (** Focus left the popup. *)
  | List_navigation
```

```ocaml
val reason_name : reason -> string
```

`"trigger-press"`, `"escape-key"` and so on, as in Base UI.

```ocaml
type parts = {
  trigger : Silica.Ref.t;
  popup : Silica.Ref.t;
  trigger_id : string;
  popup_id : string;
}
```

```ocaml
val parts : string -> parts
```

The refs and element ids of the popup with this id: `id ^ "-trigger"` and `id ^ "-popup"`. The id must be unique in the page.

```ocaml
val open_attrs : bool -> 'e Silica.Node.attr list
```

`data-open` or `data-closed`.

```ocaml
val trigger_attrs :
  parts -> haspopup:string -> open_:bool -> 'e Silica.Node.attr list
```

The id and ref of the trigger, `aria-haspopup`, `aria-expanded`, `aria-controls` (while open) and `data-popup-open` (while open).

```ocaml
val dismiss :
  ?escape:'e -> ?outside:'e -> ?focus_out:'e -> parts -> 'e Silica.Source.t
```

The dismiss layer of the popup: the trigger and the popup are inside (see `Silica.Source.dismiss`).

```ocaml
val layer :
  ?escape:'e ->
  ?outside:'e ->
  ?focus_out:'e ->
  ?place:Silica.Place.t ->
  parts ->
  'e Silica.Source.t list
```

The sources of an open popup next to its trigger: `dismiss` and `Silica.Source.anchor` of the popup to the trigger.

```ocaml
val close : ?focus:bool -> parts -> 's -> ('s, 'e) Silica.Machine.next
```

`close parts closed` goes to the closed snapshot and, unless `focus` is `false`, moves focus to the trigger. A close by an outside press passes `~focus:false`: focus stays where the press put it.

```ocaml
val class_attr : string -> 'e Silica.Node.attr list
```

`class` when the string is not empty.

```ocaml
val open_change : ('p -> bool option) -> 'p option -> 'p -> bool option
```

`open_change open_ previous props` is the open state that a parent asks for with a controlled prop `open_ props`: `Some b` when it is `Some b` and the previous props (`None` at the start) did not ask for `b` already, `None` otherwise. Use it in the `on_props` of a popup machine (see `Silica.Machine.make_with_props`).

```ocaml
val report : (bool -> reason -> 'e) option -> bool -> reason -> 'e list
```

`report on_open_change open_ reason` is the output of an open change for the parent: `[f open_ reason]` when `on_open_change` is `Some f`.

## Silica_ui.Style

Default classes for the parts of each component, written with the theme tokens of `silica-ui.css` (`bg-ui-surface`, `text-ui-text`, ...). An app that imports `silica-ui.css` in its Tailwind input gets the CSS for them, because that file has an `@source` line for this folder.

```ocaml
val button : string
```

Classes for a plain button.

```ocaml
val dialog : Dialog.classes
```

Classes for `Dialog`.

```ocaml
val popover : Popover.classes
```

Classes for `Popover`.

```ocaml
val tooltip : Tooltip.classes
```

Classes for `Tooltip`.

```ocaml
val menu : Menu.classes
```

Classes for `Menu`.

```ocaml
val select : Select.classes
```

Classes for `Select`.

```ocaml
val combobox : Combobox.classes
```

Classes for `Combobox`.

```ocaml
val command_menu : Command_menu.classes
```

Classes for `Command_menu`.

```ocaml
val checkbox : Checkbox.classes
```

Classes for `Checkbox`.

```ocaml
val switch : Switch.classes
```

Classes for `Switch`.

```ocaml
val tabs : Tabs.classes
```

Classes for `Tabs`.

```ocaml
val tab_panel : string
```

Classes for a panel of `Tabs`.

```ocaml
val toast : Toast.classes
```

Classes for `Toast`.

## Silica_ui.Story

Stories: named examples of a component, for a component browser such as the one of the Silica site.

A story is a plain value: a name, the source text that a reader sees, the controls that change its props, and a function that renders it. Keep the source honest: embed the text of the file that defines `render` with a dune rule, so that the shown code is the compiled code.

```
  let disabled =
    {
      Story.name = "Disabled";
      source = Sources.switch_disabled;
      controls = [ Story.toggle "disabled" ~label:"Disabled" true ];
      render =
        (fun ~id values ->
          Switch.make
            (Switch.props ~id ~label:"Wi-Fi"
               ~disabled:(Story.bool values "disabled")
               ()
            )
        );
    }
```

The values of the controls are data: a parent machine keeps them (one `values` per story) and gives them to `render`. `controls` draws the controls with silica-ui `Switch` and `Select`.

```ocaml
type control =
  | Toggle of { name : string; label : string; default : bool }
  | Choice of {
      name : string;
      label : string;
      options : (string * string) list;  (** Pairs of a value and its label. *)
      default : string;
    }
```

```ocaml
val toggle : string -> label:string -> bool -> control
```

`toggle name ~label default`.

```ocaml
val choice :
  string -> label:string -> (string * string) list -> string -> control
```

`choice name ~label options default`. Raises `Invalid_argument` when `default` is not one of the values of `options`.

```ocaml
type values
```

The value of each control of a story. Comparable with `=`.

```ocaml
type t = {
  name : string;  (** Shown as the heading of the story. *)
  source : string;  (** The code that a reader sees. *)
  controls : control list;
  render : 'e. id:string -> values -> 'e Silica.Node.t;
      (** The story. [id] is unique in the page: pass it, or ids made from it,
          to the components, so that one story can show more than once. *)
}
```

```ocaml
val slug : t -> string
```

The name in lower case, with `-` for each run of other characters than ASCII letters and digits: an anchor for the story.

```ocaml
val defaults : t -> values
```

The default of each control.

```ocaml
val bool : values -> string -> bool
```

The value of a `Toggle`. Raises `Not_found` when no toggle has this name.

```ocaml
val string : values -> string -> string
```

The value of a `Choice`. Raises `Not_found` when no choice has this name.

```ocaml
val set_bool : values -> string -> bool -> values
```

```ocaml
val set_string : values -> string -> string -> values
```

```ocaml
val controls : id:string -> t -> values -> (values -> 'e) -> 'e Silica.Node.t
```

The controls of the story with these values, as a `<fieldset>` of a `Switch` for each toggle and a `Select` for each choice. Each change gives the new values to the function. Empty when the story has no controls.

```ocaml
val render : id:string -> t -> values -> 'e Silica.Node.t
```

`render ~id story values` is `story.render ~id values`.
