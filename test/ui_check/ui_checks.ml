open Silica
open Silica_ui
open Ui_samples

let rec exists predicate n =
  predicate (Node.view n)
  ||
  match Node.view n with
  | Node.Element (_, _, children) | Node.List children ->
      List.exists (exists predicate) children
  | Node.Keyed (_, child) ->
      exists predicate child
  | Node.Portal (_, children) ->
      List.exists (exists predicate) children
  | Node.Boundary { body; _ } ->
      exists predicate body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      false

let has_attr name value view =
  exists
    (function
      | Node.Element (_, attrs, _) ->
          List.mem (Node.String (name, value)) attrs
      | _ ->
          false
      )
    view

let print_report pp_event report =
  Format.asprintf "%a" (fun f r -> Check.pp_report ~pp_event f r) report

let dialog_props ?open_ ?dismissible ?alert () =
  Note_dialog.Props
    (Note_dialog.props ~id:"note" ~title:"Edit note" ~trigger:"Edit"
       ~close:"Close" ?open_ ?dismissible ?alert { Note.text = "hello" }
    )

let dialog_label : Note_dialog.event -> string = function
  | Note_dialog.Props { open_ = None; _ } ->
      "props"
  | Note_dialog.Props { open_ = Some true; _ } ->
      "props open"
  | Note_dialog.Props { open_ = Some false; _ } ->
      "props closed"
  | Note_dialog.Open_request reason ->
      "open " ^ Popup.reason_name reason
  | Note_dialog.Close_request reason ->
      "close " ^ Popup.reason_name reason
  | Note_dialog.Content (Note.Edit _) ->
      "edit"
  | Note_dialog.Content Note.Save ->
      "save"

let pp_dialog_event f event = Format.pp_print_string f (dialog_label event)

let dialog_open_has_popup :
    (Note_dialog.snapshot, Note_dialog.event) Check.invariant =
  {
    name = "open-has-popup";
    holds =
      (fun (Note_dialog.At (state, _)) view ->
        let shown =
          has_attr "role" "dialog" view || has_attr "role" "alertdialog" view
        in
        match state with
        | Note_dialog.State.Open ->
            shown
            && has_attr "aria-modal" "true" view
            && has_attr "aria-expanded" "true" view
        | Note_dialog.State.Closed | Note_dialog.State.Idle ->
            not shown
      );
  }

let dialog () =
  Check.component ~label:dialog_label
    ~props:
      [
        dialog_props ();
        dialog_props ~open_:true ();
        dialog_props ~open_:false ();
        dialog_props ~open_:true ~dismissible:false ();
        dialog_props ~open_:true ~alert:true ();
      ]
    ~inputs:(function
      | Note_dialog.At (Note_dialog.State.Open, _) ->
          [ Note_dialog.Content (Note.Edit "changed") ]
      | _ ->
          []
      )
    ~invariants:[ dialog_open_has_popup ] Note_dialog.component

module Filter_popover = Popover_samples.Filter_popover

let popover_props ?open_ ?place () =
  Filter_popover.Props
    (Filter_popover.props ~id:"filter" ~trigger:"Filter" ~title:"Filter issues"
       ~close:"Close" ?open_ ?place
       { Popover_samples.Filter.query = "bug" }
    )

let popover_label : Filter_popover.event -> string = function
  | Filter_popover.Props { open_ = None; _ } ->
      "props"
  | Filter_popover.Props { open_ = Some true; _ } ->
      "props open"
  | Filter_popover.Props { open_ = Some false; _ } ->
      "props closed"
  | Filter_popover.Open_request reason ->
      "open " ^ Popup.reason_name reason
  | Filter_popover.Close_request reason ->
      "close " ^ Popup.reason_name reason
  | Filter_popover.Content (Popover_samples.Filter.Query _) ->
      "query"
  | Filter_popover.Content Popover_samples.Filter.Apply ->
      "apply"

let pp_popover_event f event = Format.pp_print_string f (popover_label event)

let popover_open_has_popup :
    (Filter_popover.snapshot, Filter_popover.event) Check.invariant =
  {
    name = "open-has-popup";
    holds =
      (fun (Filter_popover.At (state, _)) view ->
        let shown = has_attr "role" "dialog" view in
        match state with
        | Filter_popover.State.Open ->
            shown
            && has_attr "aria-expanded" "true" view
            && not (has_attr "aria-modal" "true" view)
        | Filter_popover.State.Closed ->
            (not shown) && has_attr "aria-expanded" "false" view
        | Filter_popover.State.Idle ->
            not shown
      );
  }

let popover () =
  Check.component ~label:popover_label
    ~props:
      [
        popover_props ();
        popover_props ~open_:true ();
        popover_props ~open_:false ();
        popover_props ~place:{ Place.default with side = Place.Top } ();
      ]
    ~inputs:(function
      | Filter_popover.At (Filter_popover.State.Open, _) ->
          [ Filter_popover.Content (Popover_samples.Filter.Query "crash") ]
      | _ ->
          []
      )
    ~invariants:[ popover_open_has_popup ] Filter_popover.component

let tooltip_label : Tooltip.event -> string = function
  | Tooltip.Props { delay; close_delay; _ } ->
      Printf.sprintf "props %d %d" (Tooltip.delay_ms delay)
        (Tooltip.delay_ms close_delay)
  | Hover ->
      "hover"
  | Focus ->
      "focus"
  | Cancel ->
      "cancel"
  | Leave ->
      "leave"
  | Resume ->
      "resume"
  | Close reason ->
      "close " ^ Popup.reason_name reason
  | Elapsed delay ->
      Printf.sprintf "elapsed %d" (Tooltip.delay_ms delay)

let pp_tooltip_event f event = Format.pp_print_string f (tooltip_label event)

let tooltip_shown_describes : (Tooltip.snapshot, Tooltip.event) Check.invariant
    =
  {
    name = "shown-describes-trigger";
    holds =
      (fun (Tooltip.At (state, _)) view ->
        let shown =
          has_attr "role" "tooltip" view
          && has_attr "aria-describedby" "save-popup" view
        and hidden =
          (not (has_attr "role" "tooltip" view))
          && not (has_attr "aria-describedby" "save-popup" view)
        in
        match state with
        | Tooltip.State.Open | Tooltip.State.Closing ->
            shown
        | Tooltip.State.Idle | Tooltip.State.Closed | Tooltip.State.Opening ->
            hidden
      );
  }

let tooltip () =
  Check.component ~label:tooltip_label
    ~props:
      [
        Tooltip.Props (Tooltip_samples.props ());
        Tooltip.Props
          (Tooltip_samples.props ~delay:Tooltip.Instant
             ~close_delay:Tooltip.Short ()
          );
      ]
    ~invariants:[ tooltip_shown_describes ]
    Tooltip.component

let count predicate n =
  let rec go n =
    let here =
      if predicate (Node.view n) then
        1
      else
        0
    in
    here
    +
    match Node.view n with
    | Node.Element (_, _, children)
    | Node.List children
    | Node.Portal (_, children) ->
        List.fold_left (fun acc child -> acc + go child) 0 children
    | Node.Keyed (_, child) ->
        go child
    | Node.Boundary { body; _ } ->
        go body
    | Node.Text _ | Node.Component _ | Node.Chunk _ ->
        0
  in
  go n

let attr_count name view =
  count
    (function
      | Node.Element (_, attrs, _) ->
          List.exists
            (function Node.String (n, _) -> n = name | _ -> false)
            attrs
      | _ ->
          false
      )
    view

let list_keys =
  Key.
    [
      escape;
      enter;
      tab;
      arrow_up;
      arrow_down;
      make "Home";
      make "End";
      make " ";
      make "d";
    ]

let reason_label prefix reason = prefix ^ " " ^ Popup.reason_name reason

let nav_label : Menu.nav -> string = function
  | Menu.Next ->
      "next"
  | Menu.Previous ->
      "previous"
  | Menu.First ->
      "first"
  | Menu.Last ->
      "last"
  | Menu.Letter _ ->
      "letter"

let menu_label : Menu.event -> string = function
  | Menu.Props { open_ = None; _ } ->
      "props"
  | Menu.Props { open_ = Some true; _ } ->
      "props open"
  | Menu.Props { open_ = Some false; _ } ->
      "props closed"
  | Menu.Trigger_press ->
      "trigger press"
  | Menu.Open_key (reason, Menu.Start) ->
      reason_label "open first" reason
  | Menu.Open_key (reason, Menu.End) ->
      reason_label "open last" reason
  | Menu.Move nav ->
      "move " ^ nav_label nav
  | Menu.Highlight _ ->
      "highlight"
  | Menu.Choose _ ->
      "choose"
  | Menu.Close reason ->
      reason_label "close" reason

let pp_menu_event f event = Format.pp_print_string f (menu_label event)

let menu_open_has_popup : (Menu.snapshot, Menu.event) Check.invariant =
  {
    name = "open-has-menu";
    holds =
      (fun (Menu.At (state, data)) view ->
        let shown = has_attr "role" "menu" view in
        match state with
        | Menu.State.Open ->
            shown
            && has_attr "aria-expanded" "true" view
            && attr_count "data-highlighted" view
               = List.length (Option.to_list data.highlighted)
            && count
                 (function
                   | Node.Element (_, attrs, _) ->
                       List.mem (Node.String ("tabindex", "0")) attrs
                   | _ ->
                       false
                   )
                 view
               = List.length (Option.to_list data.highlighted)
        | Menu.State.Closed | Menu.State.Idle ->
            not shown
      );
  }

let menu_props ?open_ () = Menu.Props (Menu_samples.props ?open_ "actions")

let menu () =
  Check.component ~label:menu_label
    ~props:
      [ menu_props (); menu_props ~open_:true (); menu_props ~open_:false () ]
    ~keys:list_keys ~invariants:[ menu_open_has_popup ] Menu.component

let select_nav_label : Select.nav -> string = function
  | Select.Next ->
      "next"
  | Select.Previous ->
      "previous"
  | Select.First ->
      "first"
  | Select.Last ->
      "last"
  | Select.Letter _ ->
      "letter"

let select_label : Select.event -> string = function
  | Select.Props { value = None; _ } ->
      "props"
  | Select.Props { value = Some _; _ } ->
      "props value"
  | Select.Trigger_press ->
      "trigger press"
  | Select.Open_key reason ->
      reason_label "open" reason
  | Select.Move nav ->
      "move " ^ select_nav_label nav
  | Select.Highlight _ ->
      "highlight"
  | Select.Select (_, reason) ->
      reason_label "select" reason
  | Select.Close reason ->
      reason_label "close" reason

let pp_select_event f event = Format.pp_print_string f (select_label event)

let select_open_has_listbox : (Select.snapshot, Select.event) Check.invariant =
  {
    name = "open-has-listbox";
    holds =
      (fun (Select.At (state, data)) view ->
        let shown = has_attr "role" "listbox" view in
        let input_holds (sel : Select.selection) =
          has_attr "value" (Option.value sel.value ~default:"") view
        in
        match (state, data) with
        | Select.State.Open, (sel, highlighted) -> (
            shown
            && has_attr "aria-expanded" "true" view
            && input_holds sel
            &&
            match highlighted with
            | Some i ->
                has_attr "aria-activedescendant"
                  (Select.option_id sel.props.id i)
                  view
            | None ->
                attr_count "aria-activedescendant" view = 0
          )
        | Select.State.Closed, sel ->
            (not shown)
            && attr_count "aria-activedescendant" view = 0
            && input_holds sel
        | Select.State.Idle, () ->
            not shown
      );
  }

let select_props ?value () =
  Select.Props (Select_samples.props ?value ~name:"status" "status")

let select () =
  Check.component ~label:select_label
    ~props:
      [
        select_props ();
        select_props ~value:"done" ();
        select_props ~value:"todo" ();
      ]
    ~keys:list_keys
    ~invariants:[ select_open_has_listbox ]
    Select.component

let combobox_props ?(inline = false) items =
  Combobox.Props (Combobox.props ~id:"fruit" ~label:"Fruit" ~inline items)

let short_fruits = List.filteri (fun i _ -> i < 2) Combobox_samples.fruits

let combobox_label : Combobox.event -> string = function
  | Combobox.Props { inline = true; _ } ->
      "props inline"
  | Combobox.Props _ ->
      "props"
  | Combobox.Typed _ ->
      "type"
  | Combobox.Move `Next ->
      "next"
  | Combobox.Move `Previous ->
      "previous"
  | Combobox.Move `First ->
      "first"
  | Combobox.Move `Last ->
      "last"
  | Combobox.Show ->
      "show"
  | Combobox.Highlight _ ->
      "highlight"
  | Combobox.Choose_highlighted ->
      "enter"
  | Combobox.Choose _ ->
      "press"
  | Combobox.Escape ->
      "escape"
  | Combobox.Outside_press ->
      "outside"
  | Combobox.Focus_out ->
      "focus-out"

let pp_combobox_event f event = Format.pp_print_string f (combobox_label event)

let rec collect f n =
  f (Node.view n)
  @
  match Node.view n with
  | Node.Element (_, _, children)
  | Node.List children
  | Node.Portal (_, children) ->
      List.concat_map (collect f) children
  | Node.Keyed (_, child) ->
      collect f child
  | Node.Boundary { body; _ } ->
      collect f body
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      []

let attr_values name =
  collect (function
    | Node.Element (_, attrs, _) ->
        List.filter_map
          (function Node.String (k, v) when k = name -> Some v | _ -> None)
          attrs
    | _ ->
        []
    )

let combobox_list_shown : (Combobox.snapshot, Combobox.event) Check.invariant =
  {
    name = "list-shown";
    holds =
      (fun (Combobox.At (state, _)) view ->
        let shown = has_attr "role" "listbox" view in
        match state with
        | Combobox.State.Open | Combobox.State.Inline ->
            shown && attr_values "aria-expanded" view = [ "true" ]
        | Combobox.State.Closed ->
            (not shown) && attr_values "aria-expanded" view = [ "false" ]
        | Combobox.State.Idle ->
            not shown
      );
  }

let combobox_active_descendant :
    (Combobox.snapshot, Combobox.event) Check.invariant =
  {
    name = "active-descendant";
    holds =
      (fun _ view ->
        let highlighted =
          collect
            (function
              | Node.Element (_, attrs, _)
                when List.mem (Node.String ("data-highlighted", "")) attrs ->
                  List.filter_map
                    (function Node.String ("id", v) -> Some v | _ -> None)
                    attrs
              | _ ->
                  []
              )
            view
        in
        attr_values "aria-activedescendant" view = highlighted
      );
  }

let combobox_auto_highlight :
    (Combobox.snapshot, Combobox.event) Check.invariant =
  {
    name = "inline-auto-highlight";
    holds =
      (fun snapshot view ->
        match snapshot with
        | Combobox.At (Combobox.State.Inline, data) ->
            let enabled =
              List.exists
                (fun (item : Combobox.item) ->
                  (not item.disabled) && Combobox.matches data.query item
                )
                data.props.items
            in
            enabled = (attr_values "aria-activedescendant" view <> [])
        | _ ->
            true
      );
  }

let combobox () =
  Check.component ~label:combobox_label
    ~props:
      [
        combobox_props Combobox_samples.fruits;
        combobox_props ~inline:true Combobox_samples.fruits;
        combobox_props short_fruits;
      ]
    ~inputs:(function
      | Combobox.At (Combobox.State.Idle, ()) ->
          []
      | _ ->
          [ Combobox.Typed ""; Combobox.Typed "a"; Combobox.Typed "zz" ]
      )
    ~keys:
      Key.
        [
          escape;
          enter;
          tab;
          arrow_up;
          arrow_down;
          make ~alt:true "ArrowDown";
          make "Home";
          make "End";
        ]
    ~invariants:
      [
        combobox_list_shown; combobox_active_descendant; combobox_auto_highlight;
      ]
    Combobox.component

let command_menu_props ?open_ () =
  Command_menu.Menu.Props
    (Command_menu.menu_props
       (Command_menu.props ~id:"menu" ~title:"Command menu" ?open_
          Combobox_samples.commands
       )
    )

let command_menu_label : Command_menu.Menu.event -> string = function
  | Command_menu.Menu.Props { open_ = None; _ } ->
      "props"
  | Command_menu.Menu.Props { open_ = Some true; _ } ->
      "props open"
  | Command_menu.Menu.Props { open_ = Some false; _ } ->
      "props closed"
  | Command_menu.Menu.Open_request reason ->
      "open " ^ Popup.reason_name reason
  | Command_menu.Menu.Close_request reason ->
      "close " ^ Popup.reason_name reason
  | Command_menu.Menu.Content _ ->
      "run"

let pp_command_menu_event f event =
  Format.pp_print_string f (command_menu_label event)

let command_menu_open_has_search :
    (Command_menu.Menu.snapshot, Command_menu.Menu.event) Check.invariant =
  {
    name = "open-has-search";
    holds =
      (fun (Command_menu.Menu.At (state, _)) view ->
        let dialog = has_attr "role" "dialog" view
        and search =
          exists (function Node.Component _ -> true | _ -> false) view
        in
        match state with
        | Command_menu.Menu.State.Open ->
            dialog && search
        | Command_menu.Menu.State.Closed | Command_menu.Menu.State.Idle ->
            (not dialog) && not search
      );
  }

let command_menu () =
  Check.component ~label:command_menu_label
    ~props:
      [
        command_menu_props ();
        command_menu_props ~open_:true ();
        command_menu_props ~open_:false ();
      ]
    ~inputs:(function
      | Command_menu.Menu.At (Command_menu.Menu.State.Open, _) ->
          [ Command_menu.Menu.Content "settings" ]
      | _ ->
          []
      )
    ~invariants:[ command_menu_open_has_search ]
    Command_menu.Menu.component

let command_menu_search children =
  Check.component ~label:combobox_label
    ~props:(Check.props children Combobox.component)
    ~inputs:(function
      | Combobox.At (Combobox.State.Idle, ()) ->
          []
      | _ ->
          [ Combobox.Typed ""; Combobox.Typed "sign" ]
      )
    ~allow_unreached:
      [
        ("idle", "closed");
        ("closed", "open");
        ("closed", "inline");
        ("open", "closed");
        ("open", "inline");
        ("inline", "closed");
      ]
    ~invariants:[ combobox_active_descendant; combobox_auto_highlight ]
    Combobox.component

let checkbox_props ?checked ?indeterminate ?disabled () =
  Checkbox.Props
    (Checkbox.props ~id:"terms" ~label:"Accept" ?checked ?indeterminate
       ?disabled ~name:"terms" ()
    )

let checkbox_label : Checkbox.event -> string = function
  | Checkbox.Props { indeterminate = true; _ } ->
      "props mixed"
  | Checkbox.Props { checked; disabled; _ } ->
      Printf.sprintf "props %b%s" checked
        ( if disabled then
            " disabled"
          else
            ""
        )
  | Checkbox.Toggle ->
      "toggle"
  | Checkbox.Enter_key ->
      "enter"

let pp_checkbox_event f event = Format.pp_print_string f (checkbox_label event)

let checkbox_aria : (Checkbox.snapshot, Checkbox.event) Check.invariant =
  {
    name = "aria-checked-matches-state";
    holds =
      (fun (Checkbox.At (state, _)) view ->
        let shows value =
          has_attr "role" "checkbox" view && has_attr "aria-checked" value view
        in
        match state with
        | Checkbox.State.Idle ->
            not (has_attr "role" "checkbox" view)
        | Checkbox.State.Unchecked ->
            shows "false" && not (has_attr "type" "hidden" view)
        | Checkbox.State.Checked ->
            shows "true" && has_attr "value" "on" view
        | Checkbox.State.Indeterminate ->
            shows "mixed" && not (has_attr "type" "hidden" view)
      );
  }

let checkbox () =
  Check.component ~label:checkbox_label
    ~props:
      [
        checkbox_props ();
        checkbox_props ~checked:true ();
        checkbox_props ~indeterminate:true ();
        checkbox_props ~disabled:true ();
      ]
    ~invariants:[ checkbox_aria ] Checkbox.component

let switch_props ?checked ?disabled () =
  Switch.Props
    (Switch.props ~id:"wifi" ~label:"Wi-Fi" ?checked ?disabled ~name:"wifi" ())

let switch_label : Switch.event -> string = function
  | Switch.Props { checked; disabled; _ } ->
      Printf.sprintf "props %b%s" checked
        ( if disabled then
            " disabled"
          else
            ""
        )
  | Switch.Toggle ->
      "toggle"

let pp_switch_event f event = Format.pp_print_string f (switch_label event)

let switch_aria : (Switch.snapshot, Switch.event) Check.invariant =
  {
    name = "aria-checked-matches-state";
    holds =
      (fun (Switch.At (state, _)) view ->
        let shows value =
          has_attr "role" "switch" view && has_attr "aria-checked" value view
        in
        match state with
        | Switch.State.Idle ->
            not (has_attr "role" "switch" view)
        | Switch.State.Unchecked ->
            shows "false" && not (has_attr "type" "hidden" view)
        | Switch.State.Checked ->
            shows "true" && has_attr "value" "on" view
      );
  }

let switch () =
  Check.component ~label:switch_label
    ~props:
      [
        switch_props ();
        switch_props ~checked:true ();
        switch_props ~disabled:true ();
      ]
    ~invariants:[ switch_aria ] Switch.component

let tabs_props ?orientation value =
  Tabs.Props
    (Tabs.props ~id:"set" ?orientation ~value
       [
         Tabs.tab ~value:"a" "Account";
         Tabs.tab ~value:"b" ~disabled:true "Billing";
         Tabs.tab ~value:"c" "Closed";
       ]
    )

let tabs_label : Tabs.event -> string = function
  | Tabs.Props { value; orientation = Tabs.Horizontal; _ } ->
      "props " ^ value
  | Tabs.Props { value; orientation = Tabs.Vertical; _ } ->
      "props vertical " ^ value
  | Tabs.Select value ->
      "select " ^ value
  | Tabs.Move Tabs.Next ->
      "next"
  | Tabs.Move Tabs.Previous ->
      "previous"
  | Tabs.Move Tabs.First ->
      "first"
  | Tabs.Move Tabs.Last ->
      "last"

let pp_tabs_event f event = Format.pp_print_string f (tabs_label event)

let element_has id pairs view =
  exists
    (function
      | Node.Element (_, attrs, _) ->
          List.mem (Node.String ("id", id)) attrs
          && List.for_all
               (fun (name, value) -> List.mem (Node.String (name, value)) attrs)
               pairs
      | _ ->
          false
      )
    view

let tabs_roving : (Tabs.snapshot, Tabs.event) Check.invariant =
  {
    name = "one-tab-stop-on-the-focused-tab";
    holds =
      (fun (Tabs.At (state, model)) view ->
        match state with
        | Tabs.State.Idle ->
            not (has_attr "role" "tablist" view)
        | Tabs.State.Ready ->
            let stops =
              List.filter
                (fun (tab : Tabs.tab) ->
                  element_has ("set-tab-" ^ tab.value)
                    [ ("tabindex", "0") ]
                    view
                )
                model.props.tabs
            in
            List.map (fun (tab : Tabs.tab) -> tab.value) stops
            = [ model.focused ]
            && element_has
                 ("set-tab-" ^ model.selected)
                 [ ("aria-selected", "true") ]
                 view
      );
  }

let tabs () =
  Check.component ~label:tabs_label
    ~props:
      [
        tabs_props "a";
        tabs_props "c";
        tabs_props ~orientation:Tabs.Vertical "a";
      ]
    ~keys:
      (List.map Key.make
         [ "ArrowRight"; "ArrowLeft"; "ArrowDown"; "ArrowUp"; "Home"; "End" ]
      )
    ~invariants:[ tabs_roving ] Tabs.component

let toast_props toasts =
  Toast.Props (Toast.props ~id:"note" ~timeout_ms:200 toasts)

let first = Toast.toast ~key:"a" ~description:"Saved." "Saved"
let urgent = Toast.toast ~key:"b" ~priority:Toast.High "Failed"

let toast_label : Toast.event -> string = function
  | Toast.Props { toasts; _ } ->
      "props "
      ^ String.concat "," (List.map (fun (t : Toast.toast) -> t.key) toasts)
  | Toast.Close key ->
      "close " ^ key
  | Toast.Tick ->
      "tick"
  | Toast.Hover hovered ->
      Printf.sprintf "hover %b" hovered
  | Toast.Focus focused ->
      Printf.sprintf "focus %b" focused

let pp_toast_event f event = Format.pp_print_string f (toast_label event)

let toast_shows_list : (Toast.snapshot, Toast.event) Check.invariant =
  {
    name = "region-shows-each-toast";
    holds =
      (fun snapshot view ->
        let shows (model : Toast.model) ~paused =
          element_has "note"
            [ ("role", "region"); ("aria-live", "polite") ]
            view
          && element_has "note" [ ("data-paused", "") ] view = paused
          && List.for_all
               (fun (toast : Toast.toast) ->
                 element_has
                   ("note-toast-" ^ toast.key)
                   [
                     ( "role",
                       match toast.priority with
                       | Toast.High ->
                           "alertdialog"
                       | Toast.Low ->
                           "dialog"
                     );
                     ("aria-modal", "false");
                   ]
                   view
               )
               model.props.toasts
        in
        match snapshot with
        | Toast.At (Toast.State.Idle, ()) ->
            not (has_attr "role" "region" view)
        | Toast.At (Toast.State.Running, (model, _)) ->
            model.props.toasts <> [] && shows model ~paused:false
        | Toast.At (Toast.State.Paused, model) ->
            model.props.toasts <> [] && shows model ~paused:true
        | Toast.At (Toast.State.Empty, model) ->
            model.props.toasts = [] && shows model ~paused:false
        | Toast.At (Toast.State.Expired, model) ->
            shows model ~paused:false
      );
  }

let toast () =
  Check.component ~label:toast_label
    ~props:
      [ toast_props []; toast_props [ first ]; toast_props [ first; urgent ] ]
    ~invariants:[ toast_shows_list ] Toast.component

let theme_set = Theme.set ~light:Theme.light ~dark:Theme.dark Theme.shipped

let theme_label : Theme.event -> string = function
  | Theme.Props { choice; _ } ->
      "props " ^ Theme.choice_name choice
  | Theme.Choose name ->
      "choose " ^ name
  | Theme.System_dark dark ->
      Printf.sprintf "system dark %b" dark

let pp_theme_event f event = Format.pp_print_string f (theme_label event)

let theme_current : (Theme.snapshot, Theme.event) Check.invariant =
  {
    name = "current-theme-in-set-and-scheme";
    holds =
      (fun snapshot _ ->
        match (snapshot, Theme.current snapshot) with
        | Theme.At (Theme.State.Idle, ()), None ->
            true
        | Theme.At (Theme.State.Fixed, { theme; _ }), Some current ->
            current.theme == theme && List.memq theme (Theme.themes theme_set)
        | Theme.At (Theme.State.System, { dark = None; _ }), None ->
            true
        | Theme.At (Theme.State.System, { dark = Some dark; _ }), Some current
          ->
            (Theme.scheme current.theme
            =
            if dark then
              Theme.Dark
            else
              Theme.Light
            )
            && List.memq current.theme (Theme.themes theme_set)
        | _ ->
            false
      );
  }

let theme () =
  Check.component ~label:theme_label
    ~props:
      [
        Theme.Props (Theme.props ~id:"theme" theme_set Theme.System);
        Theme.Props (Theme.props ~id:"theme" theme_set (Theme.Fixed Theme.ocean));
      ]
    ~inputs:(fun _ ->
      List.map
        (fun name -> Theme.Choose name)
        [ "system"; "light"; "high-contrast"; "sepia" ]
    )
    ~invariants:[ theme_current ] Theme.provider_component
