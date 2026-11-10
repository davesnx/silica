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

module D = Note_dialog

let dialog_props ?open_ ?dismissible ?alert () =
  D.Props
    (D.props ~id:"note" ~title:"Edit note" ~trigger:"Edit" ~close:"Close" ?open_
       ?dismissible ?alert { Note.text = "hello" }
    )

let dialog_label : D.event -> string = function
  | D.Props { open_ = None; _ } ->
      "props"
  | D.Props { open_ = Some true; _ } ->
      "props open"
  | D.Props { open_ = Some false; _ } ->
      "props closed"
  | D.Open_request reason ->
      "open " ^ Popup.reason_name reason
  | D.Close_request reason ->
      "close " ^ Popup.reason_name reason
  | D.Content (Note.Edit _) ->
      "edit"
  | D.Content Note.Save ->
      "save"

let pp_dialog_event f event = Format.pp_print_string f (dialog_label event)

let dialog_open_has_popup : (D.snapshot, D.event) Check.invariant =
  {
    name = "open-has-popup";
    holds =
      (fun (D.At (state, _)) view ->
        let shown =
          has_attr "role" "dialog" view || has_attr "role" "alertdialog" view
        in
        match state with
        | D.State.Open ->
            shown
            && has_attr "aria-modal" "true" view
            && has_attr "aria-expanded" "true" view
        | D.State.Closed | D.State.Idle ->
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
      | D.At (D.State.Open, _) -> [ D.Content (Note.Edit "changed") ] | _ -> []
      )
    ~invariants:[ dialog_open_has_popup ] D.component

module P = Popover_samples.Filter_popover

let popover_props ?open_ ?place () =
  P.Props
    (P.props ~id:"filter" ~trigger:"Filter" ~title:"Filter issues"
       ~close:"Close" ?open_ ?place
       { Popover_samples.Filter.query = "bug" }
    )

let popover_label : P.event -> string = function
  | P.Props { open_ = None; _ } ->
      "props"
  | P.Props { open_ = Some true; _ } ->
      "props open"
  | P.Props { open_ = Some false; _ } ->
      "props closed"
  | P.Open_request reason ->
      "open " ^ Popup.reason_name reason
  | P.Close_request reason ->
      "close " ^ Popup.reason_name reason
  | P.Content (Popover_samples.Filter.Query _) ->
      "query"
  | P.Content Popover_samples.Filter.Apply ->
      "apply"

let pp_popover_event f event = Format.pp_print_string f (popover_label event)

let popover_open_has_popup : (P.snapshot, P.event) Check.invariant =
  {
    name = "open-has-popup";
    holds =
      (fun (P.At (state, _)) view ->
        let shown = has_attr "role" "dialog" view in
        match state with
        | P.State.Open ->
            shown
            && has_attr "aria-expanded" "true" view
            && not (has_attr "aria-modal" "true" view)
        | P.State.Closed ->
            (not shown) && has_attr "aria-expanded" "false" view
        | P.State.Idle ->
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
      | P.At (P.State.Open, _) ->
          [ P.Content (Popover_samples.Filter.Query "crash") ]
      | _ ->
          []
      )
    ~invariants:[ popover_open_has_popup ] P.component

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

module Mn = Menu

let reason_label prefix reason = prefix ^ " " ^ Popup.reason_name reason

let nav_label : Mn.nav -> string = function
  | Mn.Next ->
      "next"
  | Mn.Previous ->
      "previous"
  | Mn.First ->
      "first"
  | Mn.Last ->
      "last"
  | Mn.Letter _ ->
      "letter"

let menu_label : Mn.event -> string = function
  | Mn.Props { open_ = None; _ } ->
      "props"
  | Mn.Props { open_ = Some true; _ } ->
      "props open"
  | Mn.Props { open_ = Some false; _ } ->
      "props closed"
  | Mn.Trigger_press ->
      "trigger press"
  | Mn.Open_key (reason, Mn.Start) ->
      reason_label "open first" reason
  | Mn.Open_key (reason, Mn.End) ->
      reason_label "open last" reason
  | Mn.Move nav ->
      "move " ^ nav_label nav
  | Mn.Highlight _ ->
      "highlight"
  | Mn.Choose _ ->
      "choose"
  | Mn.Close reason ->
      reason_label "close" reason

let pp_menu_event f event = Format.pp_print_string f (menu_label event)

let menu_open_has_popup : (Mn.snapshot, Mn.event) Check.invariant =
  {
    name = "open-has-menu";
    holds =
      (fun (Mn.At (state, data)) view ->
        let shown = has_attr "role" "menu" view in
        match state with
        | Mn.State.Open ->
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
        | Mn.State.Closed | Mn.State.Idle ->
            not shown
      );
  }

let menu_props ?open_ () = Mn.Props (Menu_samples.props ?open_ "actions")

let menu () =
  Check.component ~label:menu_label
    ~props:
      [ menu_props (); menu_props ~open_:true (); menu_props ~open_:false () ]
    ~keys:list_keys ~invariants:[ menu_open_has_popup ] Mn.component

module Sl = Select

let select_nav_label : Sl.nav -> string = function
  | Sl.Next ->
      "next"
  | Sl.Previous ->
      "previous"
  | Sl.First ->
      "first"
  | Sl.Last ->
      "last"
  | Sl.Letter _ ->
      "letter"

let select_label : Sl.event -> string = function
  | Sl.Props { value = None; _ } ->
      "props"
  | Sl.Props { value = Some _; _ } ->
      "props value"
  | Sl.Trigger_press ->
      "trigger press"
  | Sl.Open_key reason ->
      reason_label "open" reason
  | Sl.Move nav ->
      "move " ^ select_nav_label nav
  | Sl.Highlight _ ->
      "highlight"
  | Sl.Select (_, reason) ->
      reason_label "select" reason
  | Sl.Close reason ->
      reason_label "close" reason

let pp_select_event f event = Format.pp_print_string f (select_label event)

let select_open_has_listbox : (Sl.snapshot, Sl.event) Check.invariant =
  {
    name = "open-has-listbox";
    holds =
      (fun (Sl.At (state, data)) view ->
        let shown = has_attr "role" "listbox" view in
        let input_holds (sel : Sl.selection) =
          has_attr "value" (Option.value sel.value ~default:"") view
        in
        match (state, data) with
        | Sl.State.Open, (sel, highlighted) -> (
            shown
            && has_attr "aria-expanded" "true" view
            && input_holds sel
            &&
            match highlighted with
            | Some i ->
                has_attr "aria-activedescendant"
                  (Sl.option_id sel.props.id i)
                  view
            | None ->
                attr_count "aria-activedescendant" view = 0
          )
        | Sl.State.Closed, sel ->
            (not shown)
            && attr_count "aria-activedescendant" view = 0
            && input_holds sel
        | Sl.State.Idle, () ->
            not shown
      );
  }

let select_props ?value () =
  Sl.Props (Select_samples.props ?value ~name:"status" "status")

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
    Sl.component

module Cb = Combobox

let combobox_props ?(inline = false) items =
  Cb.Props (Cb.props ~id:"fruit" ~label:"Fruit" ~inline items)

let short_fruits = List.filteri (fun i _ -> i < 2) Combobox_samples.fruits

let combobox_label : Cb.event -> string = function
  | Cb.Props { inline = true; _ } ->
      "props inline"
  | Cb.Props _ ->
      "props"
  | Cb.Typed _ ->
      "type"
  | Cb.Move `Next ->
      "next"
  | Cb.Move `Previous ->
      "previous"
  | Cb.Move `First ->
      "first"
  | Cb.Move `Last ->
      "last"
  | Cb.Show ->
      "show"
  | Cb.Highlight _ ->
      "highlight"
  | Cb.Choose_highlighted ->
      "enter"
  | Cb.Choose _ ->
      "press"
  | Cb.Escape ->
      "escape"
  | Cb.Outside_press ->
      "outside"
  | Cb.Focus_out ->
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

let combobox_list_shown : (Cb.snapshot, Cb.event) Check.invariant =
  {
    name = "list-shown";
    holds =
      (fun (Cb.At (state, _)) view ->
        let shown = has_attr "role" "listbox" view in
        match state with
        | Cb.State.Open | Cb.State.Inline ->
            shown && attr_values "aria-expanded" view = [ "true" ]
        | Cb.State.Closed ->
            (not shown) && attr_values "aria-expanded" view = [ "false" ]
        | Cb.State.Idle ->
            not shown
      );
  }

let combobox_active_descendant : (Cb.snapshot, Cb.event) Check.invariant =
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

let combobox_auto_highlight : (Cb.snapshot, Cb.event) Check.invariant =
  {
    name = "inline-auto-highlight";
    holds =
      (fun snapshot view ->
        match snapshot with
        | Cb.At (Cb.State.Inline, data) ->
            let enabled =
              List.exists
                (fun (item : Cb.item) ->
                  (not item.disabled) && Cb.matches data.query item
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
      | Cb.At (Cb.State.Idle, ()) ->
          []
      | _ ->
          [ Cb.Typed ""; Cb.Typed "a"; Cb.Typed "zz" ]
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
    Cb.component

module Cm = Command_menu.Menu

let command_menu_props ?open_ () =
  Cm.Props
    (Command_menu.menu_props
       (Command_menu.props ~id:"menu" ~title:"Command menu" ?open_
          Combobox_samples.commands
       )
    )

let command_menu_label : Cm.event -> string = function
  | Cm.Props { open_ = None; _ } ->
      "props"
  | Cm.Props { open_ = Some true; _ } ->
      "props open"
  | Cm.Props { open_ = Some false; _ } ->
      "props closed"
  | Cm.Open_request reason ->
      "open " ^ Popup.reason_name reason
  | Cm.Close_request reason ->
      "close " ^ Popup.reason_name reason
  | Cm.Content _ ->
      "run"

let pp_command_menu_event f event =
  Format.pp_print_string f (command_menu_label event)

let command_menu_open_has_search : (Cm.snapshot, Cm.event) Check.invariant =
  {
    name = "open-has-search";
    holds =
      (fun (Cm.At (state, _)) view ->
        let dialog = has_attr "role" "dialog" view
        and search =
          exists (function Node.Component _ -> true | _ -> false) view
        in
        match state with
        | Cm.State.Open ->
            dialog && search
        | Cm.State.Closed | Cm.State.Idle ->
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
      | Cm.At (Cm.State.Open, _) -> [ Cm.Content "settings" ] | _ -> []
      )
    ~invariants:[ command_menu_open_has_search ]
    Cm.component

let command_menu_search children =
  Check.component ~label:combobox_label
    ~props:(Check.props children Cb.component)
    ~inputs:(function
      | Cb.At (Cb.State.Idle, ()) -> [] | _ -> [ Cb.Typed ""; Cb.Typed "sign" ]
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
    Cb.component

module C = Checkbox

let checkbox_props ?checked ?indeterminate ?disabled () =
  C.Props
    (C.props ~id:"terms" ~label:"Accept" ?checked ?indeterminate ?disabled
       ~name:"terms" ()
    )

let checkbox_label : C.event -> string = function
  | C.Props { indeterminate = true; _ } ->
      "props mixed"
  | C.Props { checked; disabled; _ } ->
      Printf.sprintf "props %b%s" checked
        ( if disabled then
            " disabled"
          else
            ""
        )
  | C.Toggle ->
      "toggle"
  | C.Enter_key ->
      "enter"

let pp_checkbox_event f event = Format.pp_print_string f (checkbox_label event)

let checkbox_aria : (C.snapshot, C.event) Check.invariant =
  {
    name = "aria-checked-matches-state";
    holds =
      (fun (C.At (state, _)) view ->
        let shows value =
          has_attr "role" "checkbox" view && has_attr "aria-checked" value view
        in
        match state with
        | C.State.Idle ->
            not (has_attr "role" "checkbox" view)
        | C.State.Unchecked ->
            shows "false" && not (has_attr "type" "hidden" view)
        | C.State.Checked ->
            shows "true" && has_attr "value" "on" view
        | C.State.Indeterminate ->
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
    ~invariants:[ checkbox_aria ] C.component

module Sw = Switch

let switch_props ?checked ?disabled () =
  Sw.Props
    (Sw.props ~id:"wifi" ~label:"Wi-Fi" ?checked ?disabled ~name:"wifi" ())

let switch_label : Sw.event -> string = function
  | Sw.Props { checked; disabled; _ } ->
      Printf.sprintf "props %b%s" checked
        ( if disabled then
            " disabled"
          else
            ""
        )
  | Sw.Toggle ->
      "toggle"

let pp_switch_event f event = Format.pp_print_string f (switch_label event)

let switch_aria : (Sw.snapshot, Sw.event) Check.invariant =
  {
    name = "aria-checked-matches-state";
    holds =
      (fun (Sw.At (state, _)) view ->
        let shows value =
          has_attr "role" "switch" view && has_attr "aria-checked" value view
        in
        match state with
        | Sw.State.Idle ->
            not (has_attr "role" "switch" view)
        | Sw.State.Unchecked ->
            shows "false" && not (has_attr "type" "hidden" view)
        | Sw.State.Checked ->
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
    ~invariants:[ switch_aria ] Sw.component

module T = Tabs

let tabs_props ?orientation value =
  T.Props
    (T.props ~id:"set" ?orientation ~value
       [
         T.tab ~value:"a" "Account";
         T.tab ~value:"b" ~disabled:true "Billing";
         T.tab ~value:"c" "Closed";
       ]
    )

let tabs_label : T.event -> string = function
  | T.Props { value; orientation = T.Horizontal; _ } ->
      "props " ^ value
  | T.Props { value; orientation = T.Vertical; _ } ->
      "props vertical " ^ value
  | T.Select value ->
      "select " ^ value
  | T.Move T.Next ->
      "next"
  | T.Move T.Previous ->
      "previous"
  | T.Move T.First ->
      "first"
  | T.Move T.Last ->
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

let tabs_roving : (T.snapshot, T.event) Check.invariant =
  {
    name = "one-tab-stop-on-the-focused-tab";
    holds =
      (fun (T.At (state, model)) view ->
        match state with
        | T.State.Idle ->
            not (has_attr "role" "tablist" view)
        | T.State.Ready ->
            let stops =
              List.filter
                (fun (tab : T.tab) ->
                  element_has ("set-tab-" ^ tab.value)
                    [ ("tabindex", "0") ]
                    view
                )
                model.props.tabs
            in
            List.map (fun (tab : T.tab) -> tab.value) stops = [ model.focused ]
            && element_has
                 ("set-tab-" ^ model.selected)
                 [ ("aria-selected", "true") ]
                 view
      );
  }

let tabs () =
  Check.component ~label:tabs_label
    ~props:
      [ tabs_props "a"; tabs_props "c"; tabs_props ~orientation:T.Vertical "a" ]
    ~keys:
      (List.map Key.make
         [ "ArrowRight"; "ArrowLeft"; "ArrowDown"; "ArrowUp"; "Home"; "End" ]
      )
    ~invariants:[ tabs_roving ] T.component

module To = Toast

let toast_props toasts = To.Props (To.props ~id:"note" ~timeout_ms:200 toasts)

let first = To.toast ~key:"a" ~description:"Saved." "Saved"
let urgent = To.toast ~key:"b" ~priority:To.High "Failed"

let toast_label : To.event -> string = function
  | To.Props { toasts; _ } ->
      "props "
      ^ String.concat "," (List.map (fun (t : To.toast) -> t.key) toasts)
  | To.Close key ->
      "close " ^ key
  | To.Tick ->
      "tick"
  | To.Hover hovered ->
      Printf.sprintf "hover %b" hovered
  | To.Focus focused ->
      Printf.sprintf "focus %b" focused

let pp_toast_event f event = Format.pp_print_string f (toast_label event)

let toast_shows_list : (To.snapshot, To.event) Check.invariant =
  {
    name = "region-shows-each-toast";
    holds =
      (fun snapshot view ->
        let shows (model : To.model) ~paused =
          element_has "note"
            [ ("role", "region"); ("aria-live", "polite") ]
            view
          && element_has "note" [ ("data-paused", "") ] view = paused
          && List.for_all
               (fun (toast : To.toast) ->
                 element_has
                   ("note-toast-" ^ toast.key)
                   [
                     ( "role",
                       match toast.priority with
                       | To.High ->
                           "alertdialog"
                       | To.Low ->
                           "dialog"
                     );
                     ("aria-modal", "false");
                   ]
                   view
               )
               model.props.toasts
        in
        match snapshot with
        | To.At (To.State.Idle, ()) ->
            not (has_attr "role" "region" view)
        | To.At (To.State.Running, (model, _)) ->
            model.props.toasts <> [] && shows model ~paused:false
        | To.At (To.State.Paused, model) ->
            model.props.toasts <> [] && shows model ~paused:true
        | To.At (To.State.Empty, model) ->
            model.props.toasts = [] && shows model ~paused:false
        | To.At (To.State.Expired, model) ->
            shows model ~paused:false
      );
  }

let toast () =
  Check.component ~label:toast_label
    ~props:
      [ toast_props []; toast_props [ first ]; toast_props [ first; urgent ] ]
    ~invariants:[ toast_shows_list ] To.component

module Th = Theme

let theme_set = Th.set ~light:Th.light ~dark:Th.dark Th.shipped

let theme_label : Th.event -> string = function
  | Th.Props { choice; _ } ->
      "props " ^ Th.choice_name choice
  | Th.Choose name ->
      "choose " ^ name
  | Th.System_dark dark ->
      Printf.sprintf "system dark %b" dark

let pp_theme_event f event = Format.pp_print_string f (theme_label event)

let theme_current : (Th.snapshot, Th.event) Check.invariant =
  {
    name = "current-theme-in-set-and-scheme";
    holds =
      (fun snapshot _ ->
        match (snapshot, Th.current snapshot) with
        | Th.At (Th.State.Idle, ()), None ->
            true
        | Th.At (Th.State.Fixed, { theme; _ }), Some current ->
            current.theme == theme && List.memq theme (Th.themes theme_set)
        | Th.At (Th.State.System, { dark = None; _ }), None ->
            true
        | Th.At (Th.State.System, { dark = Some dark; _ }), Some current ->
            (Th.scheme current.theme
            =
            if dark then
              Th.Dark
            else
              Th.Light
            )
            && List.memq current.theme (Th.themes theme_set)
        | _ ->
            false
      );
  }

let theme () =
  Check.component ~label:theme_label
    ~props:
      [
        Th.Props (Th.props ~id:"theme" theme_set Th.System);
        Th.Props (Th.props ~id:"theme" theme_set (Th.Fixed Th.ocean));
      ]
    ~inputs:(fun _ ->
      List.map
        (fun name -> Th.Choose name)
        [ "system"; "light"; "high-contrast"; "sepia" ]
    )
    ~invariants:[ theme_current ] Th.provider_component
