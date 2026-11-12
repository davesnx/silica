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

let dialog_props ?open_ ?dismissible ?alert () =
  Dialog.props ~id:"note" ~title:"Edit note" ~trigger:"Edit" ~close:"Close"
    ?open_ ?dismissible ?alert { text = "hello" }

let dialog_open_has_popup :
    (Dialog.state, note_event Dialog.event) Check.property =
  Check.always ~name:"open-has-popup"
    (Check.holds (fun state view ->
         let shown =
           has_attr "role" "dialog" view || has_attr "role" "alertdialog" view
         in
         match state with
         | Dialog.Open ->
             shown
             && has_attr "aria-modal" "true" view
             && has_attr "aria-expanded" "true" view
         | Dialog.Closed ->
             not shown
     )
    )

let dialog () =
  Check.component
    ~props:
      [
        dialog_props ();
        dialog_props ~open_:true ();
        dialog_props ~open_:false ();
        dialog_props ~open_:true ~dismissible:false ();
        dialog_props ~open_:true ~alert:true ();
      ]
    ~inputs:(function
      | Dialog.Open -> [ Dialog.Content (Edit "changed") ] | Dialog.Closed -> []
      )
    ~properties:[ dialog_open_has_popup ] note_dialog.component

let filter_popover = Popover_samples.filter_popover

let popover_props ?open_ ?place () =
  Popover.props ~id:"filter" ~trigger:"Filter" ~title:"Filter issues"
    ~close:"Close" ?open_ ?place
    { Popover_samples.query = "bug" }

let popover_open_has_popup :
    (Popover.state, Popover_samples.filter_event Popover.event) Check.property =
  Check.always ~name:"open-has-popup"
    (Check.holds (fun state view ->
         let shown = has_attr "role" "dialog" view in
         match state with
         | Popover.Open ->
             shown
             && has_attr "aria-expanded" "true" view
             && not (has_attr "aria-modal" "true" view)
         | Popover.Closed ->
             (not shown) && has_attr "aria-expanded" "false" view
     )
    )

let popover () =
  Check.component
    ~props:
      [
        popover_props ();
        popover_props ~open_:true ();
        popover_props ~open_:false ();
        popover_props ~place:{ Place.default with side = Place.Top } ();
      ]
    ~inputs:(function
      | Popover.Open ->
          [ Popover.Content (Popover_samples.Query "crash") ]
      | Popover.Closed ->
          []
      )
    ~properties:[ popover_open_has_popup ] filter_popover.component

let tooltip_shown_describes : (Tooltip.state, Tooltip.event) Check.property =
  Check.always ~name:"shown-describes-trigger"
    (Check.holds (fun state view ->
         let shown =
           has_attr "role" "tooltip" view
           && has_attr "aria-describedby" "save-popup" view
         and hidden =
           (not (has_attr "role" "tooltip" view))
           && not (has_attr "aria-describedby" "save-popup" view)
         in
         match state with
         | Tooltip.Open | Tooltip.Closing ->
             shown
         | Tooltip.Closed | Tooltip.Opening ->
             hidden
     )
    )

let tooltip () =
  Check.component
    ~props:
      [
        Tooltip_samples.props ();
        Tooltip_samples.props ~delay:Tooltip.Instant ~close_delay:Tooltip.Short
          ();
      ]
    ~properties:[ tooltip_shown_describes ]
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

let menu_open_has_popup : (Menu.state, Menu.event) Check.property =
  Check.always ~name:"open-has-menu"
    (Check.holds (fun state view ->
         let shown = has_attr "role" "menu" view in
         match state with
         | Menu.Open highlighted ->
             shown
             && has_attr "aria-expanded" "true" view
             && attr_count "data-highlighted" view
                = List.length (Option.to_list highlighted)
             && count
                  (function
                    | Node.Element (_, attrs, _) ->
                        List.mem (Node.String ("tabindex", "0")) attrs
                    | _ ->
                        false
                    )
                  view
                = List.length (Option.to_list highlighted)
         | Menu.Closed ->
             not shown
     )
    )

let menu_props ?open_ () = Menu_samples.props ?open_ "actions"

let menu () =
  Check.component
    ~props:
      [ menu_props (); menu_props ~open_:true (); menu_props ~open_:false () ]
    ~keys:list_keys ~properties:[ menu_open_has_popup ] Menu.component

let select_open_has_listbox : (Select.state, Select.event) Check.property =
  Check.always ~name:"open-has-listbox"
    (Check.holds (fun state view ->
         let shown = has_attr "role" "listbox" view in
         let input_holds value =
           has_attr "value" (Option.value value ~default:"") view
         in
         match state with
         | Select.Open (value, highlighted) -> (
             shown
             && has_attr "aria-expanded" "true" view
             && input_holds value
             &&
             match highlighted with
             | Some i ->
                 has_attr "aria-activedescendant"
                   (Select.option_id "status" i)
                   view
             | None ->
                 attr_count "aria-activedescendant" view = 0
           )
         | Select.Closed value ->
             (not shown)
             && attr_count "aria-activedescendant" view = 0
             && input_holds value
     )
    )

let select_props ?value () = Select_samples.props ?value ~name:"status" "status"

let select () =
  Check.component
    ~props:
      [
        select_props ();
        select_props ~value:"done" ();
        select_props ~value:"todo" ();
      ]
    ~keys:list_keys
    ~properties:[ select_open_has_listbox ]
    Select.component

let combobox_props ?(inline = false) items =
  Combobox.props ~id:"fruit" ~label:"Fruit" ~inline items

let short_fruits = List.filteri (fun i _ -> i < 2) Combobox_samples.fruits

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

let combobox_list_shown : (Combobox.state, Combobox.event) Check.property =
  Check.always ~name:"list-shown"
    (Check.holds (fun state view ->
         let shown = has_attr "role" "listbox" view in
         match state with
         | Combobox.Open _ | Combobox.Inline _ ->
             shown && attr_values "aria-expanded" view = [ "true" ]
         | Combobox.Closed _ ->
             (not shown) && attr_values "aria-expanded" view = [ "false" ]
     )
    )

let combobox_active_descendant : (Combobox.state, Combobox.event) Check.property
    =
  Check.always ~name:"active-descendant"
    (Check.holds (fun _ view ->
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
     )
    )

let combobox_auto_highlight : (Combobox.state, Combobox.event) Check.property =
  Check.always ~name:"inline-auto-highlight"
    (Check.holds (fun state view ->
         match state with
         | Combobox.Inline _ ->
             let enabled =
               count
                 (function
                   | Node.Element (_, attrs, _) ->
                       List.mem (Node.String ("role", "option")) attrs
                       && not
                            (List.mem
                               (Node.String ("aria-disabled", "true"))
                               attrs
                            )
                   | _ ->
                       false
                   )
                 view
               > 0
             in
             enabled = (attr_values "aria-activedescendant" view <> [])
         | Combobox.Closed _ | Combobox.Open _ ->
             true
     )
    )

let combobox () =
  Check.component
    ~props:
      [
        combobox_props Combobox_samples.fruits;
        combobox_props ~inline:true Combobox_samples.fruits;
        combobox_props short_fruits;
      ]
    ~inputs:(fun _ ->
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
    ~properties:
      [
        combobox_list_shown; combobox_active_descendant; combobox_auto_highlight;
      ]
    Combobox.component

let command_menu_props ?open_ () =
  Command_menu.menu_props
    (Command_menu.props ~id:"menu" ~title:"Command menu" ?open_
       Combobox_samples.commands
    )

let command_menu_open_has_search :
    (Dialog.state, string Dialog.event) Check.property =
  Check.always ~name:"open-has-search"
    (Check.holds (fun state view ->
         let dialog = has_attr "role" "dialog" view
         and search =
           exists (function Node.Component _ -> true | _ -> false) view
         in
         match state with
         | Dialog.Open ->
             dialog && search
         | Dialog.Closed ->
             (not dialog) && not search
     )
    )

let command_menu () =
  Check.component
    ~props:
      [
        command_menu_props ();
        command_menu_props ~open_:true ();
        command_menu_props ~open_:false ();
      ]
    ~inputs:(function
      | Dialog.Open -> [ Dialog.Content "settings" ] | Dialog.Closed -> []
      )
    ~properties:[ command_menu_open_has_search ]
    Command_menu.dialog.component

let command_menu_search children =
  Check.component
    ~props:(Check.props children Combobox.component)
    ~texts:[ ""; "sign" ]
    ~allow:
      [
        Check.allow ~rule:"unreached-state" ~state:"closed"
          ~reason:
            "The command menu gives the combobox inline props, and an inline \
             combobox stays in the inline state."
          ();
        Check.allow ~rule:"unreached-state" ~state:"open"
          ~reason:
            "The command menu gives the combobox inline props, and an inline \
             combobox stays in the inline state."
          ();
      ]
    ~properties:[ combobox_active_descendant; combobox_auto_highlight ]
    Combobox.component

let checkbox_props ?checked ?indeterminate ?disabled () =
  Checkbox.props ~id:"terms" ~label:"Accept" ?checked ?indeterminate ?disabled
    ~name:"terms" ()

let checkbox_aria : (Checkbox.state, Checkbox.event) Check.property =
  Check.always ~name:"aria-checked-matches-state"
    (Check.holds (fun state view ->
         let shows value =
           has_attr "role" "checkbox" view && has_attr "aria-checked" value view
         in
         match state with
         | Checkbox.Unchecked ->
             shows "false" && not (has_attr "type" "hidden" view)
         | Checkbox.Checked ->
             shows "true" && has_attr "value" "on" view
         | Checkbox.Indeterminate ->
             shows "mixed" && not (has_attr "type" "hidden" view)
     )
    )

let checkbox () =
  Check.component
    ~props:
      [
        checkbox_props ();
        checkbox_props ~checked:true ();
        checkbox_props ~indeterminate:true ();
        checkbox_props ~disabled:true ();
      ]
    ~properties:[ checkbox_aria ] Checkbox.component

let switch_props ?checked ?disabled () =
  Switch.props ~id:"wifi" ~label:"Wi-Fi" ?checked ?disabled ~name:"wifi" ()

let switch_aria : (Switch.state, Switch.event) Check.property =
  Check.always ~name:"aria-checked-matches-state"
    (Check.holds (fun state view ->
         let shows value =
           has_attr "role" "switch" view && has_attr "aria-checked" value view
         in
         match state with
         | Switch.Unchecked ->
             shows "false" && not (has_attr "type" "hidden" view)
         | Switch.Checked ->
             shows "true" && has_attr "value" "on" view
     )
    )

let switch () =
  Check.component
    ~props:
      [
        switch_props ();
        switch_props ~checked:true ();
        switch_props ~disabled:true ();
      ]
    ~properties:[ switch_aria ] Switch.component

let tabs_list =
  [
    Tabs.tab ~value:"a" "Account";
    Tabs.tab ~value:"b" ~disabled:true "Billing";
    Tabs.tab ~value:"c" "Closed";
  ]

let tabs_props ?orientation value =
  Tabs.props ~id:"set" ?orientation ~value tabs_list

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

let tabs_roving : (Tabs.state, Tabs.event) Check.property =
  Check.always ~name:"one-tab-stop-on-the-focused-tab"
    (Check.holds (fun (Tabs.Ready model) view ->
         let stops =
           List.filter
             (fun (tab : Tabs.tab) ->
               element_has ("set-tab-" ^ tab.value) [ ("tabindex", "0") ] view
             )
             tabs_list
         in
         List.map (fun (tab : Tabs.tab) -> tab.value) stops = [ model.focused ]
         && element_has
              ("set-tab-" ^ model.selected)
              [ ("aria-selected", "true") ]
              view
     )
    )

let tabs () =
  Check.component
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
    ~properties:[ tabs_roving ] Tabs.component

let toast_props toasts = Toast.props ~id:"note" ~timeout_ms:200 toasts

let first = Toast.toast ~key:"a" ~description:"Saved." "Saved"
let urgent = Toast.toast ~key:"b" ~priority:Toast.High "Failed"

let toast_shows_list : (Toast.state, Toast.event) Check.property =
  Check.always ~name:"region-shows-each-toast"
    (Check.holds (fun state view ->
         let shown =
           List.filter
             (fun (toast : Toast.toast) ->
               element_has ("note-toast-" ^ toast.key) [] view
             )
             [ first; urgent ]
         in
         let shows ~paused =
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
                shown
           && List.length shown = attr_count "aria-modal" view
         in
         match state with
         | Toast.Running _ ->
             shown <> [] && shows ~paused:false
         | Toast.Paused _ ->
             shown <> [] && shows ~paused:true
         | Toast.Empty _ ->
             shown = [] && shows ~paused:false
         | Toast.Expired _ ->
             shows ~paused:false
     )
    )

let toast () =
  Check.component
    ~props:
      [ toast_props []; toast_props [ first ]; toast_props [ first; urgent ] ]
    ~properties:[ toast_shows_list ] Toast.component

let theme_set = Theme.set ~light:Theme.light ~dark:Theme.dark Theme.shipped

let theme_props choice = Theme.props ~id:"theme" theme_set choice

let theme_current : (Theme.state, Theme.event) Check.property =
  Check.always ~name:"current-theme-in-set-and-scheme"
    (Check.holds (fun state _ ->
         match (state, Theme.current (theme_props Theme.System) state) with
         | Theme.Fixed_theme theme, Some current ->
             current.theme == theme && List.memq theme (Theme.themes theme_set)
         | Theme.System_scheme None, None ->
             true
         | Theme.System_scheme (Some dark), Some current ->
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
     )
    )

let theme () =
  Check.component
    ~props:[ theme_props Theme.System; theme_props (Theme.Fixed Theme.ocean) ]
    ~inputs:(fun _ ->
      List.map
        (fun name -> Theme.Choose name)
        [ "system"; "light"; "high-contrast"; "sepia" ]
    )
    ~properties:[ theme_current ] Theme.provider_component
