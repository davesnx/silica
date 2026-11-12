open Silica
open Silica_ui

let fruits =
  Combobox.
    [
      item ~keywords:[ "red" ] "apple" "Apple";
      item "banana" "Banana";
      item ~disabled:true "cherry" "Cherry";
      item ~keywords:[ "vine" ] "grape" "Grape";
    ]

let commands =
  Combobox.
    [
      item ~keywords:[ "preferences" ] "settings" "Open settings";
      item "new-issue" "New issue";
      item ~keywords:[ "exit" ] "sign-out" "Sign out";
    ]

type event =
  | Chose of string
  | Open_menu
  | Changed of bool * Popup.reason
  | Ran of string

let log_line = function
  | Chose value ->
      "value " ^ value
  | Open_menu ->
      "shortcut"
  | Changed (opened, reason) ->
      Printf.sprintf "open %b %s" opened (Popup.reason_name reason)
  | Ran value ->
      "run " ^ value

let page_machine ~id ~during =
  Machine.make ~id ~init:(None, [])
    ~name:(fun _ -> "page")
    ~during:(fun _ -> during)
    (fun (open_, log) event ->
      let log = log @ [ log_line event ] in
      match event with
      | Open_menu ->
          Go (Some true, log)
      | Changed (opened, _) ->
          Go (Some opened, log)
      | Chose _ | Ran _ ->
          Go (open_, log)
    )

let log_view log =
  Node.element "p"
    [ Attr.string "id" "log" ]
    [ Node.text (String.concat "; " log) ]

let combobox_page =
  Component.make (page_machine ~id:"combobox-page" ~during:[]) (fun (_, log) ->
      Node.element "main" []
        [
          log_view log;
          Combobox.make
            ~on_value_change:(fun value -> Chose value)
            (Combobox.props ~id:"fruit" ~label:"Fruit" ~empty:"No fruit" fruits);
          Combobox.make
            ~on_value_change:(fun value -> Chose value)
            (Combobox.props ~id:"pick" ~label:"Pick" ~inline:true fruits);
        ]
  )

let shortcut =
  Source.keys ~prevent:true
    [
      (Key.make ~ctrl:true "k", Open_menu); (Key.make ~meta:true "k", Open_menu);
    ]

let menu_page =
  Component.make (page_machine ~id:"menu-page" ~during:[ shortcut ])
    (fun (open_, log) ->
      Node.element "main" []
        [
          log_view log;
          Command_menu.make
            ~on_open_change:(fun opened reason -> Changed (opened, reason))
            ~on_run:(fun value -> Ran value)
            (Command_menu.props ~id:"menu" ~title:"Command menu"
               ~placeholder:"Type a command" ?open_ commands
            );
        ]
  )
