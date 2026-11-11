open Silica
open Silica_ui

let item ?(disabled = false) value label = { Menu.value; label; disabled }

let items =
  [
    item "edit" "Edit";
    item ~disabled:true "duplicate" "Duplicate";
    item "archive" "Archive";
    item "delete" "Delete";
  ]

let props ?open_ id = Menu.props ~id ~label:"Actions" ?open_ items

type event =
  | Opened of string * bool * Popup.reason
  | Selected of string * string
  | Set_open of bool option

let machine =
  Machine.make ~id:"menu-page" ~init:(None, [])
    ~shape:(Machine.shape ~name:(fun _ -> "page") ())
    (fun (open_, log) event ->
      match event with
      | Opened (id, opened, why) ->
          Go
            ( open_,
              log
              @ [
                  Printf.sprintf "%s open %b %s" id opened
                    (Popup.reason_name why);
                ]
            )
      | Selected (id, value) ->
          Go (open_, log @ [ Printf.sprintf "%s select %s" id value ])
      | Set_open open_ ->
          Go (open_, log)
    )

let menu ?open_ id =
  Menu.make
    ~on_open_change:(fun opened why -> Opened (id, opened, why))
    ~on_select:(fun value -> Selected (id, value))
    (props ?open_ id)

let view (open_, log) =
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " log) ];
      menu ?open_ "actions";
      menu "more";
    ]

let page = Component.make machine view
