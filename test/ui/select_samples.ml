open Silica
open Silica_ui

let item ?(disabled = false) value label = { Select.value; label; disabled }

let items =
  [
    item "backlog" "Backlog";
    item "todo" "Todo";
    item ~disabled:true "blocked" "Blocked";
    item "progress" "In progress";
    item "done" "Done";
    item "duplicate" "Duplicate";
  ]

let props ?value ?name id =
  Select.props ~id ~label:"Status" ?value ~placeholder:"Pick a status" ?name
    items

type event =
  | Changed of string * string
  | Opened of string * bool * Popup.reason
  | Set_value of string option

let machine =
  Machine.make ~id:"select-page" ~init:(None, [])
    ~shape:(Machine.shape ~name:(fun _ -> "page") ())
    (fun (value, log) event ->
      match event with
      | Changed (id, chosen) ->
          Go (value, log @ [ Printf.sprintf "%s value %s" id chosen ])
      | Opened (id, opened, why) ->
          Go
            ( value,
              log
              @ [
                  Printf.sprintf "%s open %b %s" id opened
                    (Popup.reason_name why);
                ]
            )
      | Set_value value ->
          Go (value, log)
    )

let select ?value ?name id =
  Select.make
    ~on_value_change:(fun value -> Changed (id, value))
    ~on_open_change:(fun opened why -> Opened (id, opened, why))
    (props ?value ?name id)

let view (value, log) =
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " log) ];
      Node.element "form" []
        [ select ?value ~name:"status" "status"; select "priority" ];
    ]

let page = Component.make machine view
