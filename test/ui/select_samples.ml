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

module States = struct
  type _ t = Page : (string option * string list) t
end

module P = Machine.Make (States)

let machine =
  P.make ~id:"select-page"
    ~init:(P.at Page (None, []))
    [
      P.state Page "page" (fun (value, log) -> function
        | Changed (id, v) ->
            P.goto Page (value, log @ [ Printf.sprintf "%s value %s" id v ])
        | Opened (id, opened, why) ->
            P.goto Page
              ( value,
                log
                @ [
                    Printf.sprintf "%s open %b %s" id opened
                      (Popup.reason_name why);
                  ]
              )
        | Set_value value ->
            P.goto Page (value, log)
        );
    ]

let select ?value ?name id =
  Select.make
    ~on_value_change:(fun v -> Changed (id, v))
    ~on_open_change:(fun opened why -> Opened (id, opened, why))
    (props ?value ?name id)

let view (P.At (Page, (value, log))) =
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " log) ];
      Node.element "form" []
        [ select ?value ~name:"status" "status"; select "priority" ];
    ]

let page = Component.make machine view
