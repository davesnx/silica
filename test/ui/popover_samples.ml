open Silica
open Silica_ui

type filter = { query : string }
type filter_event = Query of string | Apply

let filter_view { query } =
  Node.element "form"
    [ Attr.on_submit Apply ]
    [
      Node.element "input"
        [
          Attr.string "id" "filter-query";
          Attr.string "aria-label" "Query";
          Attr.string "value" query;
          Attr.on_input (fun value -> Query value);
        ]
        [];
      Node.element "button"
        [ Attr.string "type" "submit"; Attr.string "id" "filter-apply" ]
        [ Node.text "Apply" ];
    ]

let filter_popover =
  Popover.component ~id:"filter-popover"
    ~closes:(function Apply -> true | Query _ -> false)
    filter_view

type event =
  | Opened of bool * Popup.reason
  | Edited of filter_event
  | Set_open of bool option

let machine =
  Machine.make ~id:"filter-page" ~init:(None, [])
    ~shape:(Machine.shape ~name:(fun _ -> "page") ())
    (fun (open_, log) event ->
      match event with
      | Opened (opened, why) ->
          Go
            ( open_,
              log
              @ [ Printf.sprintf "open %b %s" opened (Popup.reason_name why) ]
            )
      | Edited (Query text) ->
          Go (open_, log @ [ "query " ^ text ])
      | Edited Apply ->
          Go (open_, log @ [ "apply" ])
      | Set_open open_ ->
          Go (open_, log)
    )

let props ?open_ () =
  Popover.props ~id:"filter" ~trigger:"Filter" ~title:"Filter issues"
    ~close:"Close" ?open_
    ~place:{ Place.default with side = Place.Top }
    { query = "bug" }

let view (open_, log) =
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " log) ];
      Popover.make
        ~on_open_change:(fun opened why -> Opened (opened, why))
        ~on_content:(fun event -> Edited event)
        filter_popover (props ?open_ ());
    ]

let page = Component.make machine view
