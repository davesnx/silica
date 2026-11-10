open Silica
open Silica_ui

module Filter = struct
  type props = { query : string }
  type event = Query of string | Apply

  let id = "filter-popover"

  let view { query } =
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

  let closes = function Apply -> true | Query _ -> false
end

module Filter_popover = Popover.Make (Filter)

type event =
  | Opened of bool * Popup.reason
  | Edited of Filter.event
  | Set_open of bool option

module State = struct
  type _ t = Page : (bool option * string list) t
end

module Page = Machine.Make (State)

let machine =
  Page.make ~id:"filter-page"
    ~init:(Page.at Page (None, []))
    [
      Page.state Page "page" (fun (open_, log) -> function
        | Opened (opened, why) ->
            Page.goto Page
              ( open_,
                log
                @ [ Printf.sprintf "open %b %s" opened (Popup.reason_name why) ]
              )
        | Edited (Filter.Query text) ->
            Page.goto Page (open_, log @ [ "query " ^ text ])
        | Edited Filter.Apply ->
            Page.goto Page (open_, log @ [ "apply" ])
        | Set_open open_ ->
            Page.goto Page (open_, log)
        );
    ]

let props ?open_ () =
  Filter_popover.props ~id:"filter" ~trigger:"Filter" ~title:"Filter issues"
    ~close:"Close" ?open_
    ~place:{ Place.default with side = Place.Top }
    { Filter.query = "bug" }

let view (Page.At (Page, (open_, log))) =
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " log) ];
      Filter_popover.make
        ~on_open_change:(fun opened why -> Opened (opened, why))
        ~on_content:(fun event -> Edited event)
        (props ?open_ ());
    ]

let page = Component.make machine view
