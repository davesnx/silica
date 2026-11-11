open Silica
open Silica_ui

type page = { selected : string; log : string list }
type event = Changed of string | Set_value of string

module Page_state = struct
  type _ t = Page : page t
end

module Page_machine = Machine.Make (Page_state)

let machine =
  Page_machine.make ~id:"tabs-page"
    ~init:(Page_machine.at Page { selected = "a"; log = [] })
    [
      Page_machine.state Page "page" (fun page -> function
        | Changed value ->
            Page_machine.goto Page
              { selected = value; log = page.log @ [ "value " ^ value ] }
        | Set_value value ->
            Page_machine.goto Page { page with selected = value }
        );
    ]

let tabs =
  [
    Tabs.tab ~value:"a" "Account";
    Tabs.tab ~value:"b" "Billing";
    Tabs.tab ~value:"c" ~disabled:true "Closed";
    Tabs.tab ~value:"d" "Danger";
  ]

let view ~orientation (Page_machine.At (Page, page)) =
  let panel value text =
    Tabs.panel ~id:"set" ~value ~selected:page.selected [ Node.text text ]
  in
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " page.log) ];
      Tabs.make
        ~on_value_change:(fun value -> Changed value)
        (Tabs.props ~id:"set" ~orientation ~value:page.selected tabs);
      panel "a" "Account panel";
      panel "b" "Billing panel";
      panel "c" "Closed panel";
      panel "d" "Danger panel";
    ]

let page = Component.make machine (view ~orientation:Tabs.Horizontal)
let vertical_page = Component.make machine (view ~orientation:Tabs.Vertical)
