open Silica
open Silica_ui

type page = { checked : bool; disabled : bool; log : string list }
type event = Changed of bool | Set_checked of bool | Set_disabled of bool

module Page_state = struct
  type _ t = Page : page t
end

module Page_machine = Machine.Make (Page_state)

let machine =
  Page_machine.make ~id:"switch-page"
    ~init:(Page_machine.at Page { checked = false; disabled = false; log = [] })
    [
      Page_machine.state Page "page" (fun page -> function
        | Changed checked ->
            Page_machine.goto Page
              {
                page with
                checked;
                log = page.log @ [ Printf.sprintf "checked %b" checked ];
              }
        | Set_checked checked ->
            Page_machine.goto Page { page with checked }
        | Set_disabled disabled ->
            Page_machine.goto Page { page with disabled }
        );
    ]

let view (Page_machine.At (Page, page)) =
  Node.element "form" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " page.log) ];
      Switch.make
        ~on_checked_change:(fun checked -> Changed checked)
        (Switch.props ~id:"wifi" ~label:"Wi-Fi" ~checked:page.checked
           ~disabled:page.disabled ~name:"wifi" ()
        );
    ]

let page = Component.make machine view
