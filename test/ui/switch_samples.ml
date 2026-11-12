open Silica
open Silica_ui

type page = { checked : bool; disabled : bool; log : string list }
type event = Changed of bool | Set_checked of bool | Set_disabled of bool

let machine =
  Machine.make ~id:"switch-page"
    ~init:{ checked = false; disabled = false; log = [] }
    ~name:(fun _ -> "page")
    (fun page event ->
      match event with
      | Changed checked ->
          Go
            {
              page with
              checked;
              log = page.log @ [ Printf.sprintf "checked %b" checked ];
            }
      | Set_checked checked ->
          Go { page with checked }
      | Set_disabled disabled ->
          Go { page with disabled }
    )

let view page =
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
