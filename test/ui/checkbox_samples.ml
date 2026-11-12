open Silica
open Silica_ui

type page = {
  checked : bool;
  indeterminate : bool;
  disabled : bool;
  log : string list;
}

type event =
  | Changed of bool
  | Set_checked of bool
  | Set_indeterminate of bool
  | Set_disabled of bool

let machine =
  Machine.make ~id:"checkbox-page"
    ~init:{ checked = false; indeterminate = false; disabled = false; log = [] }
    ~name:(fun _ -> "page")
    (fun page event ->
      match event with
      | Changed checked ->
          Go
            {
              page with
              checked;
              indeterminate = false;
              log = page.log @ [ Printf.sprintf "checked %b" checked ];
            }
      | Set_checked checked ->
          Go { page with checked }
      | Set_indeterminate indeterminate ->
          Go { page with indeterminate }
      | Set_disabled disabled ->
          Go { page with disabled }
    )

let view page =
  Node.element "form" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " page.log) ];
      Checkbox.make
        ~on_checked_change:(fun checked -> Changed checked)
        (Checkbox.props ~id:"terms" ~label:"Accept the terms"
           ~checked:page.checked ~indeterminate:page.indeterminate
           ~disabled:page.disabled ~name:"terms" ()
        );
    ]

let page = Component.make machine view
