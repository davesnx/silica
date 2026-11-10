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

module Page_state = struct
  type _ t = Page : page t
end

module P = Machine.Make (Page_state)

let machine =
  P.make ~id:"checkbox-page"
    ~init:
      (P.at Page
         { checked = false; indeterminate = false; disabled = false; log = [] }
      )
    [
      P.state Page "page" (fun page -> function
        | Changed checked ->
            P.goto Page
              {
                page with
                checked;
                indeterminate = false;
                log = page.log @ [ Printf.sprintf "checked %b" checked ];
              }
        | Set_checked checked ->
            P.goto Page { page with checked }
        | Set_indeterminate indeterminate ->
            P.goto Page { page with indeterminate }
        | Set_disabled disabled ->
            P.goto Page { page with disabled }
        );
    ]

let view (P.At (Page, page)) =
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
