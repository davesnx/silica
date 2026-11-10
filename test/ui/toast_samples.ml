open Silica
open Silica_ui

type page = { toasts : Toast.toast list; next : int; log : string list }
type event = Add of Toast.priority | Closed of string

module Page_state = struct
  type _ t = Page : page t
end

module P = Machine.Make (Page_state)

let machine =
  P.make ~id:"toast-page"
    ~init:(P.at Page { toasts = []; next = 1; log = [] })
    [
      P.state Page "page" (fun page -> function
        | Add priority ->
            let key = "t" ^ string_of_int page.next in
            let toast =
              Toast.toast ~key ~description:("Message " ^ key) ~priority
                ("Saved " ^ key)
            in
            P.goto Page
              {
                page with
                toasts = page.toasts @ [ toast ];
                next = page.next + 1;
              }
        | Closed key ->
            P.goto Page
              {
                page with
                toasts =
                  List.filter
                    (fun (t : Toast.toast) -> t.key <> key)
                    page.toasts;
                log = page.log @ [ "close " ^ key ];
              }
        );
    ]

let view (P.At (Page, page)) =
  Node.element "main" []
    [
      Node.element "p"
        [ Attr.string "id" "log" ]
        [ Node.text (String.concat "; " page.log) ];
      Toast.make
        ~on_close:(fun key -> Closed key)
        (Toast.props ~id:"note" ~timeout_ms:300 page.toasts);
    ]

let page = Component.make machine view
