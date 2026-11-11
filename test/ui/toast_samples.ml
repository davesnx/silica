open Silica
open Silica_ui

type page = { toasts : Toast.toast list; next : int; log : string list }
type event = Add of Toast.priority | Closed of string

let machine =
  Machine.make ~id:"toast-page"
    ~init:{ toasts = []; next = 1; log = [] }
    ~shape:(Machine.shape ~name:(fun _ -> "page") ())
    (fun page event ->
      match event with
      | Add priority ->
          let key = "t" ^ string_of_int page.next in
          let toast =
            Toast.toast ~key ~description:("Message " ^ key) ~priority
              ("Saved " ^ key)
          in
          Go
            { page with toasts = page.toasts @ [ toast ]; next = page.next + 1 }
      | Closed key ->
          Go
            {
              page with
              toasts =
                List.filter
                  (fun (toast : Toast.toast) -> toast.key <> key)
                  page.toasts;
              log = page.log @ [ "close " ^ key ];
            }
    )

let view page =
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
