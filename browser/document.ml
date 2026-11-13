type request =
  | Set_title of string
  | Set_cookie of { name : string; value : string; max_age : int }
  | Set_document_attribute of { name : string; value : string option }
  | Lock_scroll
  | Unlock_scroll

type t = { port : request -> unit; mutable locks : int }

let saved_scroll_style = ref ("", "")

let apply = function
  | Set_title title ->
      Dom_bindings.set_title_ Dom_bindings.document title
  | Set_cookie { name; value; max_age } ->
      Dom_bindings.set_cookie name value max_age
  | Set_document_attribute { name; value } ->
      Dom_bindings.set_document_attribute name (Js.Nullable.fromOption value)
  | Lock_scroll ->
      let root = Dom_bindings.document_element Dom_bindings.document in
      let width = Dom_bindings.scrollbar_width () in
      saved_scroll_style :=
        ( Dom_bindings.style_get root "overflow",
          Dom_bindings.style_get root "paddingRight"
        );
      Dom_bindings.style_set root "overflow" "hidden";
      if width > 0 then
        Dom_bindings.style_set root "paddingRight" (string_of_int width ^ "px")
  | Unlock_scroll ->
      let root = Dom_bindings.document_element Dom_bindings.document in
      let overflow, padding = !saved_scroll_style in
      Dom_bindings.style_set root "overflow" overflow;
      Dom_bindings.style_set root "paddingRight" padding

let make port = { port; locks = 0 }
let browser = make apply

let send document request =
  match request with
  | Lock_scroll ->
      document.locks <- document.locks + 1;
      if document.locks = 1 then document.port Lock_scroll
  | Unlock_scroll ->
      if document.locks > 0 then (
        document.locks <- document.locks - 1;
        if document.locks = 0 then document.port Unlock_scroll
      )
  | Set_title _ | Set_cookie _ | Set_document_attribute _ ->
      document.port request

let lock_scroll document =
  send document Lock_scroll;
  fun () -> send document Unlock_scroll
