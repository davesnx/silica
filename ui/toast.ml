open Silica

type priority = Low | High

type toast = {
  key : string;
  title : string;
  description : string option;
  priority : priority;
}

type classes = {
  viewport : string;
  toast : string;
  title : string;
  description : string;
  close : string;
}

let no_classes =
  { viewport = ""; toast = ""; title = ""; description = ""; close = "" }

type props = {
  id : string;
  toasts : toast list;
  timeout_ms : int;
  label : string;
  close_label : string;
  classes : classes;
}

type event = Close of string | Tick | Hover of bool | Focus of bool

type model = { hovered : bool; focused : bool }

type state =
  | Empty of model
  | Running of model * int
  | Paused of model
  | Expired of model

let name = function
  | Empty _ ->
      "empty"
  | Running _ ->
      "running"
  | Paused _ ->
      "paused"
  | Expired _ ->
      "expired"

let states = [ "empty"; "running"; "paused"; "expired" ]

let event_name = function
  | Close key ->
      "close " ^ key
  | Tick ->
      "tick"
  | Hover hovered ->
      "hover " ^ string_of_bool hovered
  | Focus focused ->
      "focus " ^ string_of_bool focused

let tick_ms = 100

let settle (props : props) model : (state, event) Machine.next =
  if props.toasts = [] then
    Go (Empty model)
  else if model.hovered || model.focused then
    Go (Paused model)
  else
    Reenter (Running (model, 0))

let shows (props : props) key =
  List.exists (fun toast -> toast.key = key) props.toasts

let machine =
  Machine.make_with_props ~id:"toast"
    ~init:(Empty { hovered = false; focused = false })
    ~name ~states ~event_name
    ~during:(fun _ -> function
      | Running _ ->
          [ Source.after tick_ms Tick ]
      | Empty _ | Paused _ | Expired _ ->
          []
      )
    ~on_props:(fun previous props state ->
      let new_list =
        match previous with
        | Some (previous : props) ->
            previous.toasts <> props.toasts
        | None ->
            true
      in
      match state with
      | Empty model | Paused model ->
          settle props model
      | (Running (model, _) | Expired model) when new_list ->
          settle props model
      | Running _ | Expired _ ->
          Ignore
    )
    (fun props state event ->
      match (state, event) with
      | Empty model, Hover hovered ->
          Go (Empty { model with hovered })
      | Empty model, Focus focused ->
          Go (Empty { model with focused })
      | Running (model, _), Hover true ->
          Go (Paused { model with hovered = true })
      | Running (model, _), Focus true ->
          Go (Paused { model with focused = true })
      | Running _, Close key when shows props key ->
          Stay []
      | Running _, (Hover false | Focus false) ->
          Stay []
      | Running (model, elapsed), Tick
        when elapsed + tick_ms >= props.timeout_ms ->
          Go (Expired model)
      | Running (model, elapsed), Tick ->
          Reenter (Running (model, elapsed + tick_ms))
      | Paused model, Hover hovered ->
          settle props { model with hovered }
      | Paused model, Focus focused ->
          settle props { model with focused }
      | Paused model, Close key when shows props key ->
          settle props { model with focused = false }
      | Expired model, Hover hovered ->
          Go (Expired { model with hovered })
      | Expired model, Focus focused ->
          Go (Expired { model with focused })
      | Expired model, Close key when shows props key ->
          Go (Expired { model with focused = false })
      | (Empty _ | Running _ | Paused _ | Expired _), (Close _ | Tick) ->
          Ignore
    )

let el = Node.element
let text = Node.text

let toast_view (props : props) toast =
  let id = props.id ^ "-toast-" ^ toast.key in
  let title_id = id ^ "-title" and description_id = id ^ "-description" in
  Node.keyed toast.key
    (el "div"
       ([
          Attr.string "id" id;
          Attr.string "role"
            (match toast.priority with High -> "alertdialog" | Low -> "dialog");
          Attr.string "aria-modal" "false";
          Attr.string "aria-labelledby" title_id;
          Attr.string "tabindex" "0";
          Attr.string "data-priority"
            (match toast.priority with High -> "high" | Low -> "low");
          Attr.on_key_down ~prevent:true (fun (key : Key.t) ->
              if key.key = "Escape" then
                Some (Close toast.key)
              else
                None
          );
        ]
       @ ( match toast.description with
         | Some _ ->
             [ Attr.string "aria-describedby" description_id ]
         | None ->
             []
         )
       @ Popup.class_attr props.classes.toast
       )
       [
         el "div"
           (Attr.string "id" title_id :: Popup.class_attr props.classes.title)
           [ text toast.title ];
         ( match toast.description with
         | Some description ->
             el "p"
               (Attr.string "id" description_id
               :: Popup.class_attr props.classes.description
               )
               [ text description ]
         | None ->
             Node.list []
         );
         el "button"
           ([
              Attr.string "type" "button";
              Attr.string "id" (id ^ "-close");
              Attr.string "aria-label" props.close_label;
              Attr.on_press (Close toast.key);
            ]
           @ Popup.class_attr props.classes.close
           )
           [
             el "span" [ Attr.string "aria-hidden" "true" ] [ text "\u{00D7}" ];
           ];
       ]
    )

let viewport (props : props) ~paused =
  el "div"
    ([
       Attr.string "id" props.id;
       Attr.string "role" "region";
       Attr.string "aria-live" "polite";
       Attr.string "aria-atomic" "false";
       Attr.string "aria-relevant" "additions text";
       Attr.string "aria-label" props.label;
       Attr.string "tabindex" "-1";
       Attr.on_pointer_enter (fun _ -> Some (Hover true));
       Attr.on_pointer_leave (fun _ -> Some (Hover false));
       Attr.on_focus (Focus true);
       Attr.on_blur (Focus false);
     ]
    @ ( if paused then
          [ Attr.string "data-paused" "" ]
        else
          []
      )
    @ Popup.class_attr props.classes.viewport
    )
    (List.map (toast_view props) props.toasts)

let view props = function
  | Empty _ | Running _ | Expired _ ->
      viewport props ~paused:false
  | Paused _ ->
      viewport props ~paused:true

let component = Component.make_with_props machine view

let toast ?description ?(priority = Low) ~key title =
  { key; title; description; priority }

let props ~id ?(timeout_ms = 5000) ?(label = "Notifications")
    ?(close_label = "Close") ?(classes = no_classes) toasts =
  if timeout_ms <= 0 then
    invalid_arg "Toast.props: timeout_ms must be more than 0";
  { id; toasts; timeout_ms; label; close_label; classes }

let make ?key ?on_close (props : props) =
  let output event state =
    match (on_close, event, state, props.toasts) with
    | Some f, Close key, _, _ ->
        [ f key ]
    | Some f, Tick, Expired _, oldest :: _ ->
        [ f oldest.key ]
    | Some _, Tick, (Empty _ | Running _ | Paused _ | Expired _), _
    | Some _, (Hover _ | Focus _), _, _
    | None, _, _, _ ->
        []
  in
  Component.node_with_props ?key ~props ~output component
