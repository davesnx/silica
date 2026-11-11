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

type event =
  | Props of props
  | Close of string
  | Tick
  | Hover of bool
  | Focus of bool

type model = { props : props; hovered : bool; focused : bool }

module State = struct
  type _ t =
    | Idle : unit t
    | Empty : model t
    | Running : (model * int) t
    | Paused : model t
    | Expired : model t
end

module Toast_machine = Machine.Make (State)

type snapshot = Toast_machine.snapshot =
  | At : 'data State.t * 'data -> snapshot

let tick_ms = 100
let settle model =
  if model.props.toasts = [] then
    Toast_machine.goto Empty model
  else if model.hovered || model.focused then
    Toast_machine.goto Paused model
  else
    Toast_machine.reenter Running (model, 0)

let shows model key =
  List.exists (fun toast -> toast.key = key) model.props.toasts

let new_list model (props : props) = props.toasts <> model.props.toasts

let machine =
  Toast_machine.make ~id:"toast" ~init:(Toast_machine.at Idle ())
    [
      Toast_machine.state Idle "idle" ~to_:[ Empty; Running ]
        (fun () -> function
        | Props props ->
            settle { props; hovered = false; focused = false }
        | _ ->
            Toast_machine.unhandled
        );
      Toast_machine.state Empty "empty" ~to_:[ Running; Paused ]
        (fun model -> function
        | Props props ->
            settle { model with props }
        | Hover hovered ->
            Toast_machine.goto Empty { model with hovered }
        | Focus focused ->
            Toast_machine.goto Empty { model with focused }
        | Close _ | Tick ->
            Toast_machine.unhandled
        );
      Toast_machine.state Running "running" ~to_:[ Empty; Paused; Expired ]
        ~after:[ (tick_ms, Tick) ]
        (fun (model, elapsed) -> function
          | Props props when new_list model props ->
              settle { model with props }
          | Props props ->
              Toast_machine.goto Running ({ model with props }, elapsed)
          | Hover true ->
              Toast_machine.goto Paused { model with hovered = true }
          | Focus true ->
              Toast_machine.goto Paused { model with focused = true }
          | Hover false | Focus false ->
              Toast_machine.unhandled
          | Close key when shows model key ->
              Toast_machine.stay []
          | Close _ ->
              Toast_machine.unhandled
          | Tick when elapsed + tick_ms >= model.props.timeout_ms ->
              Toast_machine.goto Expired model
          | Tick ->
              Toast_machine.reenter Running (model, elapsed + tick_ms)
          );
      Toast_machine.state Paused "paused" ~to_:[ Empty; Running ]
        (fun model -> function
        | Props props ->
            settle { model with props }
        | Hover hovered ->
            settle { model with hovered }
        | Focus focused ->
            settle { model with focused }
        | Close key when shows model key ->
            settle { model with focused = false }
        | Close _ | Tick ->
            Toast_machine.unhandled
        );
      Toast_machine.state Expired "expired" ~to_:[ Empty; Running; Paused ]
        (fun model -> function
        | Props props when new_list model props ->
            settle { model with props }
        | Props props ->
            Toast_machine.goto Expired { model with props }
        | Hover hovered ->
            Toast_machine.goto Expired { model with hovered }
        | Focus focused ->
            Toast_machine.goto Expired { model with focused }
        | Close key when shows model key ->
            Toast_machine.goto Expired { model with focused = false }
        | Close _ | Tick ->
            Toast_machine.unhandled
        );
    ]

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

let viewport model ~paused =
  let props = model.props in
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

let view : snapshot -> event Node.t = function
  | At (Idle, ()) ->
      Node.list []
  | At (Running, (model, _)) ->
      viewport model ~paused:false
  | At (Empty, model) ->
      viewport model ~paused:false
  | At (Expired, model) ->
      viewport model ~paused:false
  | At (Paused, model) ->
      viewport model ~paused:true

let component = Component.make machine view

let toast ?description ?(priority = Low) ~key title =
  { key; title; description; priority }

let props ~id ?(timeout_ms = 5000) ?(label = "Notifications")
    ?(close_label = "Close") ?(classes = no_classes) toasts =
  if timeout_ms <= 0 then
    invalid_arg "Toast.props: timeout_ms must be more than 0";
  { id; toasts; timeout_ms; label; close_label; classes }

let make ?key ?on_close props =
  let output event (snapshot : snapshot) =
    match (on_close, event, snapshot) with
    | Some f, Close key, _ ->
        [ f key ]
    | Some f, Tick, At (Expired, { props = { toasts = oldest :: _; _ }; _ }) ->
        [ f oldest.key ]
    | _ ->
        []
  in
  Component.node ?key ~props:(Props props) ~output component
