open Silica

type classes = { trigger : string; popup : string }

let no_classes = { trigger = ""; popup = "" }

type delay = Instant | Short | Medium | Long

let delay_ms = function
  | Instant ->
      0
  | Short ->
      300
  | Medium ->
      600
  | Long ->
      1000

type props = {
  id : string;
  label : string;
  tip : string;
  delay : delay;
  close_delay : delay;
  place : Place.t;
  classes : classes;
}

type event =
  | Hover
  | Focus
  | Cancel
  | Leave
  | Resume
  | Close of Popup.reason
  | Elapsed of delay

type state = Closed | Opening | Open | Closing [@@deriving state_shape]

let timer delay = Source.after (delay_ms delay) (Elapsed delay)

let layer (props : props) =
  Popup.layer ~escape:(Close Popup.Escape_key) ~place:props.place
    (Popup.parts props.id)

let machine =
  Machine.make_with_props ~id:"tooltip" ~init:Closed ~shape:state_shape
    ~owns:(fun (props : props) -> function
      | Closed ->
          []
      | Opening ->
          [ timer props.delay ]
      | Open ->
          layer props
      | Closing ->
          timer props.close_delay :: layer props
      )
    ~on_props:(fun previous props state ->
      match (previous, state) with
      | Some (previous : props), ((Open | Closing) as shown)
        when previous.place <> props.place ->
          Reenter shown
      | (Some _ | None), (Closed | Opening | Open | Closing) ->
          Ignore
    )
    (fun (props : props) state event ->
      match (state, event) with
      | Closed, Hover when props.delay = Instant ->
          Go Open
      | Closed, Hover ->
          Go Opening
      | Closed, Focus ->
          Go Open
      | Opening, Elapsed delay when delay = props.delay ->
          Go Open
      | Opening, Cancel ->
          Go Closed
      | Open, Leave when props.close_delay = Instant ->
          Go Closed
      | Open, Leave ->
          Go Closing
      | (Open | Closing), Close _ ->
          Go Closed
      | Closing, Elapsed delay when delay = props.close_delay ->
          Go Closed
      | Closing, Resume ->
          Go Open
      | Closed, (Cancel | Leave | Resume | Close _ | Elapsed _)
      | Opening, (Hover | Focus | Leave | Resume | Close _ | Elapsed _)
      | Open, (Hover | Focus | Cancel | Resume | Elapsed _)
      | Closing, (Hover | Focus | Cancel | Leave | Elapsed _) ->
          Ignore
    )

let el = Node.element
let on_enter event = Attr.on_pointer_enter (fun _ -> Some event)
let on_leave event = Attr.on_pointer_leave (fun _ -> Some event)

let trigger (props : props) handlers ~shown =
  let parts = Popup.parts props.id in
  el "button"
    ([
       Attr.string "type" "button";
       Attr.string "id" parts.trigger_id;
       Attr.ref_ parts.trigger;
     ]
    @ ( if shown then
          [
            Attr.string "aria-describedby" parts.popup_id;
            Attr.string "data-popup-open" "";
          ]
        else
          []
      )
    @ Popup.open_attrs shown
    @ Popup.class_attr props.classes.trigger
    @ handlers
    )
    [ Node.text props.label ]

let popup (props : props) handlers =
  let parts = Popup.parts props.id in
  Node.portal
    [
      el "div"
        ([
           Attr.string "id" parts.popup_id;
           Attr.ref_ parts.popup;
           Attr.string "role" "tooltip";
         ]
        @ Popup.open_attrs true
        @ Popup.class_attr props.classes.popup
        @ handlers
        )
        [ Node.text props.tip ];
    ]

let view props = function
  | Closed ->
      trigger props ~shown:false [ on_enter Hover; Attr.on_focus Focus ]
  | Opening ->
      trigger props ~shown:false [ on_leave Cancel; Attr.on_press Cancel ]
  | Open ->
      Node.list
        [
          trigger props ~shown:true
            [
              on_leave Leave;
              Attr.on_press (Close Popup.Trigger_press);
              Attr.on_blur (Close Popup.Trigger_focus);
            ];
          popup props [ on_leave Leave ];
        ]
  | Closing ->
      Node.list
        [
          trigger props ~shown:true
            [
              on_enter Resume;
              Attr.on_focus Resume;
              Attr.on_press (Close Popup.Trigger_press);
              Attr.on_blur (Close Popup.Trigger_focus);
            ];
          popup props [ on_enter Resume ];
        ]

let component = Component.make_with_props machine view

let props ~id ~label ~tip ?(delay = Medium) ?(close_delay = Instant)
    ?(place = { Place.default with side = Place.Top; align = Place.Center })
    ?(classes = no_classes) () =
  { id; label; tip; delay; close_delay; place; classes }

let make ?key ?on_open_change props =
  let report = Popup.report on_open_change in
  let output event state =
    match (event, state) with
    | Focus, Open ->
        report true Popup.Trigger_focus
    | (Hover | Elapsed _), Open ->
        report true Popup.Trigger_hover
    | (Leave | Elapsed _), Closed ->
        report false Popup.Trigger_hover
    | Close reason, Closed ->
        report false reason
    | (Focus | Cancel | Resume | Close _), (Closed | Opening | Open | Closing)
    | (Hover | Leave | Elapsed _), (Opening | Closing)
    | Hover, Closed
    | Leave, Open ->
        []
  in
  Component.node_with_props ?key ~props ~output component
