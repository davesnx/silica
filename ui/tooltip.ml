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
  | Props of props
  | Hover
  | Focus
  | Cancel
  | Leave
  | Resume
  | Close of Popup.reason
  | Elapsed of delay

module State = struct
  type _ t =
    | Idle : unit t
    | Closed : props t
    | Opening : props t
    | Open : props t
    | Closing : props t
end

module Tooltip_machine = Machine.Make (State)

type snapshot = Tooltip_machine.snapshot =
  | At : 'data State.t * 'data -> snapshot

let timers =
  List.map
    (fun delay -> (delay_ms delay, Elapsed delay))
    [ Instant; Short; Medium; Long ]

let sources =
  List.map
    (Popup.scoped (fun (props : props) -> props.id))
    [
      Popup.dismiss ~escape:(Close Popup.Escape_key) Popup.template;
      Source.anchor
        ~place:(fun (props : props) -> props.place)
        ~anchor:Popup.template.trigger Popup.template.popup;
    ]

let machine =
  Tooltip_machine.make ~id:"tooltip"
    ~init:(Tooltip_machine.at Idle ())
    [
      Tooltip_machine.state Idle "idle" ~to_:[ Closed ] (fun () -> function
        | Props props ->
            Tooltip_machine.goto Closed props
        | _ ->
            Tooltip_machine.unhandled
        );
      Tooltip_machine.state Closed "closed" ~to_:[ Opening; Open ]
        (fun previous -> function
        | Props props ->
            Tooltip_machine.goto Closed props
        | Hover when previous.delay = Instant ->
            Tooltip_machine.goto Open previous
        | Hover ->
            Tooltip_machine.goto Opening previous
        | Focus ->
            Tooltip_machine.goto Open previous
        | _ ->
            Tooltip_machine.unhandled
        );
      Tooltip_machine.state Opening "opening" ~to_:[ Open; Closed ]
        ~after:timers (fun previous -> function
        | Props props when props.delay <> previous.delay ->
            Tooltip_machine.reenter Opening props
        | Props props ->
            Tooltip_machine.goto Opening props
        | Elapsed delay when delay = previous.delay ->
            Tooltip_machine.goto Open previous
        | Cancel ->
            Tooltip_machine.goto Closed previous
        | _ ->
            Tooltip_machine.unhandled
        );
      Tooltip_machine.state Open "open" ~to_:[ Closing; Closed ] ~invoke:sources
        (fun previous -> function
        | Props props when props.place <> previous.place ->
            Tooltip_machine.reenter Open props
        | Props props ->
            Tooltip_machine.goto Open props
        | Leave when previous.close_delay = Instant ->
            Tooltip_machine.goto Closed previous
        | Leave ->
            Tooltip_machine.goto Closing previous
        | Close _ ->
            Tooltip_machine.goto Closed previous
        | _ ->
            Tooltip_machine.unhandled
        );
      Tooltip_machine.state Closing "closing" ~to_:[ Open; Closed ]
        ~after:timers ~invoke:sources (fun previous -> function
        | Props props
          when props.close_delay <> previous.close_delay
               || props.place <> previous.place ->
            Tooltip_machine.reenter Closing props
        | Props props ->
            Tooltip_machine.goto Closing props
        | Elapsed delay when delay = previous.close_delay ->
            Tooltip_machine.goto Closed previous
        | Resume ->
            Tooltip_machine.goto Open previous
        | Close _ ->
            Tooltip_machine.goto Closed previous
        | _ ->
            Tooltip_machine.unhandled
        );
    ]

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

let view : snapshot -> event Node.t = function
  | At (Idle, ()) ->
      Node.list []
  | At (Closed, props) ->
      trigger props ~shown:false [ on_enter Hover; Attr.on_focus Focus ]
  | At (Opening, props) ->
      trigger props ~shown:false [ on_leave Cancel; Attr.on_press Cancel ]
  | At (Open, props) ->
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
  | At (Closing, props) ->
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

let component = Component.make machine view

let props ~id ~label ~tip ?(delay = Medium) ?(close_delay = Instant)
    ?(place = { Place.default with side = Place.Top; align = Place.Center })
    ?(classes = no_classes) () =
  { id; label; tip; delay; close_delay; place; classes }

let make ?key ?on_open_change props =
  let report value reason =
    Option.to_list (Option.map (fun f -> f value reason) on_open_change)
  in
  let output event (snapshot : snapshot) =
    match (event, snapshot) with
    | Focus, At (Open, _) ->
        report true Popup.Trigger_focus
    | (Hover | Elapsed _), At (Open, _) ->
        report true Popup.Trigger_hover
    | (Leave | Elapsed _), At (Closed, _) ->
        report false Popup.Trigger_hover
    | Close reason, At (Closed, _) ->
        report false reason
    | _ ->
        []
  in
  Component.node ?key ~props:(Props props) ~output component
