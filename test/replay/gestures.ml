open Silica

type event = Fired of string
type state = Idle | Done of string

let kinds =
  [
    "double_press"; "focus"; "blur"; "key_up"; "pointer_enter"; "pointer_leave";
  ]

let machine =
  Machine.make ~id:"gestures" ~init:Idle
    ~name:(function Idle -> "idle" | Done kind -> kind)
    ~states:("idle" :: kinds)
    ~event_name:(fun (Fired kind) -> kind)
    (fun _ (Fired kind) -> Machine.Go (Done kind))

let fired kind = Some (Fired kind)

let view ~answered state =
  Node.element "div" []
    [
      Node.element "button"
        [ Attr.on_double_press (Fired "double_press") ]
        [ Node.text "Twice" ];
      Node.element "input"
        [
          Attr.string "aria-label" "Field";
          Attr.on_focus (Fired "focus");
          Attr.on_blur (Fired "blur");
          Attr.on_key_up (fun (key : Key.t) ->
              if key.key = "Enter" then
                fired "key_up"
              else
                None
          );
        ]
        [];
      Node.element "button"
        [
          Attr.on_pointer_enter (fun _ -> fired "pointer_enter");
          Attr.on_pointer_leave (fun _ -> fired "pointer_leave");
        ]
        [ Node.text "Area" ];
      Node.element "p" []
        [
          Node.text
            ( match state with
            | Done _ when answered ->
                "Answered"
            | Done _ | Idle ->
                ""
            );
        ];
    ]

let component = Component.make machine (view ~answered:false)
let fixed = Component.make machine (view ~answered:true)

let check_of component =
  Check.component ~keys:[ Key.enter ]
    ~properties:
      [
        Check.always ~name:"a gesture is answered"
          Check.(
            not_ (state (function Done _ -> true | Idle -> false))
            ||| shows "Answered"
          );
      ]
    component

let check = check_of component
let fixed_check = check_of fixed
