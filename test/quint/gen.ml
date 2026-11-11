open Silica

type event = Press | Leave | Timeout | Answer

type state = Closed | Open | Gone [@@deriving state_shape]

let endpoint =
  Remote.declare ~path:"/api/x" ~input:Codec.unit ~output:Codec.unit
    ~rejected:Codec.string

let panel = Ref.make "panel"

let door =
  Machine.make ~id:"door" ~init:Closed ~shape:state_shape
    ~final:(function Gone -> true | Closed | Open -> false)
    ~owns:(function
      | Open ->
          [
            Source.after 300 Timeout;
            Remote.call endpoint () (fun _ -> Answer);
            Source.focus_trap panel;
          ]
      | Closed | Gone ->
          []
      )
    (fun state event ->
      match (state, event) with
      | Closed, Press ->
          Machine.Go Open
      | Open, (Press | Timeout) ->
          Machine.Go Closed
      | Open, Leave ->
          Machine.Go Gone
      | Open, Answer ->
          Machine.Stay []
      | _ ->
          Machine.Ignore
    )

let button label event =
  Node.element "button" [ Attr.on_press event ] [ Node.text label ]

let view = function
  | Closed ->
      button "open" Press
  | Open ->
      Node.element "div"
        [ Attr.ref_ panel ]
        [ button "close" Press; button "leave" Leave ]
  | Gone ->
      Node.text "gone"

let label = function
  | Press ->
      "press"
  | Leave ->
      "leave"
  | Timeout ->
      "after 300ms"
  | Answer ->
      "answer"

let state ?(final = false) name : Spec.state =
  { name; final; timers = []; sources = []; held = [] }

let dead_end : Spec.t =
  {
    id = "dead_end";
    init = "start";
    states = [ state "start"; state "stuck" ];
    edges = [ ("start", "stuck") ];
  }

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let () =
  match Check.component ~label (Component.make door view) with
  | Check.Ok { spec; edges; _ } ->
      write "door.qnt" (Spec.to_quint ~labels:edges spec);
      write "dead_end.qnt" (Spec.to_quint dead_end)
  | _ ->
      prerr_endline "gen: the check of the door machine did not pass";
      exit 1
