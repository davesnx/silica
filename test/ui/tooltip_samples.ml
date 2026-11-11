open Silica
open Silica_ui

type event = Changed of bool * Popup.reason | Set_delay of Tooltip.delay

let machine ~delay =
  Machine.make ~id:"tooltip-page" ~init:(delay, [])
    ~shape:(Machine.shape ~name:(fun _ -> "page") ())
    (fun (delay, log) event ->
      match event with
      | Changed (opened, why) ->
          Go
            ( delay,
              log
              @ [ Printf.sprintf "open %b %s" opened (Popup.reason_name why) ]
            )
      | Set_delay delay ->
          Go (delay, log)
    )

let props ?(delay = Tooltip.Medium) ?close_delay () =
  Tooltip.props ~id:"save" ~label:"Save" ~tip:"Save the draft" ~delay
    ?close_delay ()

let page ?(delay = Tooltip.Medium) ?close_delay () =
  Component.make (machine ~delay) (fun (delay, log) ->
      Node.element "main" []
        [
          Node.element "p"
            [ Attr.string "id" "log" ]
            [ Node.text (String.concat "; " log) ];
          Tooltip.make
            ~on_open_change:(fun opened why -> Changed (opened, why))
            (props ~delay ?close_delay ());
        ]
  )
