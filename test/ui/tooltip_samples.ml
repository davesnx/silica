open Silica
open Silica_ui

type event = Changed of bool * Popup.reason | Set_delay of Tooltip.delay

module State = struct
  type _ t = Page : (Tooltip.delay * string list) t
end

module Page = Machine.Make (State)

let machine ~delay =
  Page.make ~id:"tooltip-page"
    ~init:(Page.at Page (delay, []))
    [
      Page.state Page "page" (fun (delay, log) -> function
        | Changed (opened, why) ->
            Page.goto Page
              ( delay,
                log
                @ [ Printf.sprintf "open %b %s" opened (Popup.reason_name why) ]
              )
        | Set_delay delay ->
            Page.goto Page (delay, log)
        );
    ]

let props ?(delay = Tooltip.Medium) ?close_delay () =
  Tooltip.props ~id:"save" ~label:"Save" ~tip:"Save the draft" ~delay
    ?close_delay ()

let page ?(delay = Tooltip.Medium) ?close_delay () =
  Component.make (machine ~delay) (fun (Page.At (Page, (delay, log))) ->
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
