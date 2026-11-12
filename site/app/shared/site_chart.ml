open Silica

let view ?current ?(smallest = 0.75) (chart : Statechart_layout.t) =
  Node.element "div"
    [
      Attr.string "class" "mx-auto [&>svg]:h-auto [&>svg]:w-full";
      Attr.string "style"
        (Printf.sprintf "max-width: %.0fpx; min-width: %.0fpx" chart.width
           (smallest *. chart.width)
        );
    ]
    [ Statechart_view.view ?current chart ]
