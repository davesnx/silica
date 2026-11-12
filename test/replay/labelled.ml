open Silica

type event = Go

let event_name = function Go -> "go"

type state = Shown

let name = function Shown -> "shown"

let states = [ "shown" ]

let machine =
  let machine =
    Machine.make_with_props ~id:"labelled" ~init:Shown ~name ~states ~event_name
      (fun _ Shown Go -> Machine.Stay []
    )
  in
  Machine.register_codecs machine ~props:Codec.string ();
  machine

let component =
  Component.make_with_props machine (fun label Shown ->
      Node.element "button" [ Attr.on_press Go ] [ Node.text label ]
  )

let check = Check.component ~props:[ "Go"; "" ] component
