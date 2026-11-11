open Silica

type event = Go [@@deriving event_shape]
type state = Shown [@@deriving state_shape]

let machine =
  let machine =
    Machine.make_with_props ~id:"labelled" ~init:Shown ~shape:state_shape
      ~events:event_shape (fun _ Shown Go -> Machine.Stay []
    )
  in
  Machine.register_codecs machine ~props:Codec.string ();
  machine

let component =
  Component.make_with_props machine (fun label Shown ->
      Node.element "button" [ Attr.on_press Go ] [ Node.text label ]
  )

let check = Check.component ~props:[ "Go"; "" ] component
