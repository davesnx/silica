open Silica

module Counter = struct
  type state = Idle of int | Ticking of int [@@deriving state_shape]
  type event = Bump | Start | Tick

  let machine =
    Machine.make ~id:"counter" ~init:(Idle 0) ~shape:state_shape
      ~owns:(function Ticking _ -> [ Source.after 50 Tick ] | Idle _ -> [])
      (fun state event ->
        match (state, event) with
        | Idle count, Bump ->
            Machine.Go (Idle (count + 1))
        | Ticking count, Bump ->
            Machine.Go (Ticking (count + 1))
        | Idle count, Start ->
            Machine.Go (Ticking count)
        | Ticking count, Tick ->
            Machine.Go (Idle (count + 100))
        | (Idle _ | Ticking _), (Start | Tick) ->
            Machine.Ignore
      )

  let count = function Idle count | Ticking count -> count

  let view state : event Node.t =
    Node.element "div" []
      [
        Node.element "p"
          [ Attr.string "id" "label" ]
          [ Node.text "Counter, edited" ];
        Node.element "button"
          [ Attr.string "id" "bump"; Attr.on_press Bump ]
          [ Node.text (string_of_int (count state)) ];
        Node.element "button"
          [ Attr.string "id" "start"; Attr.on_press Start ]
          [ Node.text "Start" ];
      ]

  let component = Component.make machine view
end

module Draft = struct
  type state = Draft of string | Sent [@@deriving state_shape]
  type event = Typed of string | Key

  let[@warning "-11"] machine =
    Machine.make ~id:"draft" ~init:(Draft "") ~shape:state_shape
      ~owns:(fun _ -> [ Source.keys [ (Key.make "k", Key) ] ])
      (fun state event ->
        match (state, event) with
        | _, Typed text ->
            Machine.Go (Draft text)
        | Draft text, Key ->
            Machine.Go (Draft (text ^ "!"))
        | _, Key ->
            Machine.Ignore
      )

  let[@warning "-11"] text = function Draft text -> text | _ -> ""

  let view state : event Node.t =
    Node.element "div" []
      [
        Node.element "input"
          [
            Attr.string "id" "draft";
            Attr.string "value" (text state);
            Attr.on_input (fun text -> Typed text);
          ]
          [];
        Node.element "span"
          [ Attr.string "id" "echo" ]
          [ Node.text (text state) ];
      ]

  let component = Component.make machine view
end

type route = Home

let routes =
  Router.routes
    [ Router.route Route.root (fun () -> Home) (fun Home -> Some ()) ]

let app =
  Router.make routes ~render:(fun Home ->
      Node.element "main" []
        [ Component.node Counter.component; Component.node Draft.component ]
  )

let mount root = Silica_browser.mount_app ~root app
