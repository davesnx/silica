open Silica

let tagged constructor value : Silica_json.Json.t =
  Codec.constructor constructor [ value ]

let untagged json cases =
  Result.bind (Codec.constructor_of json) (function
    | constructor, [ value ] -> (
        match List.assoc_opt constructor cases with
        | Some decode ->
            decode value
        | None ->
            Error ("unknown constructor " ^ constructor)
      )
    | constructor, _ ->
        Error ("unknown constructor " ^ constructor)
    )

module Counter = struct
  type state = Idle of int | Ticking of int

  let name = function Idle _ -> "idle" | Ticking _ -> "ticking"

  let states = [ "idle"; "ticking" ]

  type event = Bump | Start | Tick

  let state_codec =
    Codec.make
      ~encode:(function
        | Idle count ->
            tagged "Idle" (Codec.int.encode count)
        | Ticking count ->
            tagged "Ticking" (Codec.int.encode count)
        )
      ~decode:(fun json ->
        untagged json
          [
            ( "Idle",
              fun count ->
                Result.map (fun count -> Idle count) (Codec.int.decode count)
            );
            ( "Ticking",
              fun count ->
                Result.map (fun count -> Ticking count) (Codec.int.decode count)
            );
          ]
      )

  let machine =
    Machine.make ~id:"counter" ~init:(Idle 0) ~name ~states
      ~during:(function Ticking _ -> [ Host.after 50 Tick ] | Idle _ -> [])
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
  type state = Writing of string

  let name = function Writing _ -> "writing"

  let states = [ "writing" ]

  type event = Typed of string | Key

  let state_codec =
    Codec.make
      ~encode:(function
        | Writing text -> tagged "Writing" (Codec.string.encode text)
        )
      ~decode:(fun json ->
        untagged json
          [
            ( "Writing",
              fun text ->
                Result.map (fun text -> Writing text) (Codec.string.decode text)
            );
          ]
      )

  let[@warning "-11"] machine =
    Machine.make ~id:"draft" ~init:(Writing "") ~name ~states
      ~during:(fun _ -> [ Host.keys [ (Key.make "k", Key) ] ])
      (fun state event ->
        match (state, event) with
        | _, Typed text ->
            Machine.Go (Writing text)
        | Writing text, Key ->
            Machine.Go (Writing (text ^ "!"))
        | _, Key ->
            Machine.Ignore
      )

  let[@warning "-11"] text = function Writing text -> text | _ -> ""

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

module Note = struct
  type state = Hidden | Shown

  let name = function Hidden -> "hidden" | Shown -> "shown"

  let states = [ "hidden"; "shown" ]

  type event = Toggle

  let machine =
    Machine.make ~id:"note" ~init:Hidden ~name ~states (fun state Toggle ->
        match state with
        | Hidden ->
            Machine.Go Shown
        | Shown ->
            Machine.Go Hidden
    )

  let view state : event Node.t =
    Node.element "button"
      [ Attr.string "id" "note"; Attr.on_press Toggle ]
      [ Node.text (name state) ]

  let component = Component.make machine view
end

type route = Home

let routes =
  Router.routes
    [ Router.route Route.root (fun () -> Home) (fun Home -> Some ()) ]

let app =
  Router.make routes ~render:(fun Home ->
      Node.element "main" []
        [
          Component.node Counter.component;
          Component.node Draft.component;
          Component.node Note.component;
        ]
  )

let mount root =
  Machine.register_codecs Counter.machine ~state:Counter.state_codec ();
  Machine.register_codecs Draft.machine ~state:Draft.state_codec ();
  Silica_browser.mount_app ~root:(Silica_browser.by_id root) app
