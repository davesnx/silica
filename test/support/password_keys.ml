open Silica

type state = { password : string; keys : int; sent : bool }
type event = Typed of string | Pressed of string

let name = function
  | { sent = true; _ } ->
      "sent"
  | { password = ""; keys = 0; _ } ->
      "empty"
  | { password = _; _ } ->
      "typed"

let machine =
  Machine.make ~id:"password-keys"
    ~init:{ password = ""; keys = 0; sent = false }
    ~name
    ~states:[ "empty"; "typed"; "sent" ]
    ~event_name:(function Typed _ -> "typed" | Pressed _ -> "pressed")
    (fun state event ->
      match (state, event) with
      | { sent = true; _ }, _ ->
          Machine.Ignore
      | _, Typed password ->
          Machine.Go { state with password }
      | _, Pressed "Enter" ->
          Machine.Go { state with sent = true }
      | _, Pressed _ ->
          Machine.Go { state with keys = state.keys + 1 }
    )

let state_codec =
  Codec.make
    ~encode:(fun { password; keys; sent } ->
      Codec.constructor "State"
        [
          Codec.string.encode password;
          Codec.int.encode keys;
          Codec.bool.encode sent;
        ]
    )
    ~decode:(fun json ->
      match Codec.constructor_of json with
      | Ok ("State", [ password; keys; sent ]) ->
          Result.bind (Codec.string.decode password) (fun password ->
              Result.bind (Codec.int.decode keys) (fun keys ->
                  Result.map
                    (fun sent -> { password; keys; sent })
                    (Codec.bool.decode sent)
              )
          )
      | _ ->
          Error "not a password keys state"
    )

let event_codec =
  Codec.make
    ~encode:(function
      | Typed text ->
          Codec.constructor "Typed" [ Codec.string.encode text ]
      | Pressed key ->
          Codec.constructor "Pressed" [ Codec.string.encode key ]
      )
    ~decode:(fun json ->
      match Codec.constructor_of json with
      | Ok ("Typed", [ text ]) ->
          Result.map (fun text -> Typed text) (Codec.string.decode text)
      | Ok ("Pressed", [ key ]) ->
          Result.map (fun key -> Pressed key) (Codec.string.decode key)
      | _ ->
          Error "not a password keys event"
    )

let register () =
  Machine.register_codecs machine ~state:state_codec ~event:event_codec ()

let pressed (key : Key.t) = Some (Pressed key.key)

let component =
  Component.make machine (fun state ->
      Node.element "form" []
        [
          Node.element "label"
            [ Attr.string "for" "password" ]
            [ Node.text "Password" ];
          Node.element "input"
            [
              Attr.string "id" "password";
              Attr.string "type" "password";
              Attr.string "value" state.password;
              Attr.on_input (fun text -> Typed text);
              Attr.on_key_down pressed;
              Attr.on_key_up pressed;
            ]
            [];
        ]
  )
