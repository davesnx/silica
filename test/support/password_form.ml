open Silica

type state = { password : string; sent : bool }
type event = Typed of string | Send

let name = function
  | { sent = true; _ } ->
      "sent"
  | { password = ""; _ } ->
      "empty"
  | { password = _; _ } ->
      "typed"

let machine =
  Machine.make ~id:"password-form"
    ~init:{ password = ""; sent = false }
    ~name
    ~states:[ "empty"; "typed"; "sent" ]
    ~event_name:(function Typed _ -> "typed" | Send -> "send")
    (fun state event ->
      match (state, event) with
      | { sent = false; _ }, Typed password ->
          Machine.Go { state with password }
      | { sent = false; password }, Send when password <> "" ->
          Machine.Go { state with sent = true }
      | _ ->
          Machine.Ignore
    )

let state_codec =
  Codec.make
    ~encode:(fun { password; sent } ->
      Codec.constructor "State"
        [ Codec.string.encode password; Codec.bool.encode sent ]
    )
    ~decode:(fun json ->
      match Codec.constructor_of json with
      | Ok ("State", [ password; sent ]) ->
          Result.bind (Codec.string.decode password) (fun password ->
              Result.map
                (fun sent -> { password; sent })
                (Codec.bool.decode sent)
          )
      | _ ->
          Error "not a password form state"
    )

let event_codec =
  Codec.make
    ~encode:(function
      | Typed text ->
          Codec.constructor "Typed" [ Codec.string.encode text ]
      | Send ->
          Codec.constructor "Send" []
      )
    ~decode:(fun json ->
      match Codec.constructor_of json with
      | Ok ("Typed", [ text ]) ->
          Result.map (fun text -> Typed text) (Codec.string.decode text)
      | Ok ("Send", []) ->
          Ok Send
      | _ ->
          Error "not a password form event"
    )

let register () =
  Machine.register_codecs machine ~state:state_codec ~event:event_codec ()

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
            ]
            [];
          ( if state.password = "" || state.sent then
              Node.list []
            else
              Node.element "button" [ Attr.on_press Send ] [ Node.text "Send" ]
          );
        ]
  )
