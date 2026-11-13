open Silica

type state = Editing of int | Sent
type event = Newline | Submit

let machine =
  Machine.make ~id:"composer-keys" ~init:(Editing 0)
    ~name:(function Editing _ -> "editing" | Sent -> "sent")
    ~states:[ "editing"; "sent" ]
    (fun state event ->
      match (state, event) with
      | Editing lines, Newline ->
          Machine.Go (Editing (lines + 1))
      | Editing _, Submit ->
          Machine.Go Sent
      | Sent, (Newline | Submit) ->
          Machine.Ignore
    )

let state_codec =
  Codec.make
    ~encode:(function
      | Editing lines ->
          Codec.constructor "Editing" [ Codec.int.encode lines ]
      | Sent ->
          Codec.constructor "Sent" []
      )
    ~decode:(fun json ->
      match Codec.constructor_of json with
      | Ok ("Editing", [ lines ]) ->
          Result.map (fun lines -> Editing lines) (Codec.int.decode lines)
      | Ok ("Sent", []) ->
          Ok Sent
      | _ ->
          Error "not a composer state"
    )

let event_codec =
  Codec.make
    ~encode:(function
      | Newline ->
          Codec.constructor "Newline" []
      | Submit ->
          Codec.constructor "Submit" []
      )
    ~decode:(fun json ->
      match Codec.constructor_of json with
      | Ok ("Newline", []) ->
          Ok Newline
      | Ok ("Submit", []) ->
          Ok Submit
      | _ ->
          Error "not a composer event"
    )

let register () =
  Machine.register_codecs machine ~state:state_codec ~event:event_codec ()

let component =
  Component.make machine (fun _ ->
      Node.element "textarea"
        [
          Attr.string "aria-label" "Message";
          Attr.on_key_down (fun (key : Key.t) ->
              match (key.key, key.shift) with
              | "Enter", true ->
                  Some Newline
              | "Enter", false ->
                  Some Submit
              | _ ->
                  None
          );
        ]
        []
  )

let check =
  Check.component
    ~keys:[ Key.enter; Key.make ~shift:true "Enter" ]
    ~properties:
      [
        Check.always ~name:"no new line"
          (Check.state (function Editing lines -> lines = 0 | Sent -> true));
      ]
    component
