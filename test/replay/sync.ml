open Silica

type event = Sync | Answer of (int, string) Remote.answer
type state = Unsynced | Syncing | Synced | Offline

let name = function
  | Unsynced ->
      "unsynced"
  | Syncing ->
      "syncing"
  | Synced ->
      "synced"
  | Offline ->
      "offline"

let states = [ "unsynced"; "syncing"; "synced"; "offline" ]

let endpoint =
  Remote.declare ~path:"/api/sync" ~input:Codec.unit ~output:Codec.int
    ~rejected:Codec.string

let event_name = function
  | Sync ->
      "sync"
  | Answer (Ok _) ->
      "answer ok"
  | Answer _ ->
      "answer failed"

let machine =
  Machine.make ~id:"sync" ~init:Unsynced ~name ~states ~event_name
    ~final:(function Synced -> true | Unsynced | Syncing | Offline -> false)
    ~during:(function
      | Syncing ->
          [ Remote.call endpoint () (fun answer -> Answer answer) ]
      | Unsynced | Synced | Offline ->
          []
      )
    (fun state event ->
      match (state, event) with
      | (Unsynced | Offline), Sync ->
          Machine.Go Syncing
      | Syncing, Answer (Ok _) ->
          Machine.Go Synced
      | Syncing, Answer _ ->
          Machine.Go Offline
      | _ ->
          Machine.Ignore
    )

let component =
  Component.make machine (function
    | Unsynced | Offline ->
        Node.element "button" [ Attr.on_press Sync ] [ Node.text "Sync" ]
    | Syncing ->
        Node.text "syncing"
    | Synced ->
        Node.text "synced"
    )

let check =
  Check.component
    ~samples:[ Check.answers endpoint [ 1 ] ]
    ~properties:
      [
        Check.leads_to ~name:"sync leads to synced"
          ~from:(Check.event (( = ) Sync))
          ~to_:(Check.state_named "synced")
          ();
      ]
    component
