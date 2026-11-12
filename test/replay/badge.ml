open Silica

type event = Remove | Cancel | Aged
type state = Badge of { fresh : bool; confirming : bool }

let event_name = function
  | Remove ->
      "remove"
  | Cancel ->
      "cancel"
  | Aged ->
      "aged"

let machine ~pauses =
  Machine.make ~id:"badge"
    ~init:(Badge { fresh = true; confirming = false })
    ~name:(fun _ -> "badge")
    ~states:[ "badge" ] ~event_name
    ~during:(fun (Badge { fresh; confirming }) ->
      ( if fresh && not (pauses && confirming) then
          [ Source.after 3000 Aged ]
        else
          []
      )
      @
      if confirming then
        [ Source.after 5000 Cancel ]
      else
        []
    )
    (fun (Badge badge) event ->
      match event with
      | Remove ->
          Machine.Go (Badge { badge with confirming = true })
      | Cancel ->
          Machine.Go (Badge { badge with confirming = false })
      | Aged ->
          Machine.Go (Badge { badge with fresh = false })
    )

let view (Badge { fresh; confirming }) =
  Node.element "div" []
    (( if fresh then
         [ Node.element "p" [] [ Node.text "New" ] ]
       else
         []
     )
    @
    if confirming then
      [ Node.element "button" [ Attr.on_press Cancel ] [ Node.text "Cancel" ] ]
    else
      [ Node.element "button" [ Attr.on_press Remove ] [ Node.text "Remove" ] ]
    )

let component = Component.make (machine ~pauses:true) view
let fixed = Component.make (machine ~pauses:false) view

let check_of component =
  Check.component
    ~properties:
      [
        Check.leads_to ~name:"a new badge gets old"
          ~from:(Check.state (fun (Badge { fresh; _ }) -> fresh))
          ~to_:(Check.state (fun (Badge { fresh; _ }) -> not fresh))
          ();
      ]
    component

let check = check_of component
let fixed_check = check_of fixed
