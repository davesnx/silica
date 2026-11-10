open Silica

let find : 'e. string -> (unit -> 'e Node.t) option = function
  | "examples/shared/tutorial_counter.mlx" ->
      Some (fun () -> Component.node Tutorial_counter.component)
  | "examples/shared/tutorial_greeting.mlx" ->
      Some (fun () -> Component.node Tutorial_greeting.component)
  | "examples/shared/tutorial_icon.mlx" ->
      Some (fun () -> Tutorial_icon.check ())
  | "examples/shared/doc_lamp.mlx" ->
      Some (fun () -> Component.node Doc_lamp.component)
  | "examples/shared/doc_debounce.mlx" ->
      Some (fun () -> Component.node Doc_debounce.component)
  | "examples/shared/doc_stepper.mlx" ->
      Some
        (fun () ->
          Component.node ~props:(Doc_stepper.Props "Adults")
            Doc_stepper.component
        )
  | "examples/shared/doc_party.mlx" ->
      Some (fun () -> Component.node Doc_party.component)
  | "examples/shared/doc_copy.mlx" ->
      Some (fun () -> Component.node Doc_copy.component)
  | "examples/shared/doc_greeting_status.mlx" ->
      Some (fun () -> Component.node Doc_greeting_status.component)
  | "examples/shared/doc_checkbox_story.mlx" ->
      Some
        (fun () ->
          Silica_ui.Story.render ~id:"docs-checkbox-story" Doc_stories.checkbox
            (Silica_ui.Story.defaults Doc_stories.checkbox)
        )
  | _ ->
      None
