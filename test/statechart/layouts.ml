let read text =
  match Silica.Spec.of_text text with
  | Ok read ->
      read
  | Error message ->
      failwith message

let all () =
  String.concat ""
    (List.map
       (fun (name, text) ->
         name ^ "\n"
         ^ Silica.Statechart_layout.to_string
             (Silica.Statechart_layout.layout (read text))
       )
       Golden_specs.all
    )
