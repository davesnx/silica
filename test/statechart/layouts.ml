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
         let spec, labels = read text in
         name ^ "\n"
         ^ Silica.Statechart_layout.to_string
             (Silica.Statechart_layout.layout ~labels spec)
       )
       Golden_specs.all
    )
