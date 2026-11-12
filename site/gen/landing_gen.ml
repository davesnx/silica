let () =
  let report =
    Check.run
      (Check.component
         ~samples:
           [
             Check.answers ~rejected:[ "rejected" ] Landing_save.save
               [ "12:00" ];
           ]
         Landing_save.component
      )
  in
  match report.specs with
  | spec :: _ when Check.passed report ->
      print_string (Silica_json.Json.to_string (Silica.Spec.to_json spec))
  | _ ->
      Format.eprintf "landing_gen: %a@." Check.pp_report report;
      exit 1
