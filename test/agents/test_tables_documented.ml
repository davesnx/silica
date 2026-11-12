let lines path = In_channel.with_open_bin path In_channel.input_lines

let section label lines =
  let heading line =
    String.starts_with ~prefix:"{1" line || String.starts_with ~prefix:"{2" line
  in
  let rec from = function
    | line :: rest
      when String.starts_with ~prefix:("{1:" ^ label ^ " ") line
           || String.starts_with ~prefix:("{2:" ^ label ^ " ") line ->
        rest
    | _ :: rest ->
        from rest
    | [] ->
        failwith ("no section " ^ label)
  in
  let rec until = function
    | line :: _ when heading line ->
        []
    | line :: rest ->
        line :: until rest
    | [] ->
        []
  in
  until (from lines)

let first_cells lines =
  List.filter_map
    (fun line ->
      match String.split_on_char '|' line with
      | "" :: cell :: _ :: _ ->
          let cell = String.trim cell in
          let length = String.length cell in
          if length > 2 && cell.[0] = '[' && cell.[length - 1] = ']' then
            Some (String.sub cell 1 (length - 2))
          else
            None
      | _ ->
          None
    )
    lines

let role_constructors mli =
  let rec from = function
    | "type role =" :: rest ->
        rest
    | _ :: rest ->
        from rest
    | [] ->
        failwith "no type role in the interface"
  in
  let rec take = function
    | line :: rest when String.starts_with ~prefix:"  | " line ->
        String.trim (String.sub line 4 (String.length line - 4)) :: take rest
    | _ ->
        []
  in
  take (from (lines mli))

let failures = ref 0

let compare_sets path what ~documented ~expected =
  List.iter
    (fun name ->
      if not (List.mem name documented) then (
        incr failures;
        Printf.eprintf "%s: no row for the %s %s\n" path what name
      )
    )
    expected;
  List.iter
    (fun name ->
      if not (List.mem name expected) then (
        incr failures;
        Printf.eprintf "%s: a row for %s, which is not a %s\n" path name what
      )
    )
    documented

let () =
  let path = Sys.argv.(1) and mli = Sys.argv.(2) in
  let page = lines path in
  compare_sets path "rule"
    ~documented:(first_cells (section "rules" page))
    ~expected:(List.map (fun (rule : Check.rule) -> rule.id) Check.rules);
  let roles = first_cells (section "roles" page) in
  compare_sets path "role" ~documented:roles ~expected:(role_constructors mli);
  List.iter
    (fun role ->
      let name = String.lowercase_ascii role in
      match Silica.Accessibility.role_of_string name with
      | Some value when Silica.Accessibility.role_to_string value = name ->
          ()
      | _ ->
          incr failures;
          Printf.eprintf "%s: %s is not the ARIA role %S\n" path role name
    )
    roles;
  if !failures > 0 then exit 1
