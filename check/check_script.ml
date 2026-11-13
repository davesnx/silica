type target = { role : string option; name : string; position : int option }

type action =
  | Press of target
  | Double_press of target
  | Submit of target
  | Focus of target
  | Blur of target
  | Hover of target
  | Leave of target
  | Input of target * string
  | Key_down of target * string
  | Key_up of target * string
  | Advance of int
  | Reply of { path : string; answer : Silica_json.Json.t }
  | Live_update of { topic : string; update : Silica_json.Json.t }
  | Set_props of int
  | Back
  | Forward
  | Open_url of string

type mount = Plain | With_props of int | Routed of string

type ending =
  | Handled of action
  | No_error
  | View_rule
  | Holds of { cycle_from : int option; start : int option }

type t = {
  component : string;
  check : string;
  mount : mount;
  actions : action list;
  rule : string;
  state : string;
  message : string;
  hint : string;
  ending : ending;
}

let module_of_id id =
  String.capitalize_ascii
    (String.map
       (fun character ->
         match character with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
             character
         | _ ->
             '_'
       )
       id
    )

let value_of_id id = String.uncapitalize_ascii (module_of_id id)

let contains text part =
  let length = String.length part in
  let rec go index =
    index + length <= String.length text
    && (String.sub text index length = part || go (index + 1))
  in
  go 0

let quoted text =
  if String.contains text '\n' || String.contains text '"' then
    let rec delimiter candidate =
      if contains text ("|" ^ candidate ^ "}") then
        delimiter (candidate ^ "x")
      else
        candidate
    in
    let name = delimiter "" in
    "{" ^ name ^ "|" ^ text ^ "|" ^ name ^ "}"
  else
    Printf.sprintf "%S" text

let role_constructor role = "Accessibility." ^ String.capitalize_ascii role

let element target =
  match (target.role, target.position) with
  | Some role, None when target.name = "" ->
      Printf.sprintf "Memory.by_role page %s" (role_constructor role)
  | Some role, None ->
      Printf.sprintf "Memory.by_role page %s ~name:%s" (role_constructor role)
        (quoted target.name)
  | Some role, Some position ->
      Printf.sprintf "List.nth (Memory.all_by_role page %s ~name:%s) %d"
        (role_constructor role) (quoted target.name) position
  | None, None ->
      Printf.sprintf "Memory.by_name page %s" (quoted target.name)
  | None, Some position ->
      Printf.sprintf "List.nth (Memory.all_by_name page %s) %d"
        (quoted target.name) position

let reply path answer =
  let call = Printf.sprintf "(Memory.pending_request page %S)" path in
  let text json = quoted (Silica_json.Json.to_string json) in
  match Silica.Codec.constructor_of answer with
  | Ok ("Ok", [ value ]) ->
      Printf.sprintf "Memory.reply %s %s" call (text value)
  | Ok ("Rejected", [ value ]) ->
      Printf.sprintf "Memory.reply ~status:422 %s %s" call (text value)
  | Ok ("Failed", [ error ]) -> (
      match Silica.Codec.constructor_of error with
      | Ok ("Http", [ status ]) ->
          Printf.sprintf "Memory.reply ~status:%s %s \"\""
            (Silica_json.Json.to_string status)
            call
      | Ok ("Network", [ reason ]) ->
          Printf.sprintf "Memory.network_error %s %s" call
            ( match Silica_json.Json.classify reason with
            | `String reason ->
                quoted reason
            | _ ->
                text reason
            )
      | _ ->
          Printf.sprintf "Memory.reply %s \"not json\"" call
    )
  | _ ->
      Printf.sprintf "Memory.reply %s %s" call (text answer)

let action_lines = function
  | Press target ->
      [ Printf.sprintf "Memory.press (%s)" (element target) ]
  | Double_press target ->
      [ Printf.sprintf "Memory.double_press (%s)" (element target) ]
  | Submit target ->
      [ Printf.sprintf "Memory.submit (%s)" (element target) ]
  | Focus target ->
      [ Printf.sprintf "Memory.focus (%s)" (element target) ]
  | Blur target ->
      [
        Printf.sprintf "Memory.focus (%s)" (element target); "Memory.blur page";
      ]
  | Hover target ->
      [ Printf.sprintf "Memory.hover (%s)" (element target) ]
  | Leave target ->
      [ Printf.sprintf "Memory.leave (%s)" (element target) ]
  | Input (target, text) ->
      [ Printf.sprintf "Memory.input (%s) %s" (element target) (quoted text) ]
  | Key_down (target, key) ->
      [
        Printf.sprintf "Memory.focus (%s)" (element target);
        Printf.sprintf "Memory.key page (Key.make %S)" key;
      ]
  | Key_up (target, key) ->
      [
        Printf.sprintf "Memory.focus (%s)" (element target);
        Printf.sprintf "Memory.key_up page (Key.make %S)" key;
      ]
  | Advance ms ->
      [ Printf.sprintf "Memory.advance page %d" ms ]
  | Reply { path; answer } ->
      [ reply path answer ]
  | Live_update { topic; update } ->
      [
        Printf.sprintf "Memory.live_update page ~topic:%s %s" (quoted topic)
          (quoted (Silica_json.Json.to_string update));
      ]
  | Set_props index ->
      [ Printf.sprintf "Memory.set_props page (props %d)" index ]
  | Back ->
      [ "Memory.back page" ]
  | Forward ->
      [ "Memory.forward page" ]
  | Open_url url ->
      [ Printf.sprintf "Memory.open_url page %S" url ]

let target_of = function
  | Press target
  | Double_press target
  | Submit target
  | Focus target
  | Blur target
  | Hover target
  | Leave target
  | Input (target, _)
  | Key_down (target, _)
  | Key_up (target, _) ->
      Some target
  | Advance _
  | Reply _
  | Live_update _
  | Set_props _
  | Back
  | Forward
  | Open_url _ ->
      None

let input_of = function
  | Press _ ->
      Some "Memory.press"
  | Double_press _ ->
      Some "Memory.double_press"
  | Submit _ ->
      Some "Memory.submit"
  | Focus _ ->
      Some "Memory.focus"
  | Hover _ ->
      Some "Memory.hover"
  | Leave _ ->
      Some "Memory.leave"
  | Blur _ ->
      Some
        "(fun element ->\n\
        \      Memory.focus element;\n\
        \      Memory.blur page\n\
        \    )"
  | Input (_, text) ->
      Some
        (Printf.sprintf "(fun element -> Memory.input element %s)" (quoted text))
  | Key_down (_, key) ->
      Some
        (Printf.sprintf
           "(fun element ->\n\
           \      Memory.focus element;\n\
           \      Memory.key page (Key.make %S)\n\
           \    )"
           key
        )
  | Key_up (_, key) ->
      Some
        (Printf.sprintf
           "(fun element ->\n\
           \      Memory.focus element;\n\
           \      Memory.key_up page (Key.make %S)\n\
           \    )"
           key
        )
  | Advance _
  | Reply _
  | Live_update _
  | Set_props _
  | Back
  | Forward
  | Open_url _ ->
      None

let literal text =
  let wrap line =
    List.fold_left
      (fun lines word ->
        match lines with
        | current :: rest when String.length current + String.length word < 66
          ->
            (current ^ " " ^ word) :: rest
        | lines ->
            word :: lines
      )
      []
      (String.split_on_char ' ' (String.escaped line))
    |> List.rev
  in
  match wrap text with
  | [] ->
      [ "\"\"" ]
  | lines ->
      let last = List.length lines - 1 in
      List.mapi
        (fun index line ->
          ( if index = 0 then
              "\""
            else
              " "
          )
          ^ line
          ^
          if index = last then
            "\""
          else
            " \\"
        )
        lines

let labelled label text =
  match literal text with
  | first :: rest ->
      ("~" ^ label ^ ":" ^ first)
      :: List.map
           (fun line -> String.make (String.length label + 2) ' ' ^ line)
           rest
  | [] ->
      []

let to_string script =
  let props_call index =
    Printf.sprintf "(Check.nth_props %s %d)" script.check index
  in
  let line_of action =
    match action with
    | Set_props index ->
        [
          Printf.sprintf "Check_expect.set_props %s page %d" script.check index;
        ]
    | _ ->
        action_lines action
  in
  let broken =
    [
      "let broken =";
      Printf.sprintf "  Check_expect.rule %S ~state:%S" script.rule script.state;
    ]
    @ List.map (fun line -> "    " ^ line) (labelled "message" script.message)
    @ List.map (fun line -> "    " ^ line) (labelled "hint" script.hint)
    @ [ "" ]
  in
  let view = [] in
  let on_error =
    match script.ending with
    | No_error ->
        " ~on_error:(Check_expect.on_error broken)"
    | Handled _ | View_rule | Holds _ ->
        ""
  in
  let mount =
    match script.mount with
    | Plain ->
        Printf.sprintf "let page = Memory.mount%s %s in" on_error
          script.component
    | With_props index ->
        Printf.sprintf
          "let page =\n\
          \    Memory.mount_with_props%s\n\
          \      ~props:%s\n\
          \      %s\n\
          \  in"
          on_error (props_call index) script.component
    | Routed url ->
        Printf.sprintf "let page = Memory.mount_app%s ~url:%S %s in" on_error
          url script.component
  in
  let steps action = List.map (fun line -> line ^ ";") (line_of action) in
  let ending =
    match script.ending with
    | Handled action -> (
        match (target_of action, input_of action) with
        | Some ({ role = Some role; _ } as target), Some input ->
            let role = role_constructor role in
            let call =
              Printf.sprintf "Check_expect.handles broken%s%s page %s"
                ( match target.position with
                | None | Some 0 ->
                    ""
                | Some position ->
                    Printf.sprintf " ~position:%d" position
                )
                ( if target.name = "" then
                    ""
                  else
                    " ~name:" ^ quoted target.name
                )
                role
            in
            if String.contains input '\n' then
              [ call; "  " ^ input ]
            else
              [ call ^ " " ^ input ]
        | _ ->
            [ "Check_expect.handles_input broken page (fun () ->" ]
            @ List.map (fun line -> "    " ^ line) (line_of action)
            @ [ ")" ]
      )
    | No_error ->
        [ "ignore page" ]
    | View_rule ->
        [ "Check_expect.view_passes broken page" ]
    | Holds _ ->
        []
  in
  let body =
    match (script.ending, List.rev (List.concat_map steps script.actions)) with
    | No_error, [] ->
        [ mount; "ignore page" ]
    | No_error, last :: earlier ->
        (mount :: List.rev earlier)
        @ [ String.sub last 0 (String.length last - 1) ]
    | Holds { cycle_from; start }, steps ->
        let options =
          String.concat ""
            (List.filter_map Fun.id
               [
                 Option.map (Printf.sprintf " ~cycle_from:%d") cycle_from;
                 Option.map (Printf.sprintf " ~start:%d") start;
               ]
            )
        in
        let steps =
          match List.rev steps with
          | [] ->
              [ "()" ]
          | steps -> (
              match List.rev steps with
              | last :: earlier ->
                  List.rev
                    (String.sub last 0 (String.length last - 1) :: earlier)
              | [] ->
                  []
            )
        in
        [
          mount;
          Printf.sprintf "Check_expect.holds broken%s %s page (fun () ->"
            options script.check;
        ]
        @ List.map (fun line -> "    " ^ line) steps
        @ [ ")" ]
    | _, steps ->
        (mount :: List.rev steps) @ ending
  in
  let main = [ "let () =" ] @ List.map (fun line -> "  " ^ line) body in
  let opens =
    if
      List.exists
        (fun line -> contains line "Accessibility." || contains line "Key.")
        main
    then
      [ "open Silica" ]
    else
      []
  in
  String.concat "\n"
    (opens @ [ "module Memory = Silica_memory"; "" ] @ broken @ view @ main)
  ^ "\n"
