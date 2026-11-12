open Html_attributes

let attributes element =
  let seen = Hashtbl.create 64 in
  List.filter_map
    (function
      | (Attribute { jsxName; type_; _ } | Rich_attribute { jsxName; type_; _ })
        when not (Hashtbl.mem seen jsxName) ->
          Hashtbl.add seen jsxName ();
          Some (jsxName, type_)
      | Attribute _ | Rich_attribute _ | Event _ ->
          None
      )
    element.attributes

let html = Html.htmlElements ()
let all = html @ Html.svgElements

let typed elements =
  let groups = Hashtbl.create 64 in
  let order = ref [] in
  List.iter
    (fun element ->
      List.iter
        (fun ((_, kind) as key) ->
          if kind <> String then (
            if not (Hashtbl.mem groups key) then order := key :: !order;
            Hashtbl.replace groups key
              (element.tag
              :: Option.value (Hashtbl.find_opt groups key) ~default:[]
              )
          )
        )
        (attributes element)
    )
    elements;
  List.sort compare !order
  |> List.map (fun key -> (key, List.rev (Hashtbl.find groups key)))

let variant (option : polyvariant) = "`" ^ option.type_

let markdown_kind = function
  | String ->
      "`string`"
  | Int ->
      "`int`"
  | Bool ->
      "`bool`"
  | BooleanishString ->
      "`bool`, written as \"true\" or \"false\""
  | Polyvariant options ->
      String.concat ", "
        (List.map
           (fun (option : polyvariant) ->
             "`` " ^ variant option ^ " ``"
             ^
             if option.type_ = option.jsxName then
               ""
             else
               " (\"" ^ option.jsxName ^ "\")"
           )
           options
        )

let elements_text tags =
  let html_tags = List.map (fun element -> element.tag) html in
  let svg_tags = List.map (fun element -> element.tag) Html.svgElements in
  let covers group = List.for_all (fun tag -> List.mem tag tags) group in
  let rest =
    List.filter
      (fun tag ->
        not
          ((covers html_tags && List.mem tag html_tags)
          || (covers svg_tags && List.mem tag svg_tags)
          )
      )
      tags
  in
  String.concat ", "
    (( if covers html_tags then
         [ "every HTML element" ]
       else
         []
     )
    @ ( if covers svg_tags then
          [ "every SVG element" ]
        else
          []
      )
    @ List.map (fun tag -> "`" ^ tag ^ "`") rest
    )

let markdown () =
  print_string
    "# Attribute values in mlx\n\n\
     Generated from the attribute tables of the ppx (`ppx/html.ml`). An \
     attribute that is not listed here takes a `string`. An attribute with \
     variants takes one of them, such as ``type_=`number``; a string such as \
     `type_=\"number\"` does not compile. An optional attribute, written \
     `?name`, takes an option of the same type.\n\n\
     | Attribute | Elements | Value |\n\
     |---|---|---|\n";
  List.iter
    (fun ((name, kind), tags) ->
      Printf.printf "| `%s` | %s | %s |\n" name (elements_text tags)
        (markdown_kind kind)
    )
    (typed all)

let cells line =
  match String.split_on_char '|' line with
  | "" :: rest ->
      List.map String.trim
        (List.filteri (fun index _ -> index < List.length rest - 1) rest)
  | _ ->
      []

let section_rows label lines =
  let rec from = function
    | line :: rest when String.starts_with ~prefix:("{2:" ^ label ^ " ") line ->
        rest
    | _ :: rest ->
        from rest
    | [] ->
        failwith ("no section " ^ label)
  in
  let rec until = function
    | line :: _
      when String.starts_with ~prefix:"{1" line
           || String.starts_with ~prefix:"{2" line ->
        []
    | line :: rest ->
        line :: until rest
    | [] ->
        []
  in
  until (from lines)

let unbracket cell =
  let length = String.length cell in
  if length > 2 && cell.[0] = '[' && cell.[length - 1] = ']' then
    Some (String.sub cell 1 (length - 2))
  else
    None

let variants_in cell =
  let found = ref [] in
  String.iteri
    (fun index character ->
      if character = '`' then (
        let stop = ref (index + 1) in
        while
          !stop < String.length cell
          &&
          match cell.[!stop] with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
              true
          | _ ->
              false
        do
          incr stop
        done;
        found := String.sub cell index (!stop - index) :: !found
      )
    )
    cell;
  List.sort compare !found

let check path =
  let lines = In_channel.with_open_bin path In_channel.input_lines in
  let failures = ref 0 in
  let fail fmt =
    Printf.ksprintf
      (fun message ->
        incr failures;
        prerr_endline (path ^ ": " ^ message)
      )
      fmt
  in
  let rows =
    List.filter_map
      (fun line ->
        match cells line with
        | attribute :: tags :: value :: _ -> (
            match (unbracket attribute, unbracket tags) with
            | Some attribute, Some tags ->
                Some (attribute, String.split_on_char ' ' tags, value)
            | _ ->
                None
          )
        | _ ->
            None
      )
      (section_rows "attribute_types" lines)
  in
  if rows = [] then fail "no rows in the section attribute_types";
  List.iter
    (fun (attribute, tags, value) ->
      List.iter
        (fun tag ->
          match List.find_opt (fun element -> element.tag = tag) all with
          | None ->
              fail "%s: no element %s" attribute tag
          | Some element -> (
              match List.assoc_opt attribute (attributes element) with
              | None ->
                  fail "%s is not an attribute of %s" attribute tag
              | Some kind ->
                  let expected =
                    match kind with
                    | String ->
                        "[string]"
                    | Int ->
                        "[int]"
                    | Bool | BooleanishString ->
                        "[bool]"
                    | Polyvariant _ ->
                        ""
                  in
                  let agrees =
                    match kind with
                    | Polyvariant options ->
                        variants_in value
                        = List.sort compare (List.map variant options)
                    | _ ->
                        value = expected
                  in
                  if not agrees then
                    fail "%s on %s: the table says %s, the ppx takes %s"
                      attribute tag value
                      ( match kind with
                      | Polyvariant options ->
                          String.concat " " (List.map variant options)
                      | _ ->
                          expected
                      )
            )
        )
        tags
    )
    rows;
  if !failures > 0 then exit 1

let () =
  match Sys.argv with
  | [| _; "markdown" |] ->
      markdown ()
  | [| _; "check"; path |] ->
      check path
  | _ ->
      prerr_endline "usage: attributes_gen (markdown | check FILE.mld)";
      exit 2
