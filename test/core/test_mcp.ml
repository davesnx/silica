open Silica
module Json = Silica_json.Json
module Input = Mcp.Input

let json text =
  match Json.of_string text with
  | Ok json ->
      json
  | Error reason ->
      Alcotest.failf "bad JSON in the test %s: %s" text reason

let schema_is expected input =
  Alcotest.(check string) "schema" expected (Json.to_string (Input.schema input))

let number json =
  match Json.classify json with
  | `Int value ->
      Some (Float.of_int value)
  | `Float value ->
      Some value
  | `Null | `Bool _ | `String _ | `List _ | `Assoc _ ->
      None

let fields schema =
  match Json.classify schema with
  | `Assoc fields ->
      fields
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
      Alcotest.failf "a schema that is not an object: %s" (Json.to_string schema)

let strings json =
  match Json.classify json with
  | `List items ->
      List.map
        (fun item ->
          match Json.classify item with
          | `String text ->
              text
          | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ ->
              Alcotest.failf "not a string: %s" (Json.to_string item)
        )
        items
  | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `Assoc _ ->
      Alcotest.failf "not a list: %s" (Json.to_string json)

(* JSON Schema 2020-12 for the keywords that Input writes; another keyword
   fails the test until it has a rule here. *)
let rec validates schema json =
  let keywords = fields schema in
  let object_fields =
    match Json.classify json with
    | `Assoc fields ->
        Some fields
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
        None
  in
  let properties =
    Option.fold ~none:[] ~some:fields (List.assoc_opt "properties" keywords)
  in
  List.for_all
    (fun (keyword, value) ->
      match (keyword, object_fields) with
      | "type", _ -> (
          match (Json.classify value, Json.classify json) with
          | `String "object", `Assoc _
          | `String "string", `String _
          | `String "boolean", `Bool _
          | `String "integer", `Int _ ->
              true
          | `String "integer", `Float value ->
              Float.is_integer value
          | _ ->
              false
        )
      | "properties", Some fields ->
          List.for_all
            (fun (key, value) ->
              match List.assoc_opt key properties with
              | Some property ->
                  validates property value
              | None ->
                  true
            )
            fields
      | "required", Some fields ->
          List.for_all (fun name -> List.mem_assoc name fields) (strings value)
      | "additionalProperties", Some fields ->
          Json.to_string value = "false"
          && List.for_all (fun (key, _) -> List.mem_assoc key properties) fields
      | "enum", _ ->
          List.mem (Json.to_string json)
            (List.map
               (fun case -> Json.to_string (Json.declassify (`String case)))
               (strings value)
            )
      | "minimum", _ ->
          Option.fold ~none:true
            ~some:(fun x -> Option.get (number value) <= x)
            (number json)
      | "maximum", _ ->
          Option.fold ~none:true
            ~some:(fun x -> x <= Option.get (number value))
            (number json)
      | ("properties" | "required" | "additionalProperties"), None
      | "description", _ ->
          true
      | other, _ ->
          Alcotest.failf "the test validator has no rule for %s" other
    )
    keywords

let agree name input cases =
  List.iter
    (fun (text, accepted) ->
      let value = json text in
      let decoded = Input.decode input value in
      Alcotest.(check bool)
        (Printf.sprintf "%s: the schema on %s" name text)
        accepted
        (validates (Input.schema input) value);
      Alcotest.(check bool)
        (Printf.sprintf "%s: decode of %s (%s)" name text
           (match decoded with Ok _ -> "ok" | Error reason -> reason)
        )
        accepted (Result.is_ok decoded)
    )
    cases

let decoded_error input text =
  match Input.decode input (json text) with
  | Ok _ ->
      Alcotest.failf "%s decodes" text
  | Error reason ->
      reason

type priority = Low | High

let draft =
  Input.(
    both
      (field "title" ~description:"The title" string)
      (both
         (optional "priority" (enum [ ("low", Low); ("high", High) ]))
         (both (optional "count" int) (field "done" bool))
      )
  )

let none_schema () =
  schema_is {|{"type":"object","additionalProperties":false}|} Input.none

let field_schema () =
  schema_is
    {|{"type":"object","properties":{"title":{"type":"string","description":"The title"},"priority":{"type":"string","enum":["low","high"]},"count":{"type":"integer","minimum":-4611686018427387904,"maximum":4611686018427387903},"done":{"type":"boolean"}},"required":["title","done"],"additionalProperties":false}|}
    draft

let nested_schema () =
  schema_is
    {|{"type":"object","properties":{"point":{"type":"object","properties":{"x":{"type":"integer","minimum":-4611686018427387904,"maximum":4611686018427387903}},"required":["x"],"additionalProperties":false,"description":"Where"}},"required":["point"],"additionalProperties":false}|}
    Input.(field "point" ~description:"Where" (field "x" int))

let none_agrees () =
  agree "none" Input.none
    [
      ("{}", true);
      ({|{"a":1}|}, false);
      ("null", false);
      ("[]", false);
      ({|""|}, false);
    ]

let fields_agree () =
  agree "draft" draft
    [
      ({|{"title":"A","done":true}|}, true);
      ({|{"title":"A","done":false,"priority":"high","count":3}|}, true);
      ({|{"title":"A","done":false,"count":3.0}|}, true);
      ({|{"title":"A"}|}, false);
      ({|{"done":true}|}, false);
      ({|{"title":"A","done":true,"other":1}|}, false);
      ({|{"title":1,"done":true}|}, false);
      ({|{"title":"A","done":"yes"}|}, false);
      ({|{"title":"A","done":true,"priority":"urgent"}|}, false);
      ({|{"title":"A","done":true,"priority":null}|}, false);
      ({|{"title":"A","done":true,"count":1.5}|}, false);
      ({|{"title":"A","done":true,"count":1e300}|}, false);
      ({|{"title":"A","done":true,"count":"3"}|}, false);
      ("[]", false);
    ]

let nested_agrees () =
  agree "point"
    Input.(field "point" (field "x" int))
    [
      ({|{"point":{"x":1}}|}, true);
      ({|{"point":{"x":1,"y":2}}|}, false);
      ({|{"point":{}}|}, false);
      ({|{"point":1}|}, false);
    ]

let decode_values () =
  match
    Input.decode draft
      (json {|{"title":"A","done":true,"priority":"high","count":2.0}|})
  with
  | Ok ("A", (Some High, (Some 2, true))) ->
      ()
  | Ok _ ->
      Alcotest.fail "the wrong value"
  | Error reason ->
      Alcotest.fail reason

let decode_reasons () =
  let check expected text =
    Alcotest.(check string) text expected (decoded_error draft text)
  in
  check "missing field title" {|{"done":true}|};
  check "unknown field other" {|{"title":"A","done":true,"other":1}|};
  check "title: expected a string, got a number" {|{"title":1,"done":true}|};
  check {|priority: expected one of low, high, got "urgent"|}
    {|{"title":"A","done":true,"priority":"urgent"}|};
  check
    "count: expected an integer from -4611686018427387904 to \
     4611686018427387903, got a number"
    {|{"title":"A","done":true,"count":1.5}|};
  check "expected an object, got an array" "[]";
  Alcotest.(check string)
    "nested" "point: x: expected an integer, got a string"
    (decoded_error Input.(field "point" (field "x" int)) {|{"point":{"x":"1"}}|})

let raises what f =
  match f () with
  | _ ->
      Alcotest.failf "%s does not raise" what
  | exception Invalid_argument _ ->
      ()

let refused_inputs () =
  raises "both with a string" (fun () -> Input.(both string none));
  raises "both with two fields named a" (fun () ->
      Input.(both (field "a" int) (optional "a" string))
  );
  raises "enum with a name twice" (fun () -> Input.enum [ ("a", 1); ("a", 2) ])

let tool name = Mcp.make ~name ~description:"A tool" Input.none

let names () =
  List.iter
    (fun name -> Alcotest.(check string) name name (Mcp.name (tool name)))
    [ "edit.save"; "DATA_EXPORT_v2"; "a-b"; "x"; String.make 128 'a' ];
  List.iter
    (fun name ->
      raises (Printf.sprintf "the name %S" name) (fun () -> tool name)
    )
    [ ""; String.make 129 'a'; "a b"; "a,b"; "caf\xc3\xa9"; "a/b" ];
  raises "an input that is not an object" (fun () ->
      Mcp.make ~name:"bare" ~description:"A tool" Input.string
  )

let machine =
  Machine.make ~id:"switch" ~init:false ~name:string_of_bool (fun on () ->
      Go (not on)
  )

let definition () =
  let flip =
    Mcp.make ~name:"switch.flip" ~title:"Flip" ~description:"Flip the switch"
      ~annotations:(Mcp.annotations ~read_only:false ~idempotent:false ())
      Input.none
  in
  Alcotest.(check string)
    "definition"
    {|{"name":"switch.flip","title":"Flip","description":"Flip the switch","inputSchema":{"type":"object","additionalProperties":false},"annotations":{"readOnlyHint":false,"idempotentHint":false}}|}
    (Json.to_string (Mcp.definition flip));
  Alcotest.(check string)
    "no annotations"
    {|{"name":"x","description":"A tool","inputSchema":{"type":"object","additionalProperties":false}}|}
    (Json.to_string (Mcp.definition (tool "x")));
  match Mcp.tools machine [ Mcp.on flip Fun.id ] with
  | Mcp.Publication { tools = [ bound ]; _ } -> (
      Alcotest.(check string)
        "the tool of the machine takes instance"
        {|{"name":"switch.flip","title":"Flip","description":"Flip the switch","inputSchema":{"type":"object","properties":{"instance":{"type":"string","description":"The instance path of the running instance of switch that gets the event. Leave it out when one instance runs."}},"additionalProperties":false},"annotations":{"readOnlyHint":false,"idempotentHint":false}}|}
        (Json.to_string (Mcp.definition bound));
      match Input.decode (Mcp.input bound) (json {|{"instance":"switch"}|}) with
      | Ok { Mcp.instance; _ } ->
          Alcotest.(check (option string)) "instance" (Some "switch") instance
      | Error reason ->
          Alcotest.fail reason
    )
  | Mcp.Publication _ ->
      Alcotest.fail "one tool"

let refused_publications () =
  raises "two tools with one name" (fun () ->
      Mcp.tools machine [ Mcp.on (tool "a") Fun.id; Mcp.on (tool "a") Fun.id ]
  );
  raises "a tool with its own field instance" (fun () ->
      Mcp.tools machine
        [
          Mcp.on
            (Mcp.make ~name:"a" ~description:"A tool"
               Input.(field "instance" string)
            )
            ignore;
        ]
  )

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "mcp"
    [
      ( "input",
        [
          case "none is the closed empty object" none_schema;
          case "fields give one object schema" field_schema;
          case "a field holds a nested object" nested_schema;
          case "none: the schema and decode agree" none_agrees;
          case "fields: the schema and decode agree" fields_agree;
          case "nested: the schema and decode agree" nested_agrees;
          case "decode gives the values" decode_values;
          case "decode names the field that fails" decode_reasons;
          case "both and enum refuse what has no schema" refused_inputs;
        ]
      );
      ( "tools",
        [
          case "names are 1 to 128 of A-Z a-z 0-9 _ - ." names;
          case "a definition is the tool of tools/list" definition;
          case "a publication refuses a name twice and its own instance"
            refused_publications;
        ]
      );
    ]
