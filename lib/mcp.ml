module Json = Silica_json.Json

let text value = Json.declassify (`String value)
let assoc fields = Json.declassify (`Assoc fields)
let boolean value = Json.declassify (`Bool value)

let duplicate names =
  let rec find seen = function
    | [] ->
        None
    | name :: rest ->
        if List.mem name seen then
          Some name
        else
          find (name :: seen) rest
  in
  find [] names

module Input = struct
  type 'a t =
    | Value of {
        schema : (string * Json.t) list;
        decode : Json.t -> ('a, string) result;
      }
    | Object of {
        properties : (string * Json.t) list;
        required : string list;
        decode : (string * Json.t) list -> ('a, string) result;
      }

  let object_schema properties required =
    ("type", text "object")
    ::
    ( match properties with
    | [] ->
        []
    | _ :: _ ->
        [ ("properties", assoc properties) ]
    )
    @ ( match required with
      | [] ->
          []
      | _ :: _ ->
          [ ("required", Json.declassify (`List (List.map text required))) ]
      )
    @ [ ("additionalProperties", boolean false) ]

  let schema_fields = function
    | Value { schema; _ } ->
        schema
    | Object { properties; required; _ } ->
        object_schema properties required

  let schema input = assoc (schema_fields input)

  let kind json =
    match Json.classify json with
    | `Null ->
        "null"
    | `Bool _ ->
        "a boolean"
    | `Int _ | `Float _ ->
        "a number"
    | `String _ ->
        "a string"
    | `List _ ->
        "an array"
    | `Assoc _ ->
        "an object"

  let expected what json =
    Error (Printf.sprintf "expected %s, got %s" what (kind json))

  let decode input json =
    match input with
    | Value { decode; _ } ->
        decode json
    | Object { properties; decode; _ } -> (
        match Json.classify json with
        | `Assoc fields -> (
            match
              List.find_opt
                (fun (key, _) -> not (List.mem_assoc key properties))
                fields
            with
            | Some (key, _) ->
                Error ("unknown field " ^ key)
            | None ->
                decode fields
          )
        | `Null | `Bool _ | `Int _ | `Float _ | `String _ | `List _ ->
            expected "an object" json
      )

  let none =
    Object { properties = []; required = []; decode = (fun _ -> Ok ()) }

  let string =
    Value
      {
        schema = [ ("type", text "string") ];
        decode =
          (fun json ->
            match Json.classify json with
            | `String text ->
                Ok text
            | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ ->
                expected "a string" json
          );
      }

  let int =
    Value
      {
        schema =
          [
            ("type", text "integer");
            ("minimum", Json.declassify (`Int min_int));
            ("maximum", Json.declassify (`Int max_int));
          ];
        decode =
          (fun json ->
            match Json.classify json with
            | `Int value ->
                Ok value
            | `Float value
              when Float.is_integer value
                   && value >= Float.of_int min_int
                   && value < -.Float.of_int min_int ->
                Ok (Float.to_int value)
            | `Float _ ->
                expected
                  (Printf.sprintf "an integer from %d to %d" min_int max_int)
                  json
            | `Null | `Bool _ | `String _ | `List _ | `Assoc _ ->
                expected "an integer" json
          );
      }

  let bool =
    Value
      {
        schema = [ ("type", text "boolean") ];
        decode =
          (fun json ->
            match Json.classify json with
            | `Bool value ->
                Ok value
            | `Null | `Int _ | `Float _ | `String _ | `List _ | `Assoc _ ->
                expected "a boolean" json
          );
      }

  let enum cases =
    let names = List.map fst cases in
    Option.iter
      (fun name ->
        invalid_arg
          (Printf.sprintf "Silica.Mcp.Input.enum: %S is twice in the list" name)
      )
      (duplicate names);
    let one_of = "one of " ^ String.concat ", " names in
    Value
      {
        schema =
          [
            ("type", text "string");
            ("enum", Json.declassify (`List (List.map text names)));
          ];
        decode =
          (fun json ->
            match Json.classify json with
            | `String text -> (
                match List.assoc_opt text cases with
                | Some value ->
                    Ok value
                | None ->
                    Error (Printf.sprintf "expected %s, got %S" one_of text)
              )
            | `Null | `Bool _ | `Int _ | `Float _ | `List _ | `Assoc _ ->
                expected one_of json
          );
      }

  let property ?description input =
    assoc
      (schema_fields input
      @
      match description with
      | Some description ->
          [ ("description", text description) ]
      | None ->
          []
      )

  let in_field name result =
    Result.map_error (fun reason -> name ^ ": " ^ reason) result

  let field name ?description input =
    Object
      {
        properties = [ (name, property ?description input) ];
        required = [ name ];
        decode =
          (fun fields ->
            match List.assoc_opt name fields with
            | Some value ->
                in_field name (decode input value)
            | None ->
                Error ("missing field " ^ name)
          );
      }

  let optional name ?description input =
    Object
      {
        properties = [ (name, property ?description input) ];
        required = [];
        decode =
          (fun fields ->
            match List.assoc_opt name fields with
            | Some value ->
                in_field name (Result.map Option.some (decode input value))
            | None ->
                Ok None
          );
      }

  let both first second =
    match (first, second) with
    | Object first, Object second ->
        Option.iter
          (fun name ->
            invalid_arg ("Silica.Mcp.Input.both: two fields are named " ^ name)
          )
          (duplicate (List.map fst (first.properties @ second.properties)));
        Object
          {
            properties = first.properties @ second.properties;
            required = first.required @ second.required;
            decode =
              (fun fields ->
                Result.bind (first.decode fields) (fun a ->
                    Result.map (fun b -> (a, b)) (second.decode fields)
                )
              );
          }
    | Value _, (Value _ | Object _) | Object _, Value _ ->
        invalid_arg
          "Silica.Mcp.Input.both: both arguments must be objects, made with \
           none, field, optional or both"

  let map f = function
    | Value { schema; decode } ->
        Value { schema; decode = (fun json -> Result.map f (decode json)) }
    | Object { properties; required; decode } ->
        Object
          {
            properties;
            required;
            decode = (fun fields -> Result.map f (decode fields));
          }

  let fields = function
    | Value _ ->
        None
    | Object { properties; _ } ->
        Some (List.map fst properties)
end

type annotations = {
  read_only : bool option;
  destructive : bool option;
  idempotent : bool option;
  open_world : bool option;
}

let annotations ?read_only ?destructive ?idempotent ?open_world () =
  { read_only; destructive; idempotent; open_world }

let no_annotations = annotations ()

type 'a t = {
  name : string;
  title : string option;
  description : string;
  annotations : annotations;
  input : 'a Input.t;
}

let valid_name name =
  let length = String.length name in
  length >= 1 && length <= 128
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' ->
             true
         | _ ->
             false
         )
       name

let make ~name ?title ~description ?(annotations = no_annotations) input =
  if not (valid_name name) then
    invalid_arg
      (Printf.sprintf
         "Silica.Mcp.make: the tool name %S is not 1 to 128 characters of A-Z, \
          a-z, 0-9, _, - and ."
         name
      );
  ( match Input.fields input with
  | Some _ ->
      ()
  | None ->
      invalid_arg
        (Printf.sprintf
           "Silica.Mcp.make: the input of the tool %s is not an object; make \
            it with Input.none, Input.field, Input.optional or Input.both"
           name
        )
  );
  { name; title; description; annotations; input }

let name tool = tool.name
let input tool = tool.input

let definition tool =
  let hint key = Option.map (fun value -> (key, boolean value)) in
  let hints =
    List.filter_map Fun.id
      [
        hint "readOnlyHint" tool.annotations.read_only;
        hint "destructiveHint" tool.annotations.destructive;
        hint "idempotentHint" tool.annotations.idempotent;
        hint "openWorldHint" tool.annotations.open_world;
      ]
  in
  assoc
    (List.filter_map Fun.id
       [
         Some ("name", text tool.name);
         Option.map (fun title -> ("title", text title)) tool.title;
         Some ("description", text tool.description);
         Some ("inputSchema", Input.schema tool.input);
         ( match hints with
         | [] ->
             None
         | _ :: _ ->
             Some ("annotations", assoc hints)
         );
       ]
    )

type 'e binding = Binding : 'a t * ('a -> 'e) -> 'e binding

let on tool event = Binding (tool, event)

type 'e call = { event : 'e; instance : string option }

type publication =
  | Publication : {
      machine : ('p, 's, 'e) Machine.t;
      tools : 'e call t list;
    }
      -> publication

let tools machine bindings =
  let instance =
    Input.optional "instance"
      ~description:
        (Printf.sprintf
           "The instance path of the running instance of %s that gets the \
            event. Leave it out when one instance runs."
           (Machine.id machine)
        )
      Input.string
  in
  let bind (Binding (tool, event)) =
    if List.mem "instance" (Option.value (Input.fields tool.input) ~default:[])
    then
      invalid_arg
        (Printf.sprintf
           "Silica.Mcp.tools: the tool %s has a field instance, which every \
            tool of a machine gets"
           tool.name
        );
    {
      tool with
      input =
        Input.map
          (fun (value, instance) -> { event = event value; instance })
          (Input.both tool.input instance);
    }
  in
  let tools = List.map bind bindings in
  Option.iter
    (fun name ->
      invalid_arg
        (Printf.sprintf "Silica.Mcp.tools: two tools of %s are named %s"
           (Machine.id machine) name
        )
    )
    (duplicate (List.map (fun tool -> tool.name) tools));
  Publication { machine; tools }
