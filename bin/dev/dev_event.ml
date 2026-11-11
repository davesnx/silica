module Json = Silica_json.Json

type location = {
  file : string;
  line : int;
  column : int;
  end_line : int;
  end_column : int;
  excerpt : (int * string) list;
}

type severity = Error | Warning
type error = {
  location : location option;
  severity : severity;
  message : string;
}
type swap = { from : string; target : string }
type reload = Page | Stylesheets of swap list

type t =
  | Listening of { url : string }
  | Build_started
  | Build_progress of { complete : int; remaining : int }
  | Build_failed of error list
  | Build_ok
  | Server_started of { pid : int; milliseconds : int; restart : bool }
  | Server_crashed of { code : string; output : string }
  | Reload of reload

let severity_name = function Error -> "error" | Warning -> "warning"

let object_ fields =
  Json.declassify
    (`Assoc
       (List.map (fun (name, value) -> (name, Json.declassify value)) fields)
    )

let error_to_json error =
  let location =
    match error.location with
    | None ->
        []
    | Some location ->
        [
          ("file", `String location.file);
          ("line", `Int location.line);
          ("col", `Int location.column);
          ("end_line", `Int location.end_line);
          ("end_col", `Int location.end_column);
        ]
  in
  let excerpt =
    match error.location with
    | None ->
        []
    | Some location ->
        [
          ( "excerpt",
            `List
              (List.map
                 (fun (number, text) ->
                   Json.declassify
                     (`List
                        [
                          Json.declassify (`Int number);
                          Json.declassify (`String text);
                        ]
                     )
                 )
                 location.excerpt
              )
          );
        ]
  in
  object_
    (location
    @ [
        ("severity", `String (severity_name error.severity));
        ("message", `String error.message);
      ]
    @ excerpt
    )

let to_json = function
  | Listening { url } ->
      object_ [ ("t", `String "dev"); ("url", `String url) ]
  | Build_started ->
      object_ [ ("t", `String "build"); ("status", `String "started") ]
  | Build_progress { complete; remaining } ->
      object_
        [
          ("t", `String "progress");
          ("complete", `Int complete);
          ("remaining", `Int remaining);
        ]
  | Build_failed errors ->
      object_
        [
          ("t", `String "build");
          ("status", `String "failed");
          ("errors", `List (List.map error_to_json errors));
        ]
  | Build_ok ->
      object_ [ ("t", `String "build"); ("status", `String "ok") ]
  | Server_started { pid; milliseconds; restart } ->
      object_
        [
          ("t", `String "server");
          ( "status",
            `String
              ( if restart then
                  "restarted"
                else
                  "started"
              )
          );
          ("pid", `Int pid);
          ("ms", `Int milliseconds);
        ]
  | Server_crashed { code; output } ->
      object_
        [
          ("t", `String "server");
          ("status", `String "crashed");
          ("code", `String code);
          ("output", `String output);
        ]
  | Reload Page ->
      object_ [ ("t", `String "reload"); ("what", `String "page") ]
  | Reload (Stylesheets swaps) ->
      object_
        [
          ("t", `String "reload");
          ("what", `String "css");
          ( "css",
            `List
              (List.map
                 (fun swap ->
                   object_
                     [
                       ("from", `String swap.from); ("to", `String swap.target);
                     ]
                 )
                 swaps
              )
          );
        ]

let to_string event = Json.to_string (to_json event)

let error_to_text error =
  let head =
    match error.location with
    | None ->
        severity_name error.severity ^ ": " ^ error.message
    | Some location ->
        Printf.sprintf "%s:%d:%d: %s: %s" location.file location.line
          location.column
          (severity_name error.severity)
          error.message
  in
  let excerpt =
    match error.location with
    | None ->
        []
    | Some location ->
        List.map
          (fun (number, text) ->
            Printf.sprintf "  %s %4d | %s"
              ( if number = location.line then
                  ">"
                else
                  " "
              )
              number text
          )
          location.excerpt
  in
  String.concat "\n" (head :: excerpt)

let to_text = function
  | Listening { url } ->
      Some ("silica dev: " ^ url)
  | Build_started ->
      Some "build started"
  | Build_progress _ ->
      None
  | Build_failed errors ->
      Some
        (String.concat "\n"
           (Printf.sprintf "build failed: %d %s" (List.length errors)
              ( if List.length errors = 1 then
                  "error"
                else
                  "errors"
              )
           :: List.map error_to_text errors
           )
        )
  | Build_ok ->
      Some "build ok"
  | Server_started { pid; milliseconds; restart } ->
      Some
        (Printf.sprintf "server %s in %d ms (pid %d)"
           ( if restart then
               "restarted"
             else
               "started"
           )
           milliseconds pid
        )
  | Server_crashed { code; output } ->
      Some (Printf.sprintf "server crashed (%s):\n%s" code output)
  | Reload Page ->
      Some "browser: reload the page"
  | Reload (Stylesheets _) ->
      Some "browser: swap the stylesheets"
