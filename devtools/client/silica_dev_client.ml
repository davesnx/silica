open Silica
module Json = Silica_json.Json

type location = {
  file : string;
  line : int;
  column : int;
  excerpt : (int * string) list;
}

type error = { location : location option; severity : string; message : string }

type status =
  | Connecting
  | Building of { complete : int; remaining : int }
  | Failed of error list
  | Ready

type state = {
  mutable status : status;
  mutable crash : (string * string) option;
  mutable lost : bool;
}

type content
type socket
type message

external set_timeout : (unit -> unit) -> int -> unit = "setTimeout"
external web_socket : string -> socket = "WebSocket" [@@mel.new]

external listen : socket -> string -> (message -> unit) -> unit
  = "addEventListener"
[@@mel.send]

external data : message -> string = "data" [@@mel.get]
external set_inner_html : content -> string -> unit = "innerHTML" [@@mel.set]

let socket_url : unit -> string =
  [%mel.raw
    {|function () { return (location.protocol === "https:" ? "wss://" : "ws://") + location.host + "/_silica/dev" }|}]

let reload_page : unit -> unit =
  [%mel.raw {|function () { location.reload() }|}]

let make_content : string -> content =
  [%mel.raw
    {|function (css) {
  var host = document.createElement("div");
  host.id = "silica-dev";
  var root = host.attachShadow({ mode: "open" });
  var style = document.createElement("style");
  style.textContent = css;
  root.appendChild(style);
  var content = document.createElement("div");
  root.appendChild(content);
  (document.body || document.documentElement).appendChild(host);
  return content;
}|}]

let swap_stylesheet : string -> string -> unit =
  [%mel.raw
    {|function (from, to) {
  document.querySelectorAll('link[rel="stylesheet"]').forEach(function (link) {
    var href = link.getAttribute("href") || "";
    if (href === from || new URL(href, location.href).pathname === from) link.setAttribute("href", to);
  });
}|}]

let css =
  {|
.indicator { position: fixed; right: 12px; bottom: 12px; z-index: 2147483647; font: 12px/1.4 ui-monospace, monospace; background: #1f2328; color: #f0f3f6; padding: 4px 8px; border-radius: 4px; }
.indicator[data-status="failed"] { background: #b42318; }
.overlay { position: fixed; inset: 0; z-index: 2147483646; overflow: auto; background: rgba(15, 17, 21, 0.94); color: #f0f3f6; font: 14px/1.5 ui-monospace, monospace; padding: 32px; }
.overlay h1 { font-size: 18px; margin: 0 0 16px; color: #ff8182; }
.error { margin: 0 0 24px; }
.where { color: #79c0ff; margin: 0 0 4px; }
.message { white-space: pre-wrap; margin: 0 0 8px; }
.excerpt { background: #0d1117; padding: 8px 0; margin: 0; }
.line { white-space: pre; padding: 0 12px; }
.line[data-error] { background: #4a1d1d; }
.output { white-space: pre-wrap; background: #0d1117; padding: 8px 12px; margin: 0; }
|}

let field name json =
  match Json.classify json with
  | `Assoc fields ->
      List.assoc_opt name fields
  | _ ->
      None

let string_field name json =
  match Option.map Json.classify (field name json) with
  | Some (`String text) ->
      Some text
  | _ ->
      None

let int_field name json =
  match Option.map Json.classify (field name json) with
  | Some (`Int number) ->
      Some number
  | _ ->
      None

let list_field name json =
  match Option.map Json.classify (field name json) with
  | Some (`List items) ->
      items
  | _ ->
      []

let excerpt_line json =
  match Json.classify json with
  | `List [ number; text ] -> (
      match (Json.classify number, Json.classify text) with
      | `Int number, `String text ->
          Some (number, text)
      | _ ->
          None
    )
  | _ ->
      None

let error_of_json json =
  let location =
    match (string_field "file" json, int_field "line" json) with
    | Some file, Some line ->
        Some
          {
            file;
            line;
            column = Option.value (int_field "col" json) ~default:1;
            excerpt = List.filter_map excerpt_line (list_field "excerpt" json);
          }
    | _ ->
        None
  in
  {
    location;
    severity = Option.value (string_field "severity" json) ~default:"error";
    message = Option.value (string_field "message" json) ~default:"";
  }

let text_element tag attributes text =
  Node.element tag attributes [ Node.text text ]

let pad number = String.make (max 0 (5 - String.length number)) ' ' ^ number

let error_view error =
  let where, excerpt =
    match error.location with
    | None ->
        ([], [])
    | Some location ->
        ( [
            text_element "p"
              [ Attr.string "class" "where" ]
              (location.file ^ ":"
              ^ string_of_int location.line
              ^ ":"
              ^ string_of_int location.column
              );
          ],
          [
            Node.element "div"
              [ Attr.string "class" "excerpt" ]
              (List.map
                 (fun (number, text) ->
                   text_element "div"
                     (Attr.string "class" "line"
                     :: Attr.string "data-line" (string_of_int number)
                     ::
                     ( if number = location.line then
                         [ Attr.string "data-error" "" ]
                       else
                         []
                     )
                     )
                     (pad (string_of_int number) ^ " | " ^ text)
                 )
                 location.excerpt
              );
          ]
        )
  in
  let attributes =
    match error.location with
    | None ->
        [ Attr.string "class" "error" ]
    | Some location ->
        [
          Attr.string "class" "error";
          Attr.string "data-file" location.file;
          Attr.string "data-line" (string_of_int location.line);
        ]
  in
  Node.element "section" attributes
    (where
    @ [
        text_element "pre"
          [ Attr.string "class" "message" ]
          (error.severity ^ ": " ^ error.message);
      ]
    @ excerpt
    )

let overlay title children =
  Node.element "div"
    [
      Attr.string "class" "overlay";
      Attr.string "role" "dialog";
      Attr.string "aria-label" title;
      Attr.string "data-silica-dev-overlay" "";
    ]
    (text_element "h1" [] title :: children)

let indicator status text =
  text_element "div"
    [
      Attr.string "class" "indicator";
      Attr.string "role" "status";
      Attr.string "data-status" status;
    ]
    text

let view state : unit Node.t =
  let status =
    match state.status with
    | Connecting | Ready ->
        []
    | Building { complete; remaining } ->
        [
          indicator "building"
            ("Building " ^ string_of_int complete ^ "/"
            ^ string_of_int (complete + remaining)
            );
        ]
    | Failed errors ->
        [
          indicator "failed" "Build failed";
          overlay "Build failed" (List.map error_view errors);
        ]
  in
  let crash =
    match state.crash with
    | None ->
        []
    | Some (code, output) ->
        [
          overlay
            ("The server stopped (" ^ code ^ ")")
            [ text_element "pre" [ Attr.string "class" "output" ] output ];
        ]
  in
  Node.list (status @ crash)

let content = lazy (make_content css)

let paint state =
  set_inner_html (Lazy.force content) (Render.to_string (view state))

let receive state json =
  match (string_field "t" json, string_field "status" json) with
  | Some "build", Some "started" ->
      state.status <- Building { complete = 0; remaining = 0 }
  | Some "progress", _ ->
      state.status <-
        Building
          {
            complete = Option.value (int_field "complete" json) ~default:0;
            remaining = Option.value (int_field "remaining" json) ~default:0;
          }
  | Some "build", Some "failed" ->
      state.status <- Failed (List.map error_of_json (list_field "errors" json))
  | Some "build", Some "ok" ->
      if state.lost then
        reload_page ()
      else
        state.status <- Ready
  | Some "server", Some "crashed" ->
      state.crash <-
        Some
          ( Option.value (string_field "code" json) ~default:"",
            Option.value (string_field "output" json) ~default:""
          )
  | Some "server", Some ("started" | "restarted") ->
      state.crash <- None
  | Some "reload", _ -> (
      match string_field "what" json with
      | Some "css" ->
          List.iter
            (fun pair ->
              match (string_field "from" pair, string_field "to" pair) with
              | Some from, Some target ->
                  swap_stylesheet from target
              | _ ->
                  ()
            )
            (list_field "css" json)
      | _ ->
          reload_page ()
    )
  | _ ->
      ()

let rec connect state =
  let socket = web_socket (socket_url ()) in
  listen socket "message" (fun message ->
      match Json.of_string (data message) with
      | Ok json ->
          receive state json;
          paint state
      | Error _ ->
          ()
  );
  listen socket "close" (fun _ ->
      state.lost <- true;
      set_timeout (fun () -> connect state) 1000
  )

let () = connect { status = Connecting; crash = None; lost = false }
