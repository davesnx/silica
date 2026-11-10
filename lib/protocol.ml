module Json = Silica_json.Json

type host =
  | Focus of Ref.t
  | Blur of Ref.t
  | Scroll_into_view of Ref.t
  | Scroll_to of { target : Ref.t option; left : int; top : int }
  | Navigate of Url.t
  | Replace of Url.t
  | Back
  | Forward
  | Reload

let host_of_command : type e. e Cmd.t -> host option = function
  | Cmd.Focus r ->
      Some (Focus r)
  | Cmd.Blur r ->
      Some (Blur r)
  | Cmd.Scroll_into_view r ->
      Some (Scroll_into_view r)
  | Cmd.Scroll_to { target; left; top } ->
      Some (Scroll_to { target; left; top })
  | Cmd.Navigate u ->
      Some (Navigate u)
  | Cmd.Replace u ->
      Some (Replace u)
  | Cmd.Back ->
      Some Back
  | Cmd.Forward ->
      Some Forward
  | Cmd.Reload ->
      Some Reload
  | Cmd.Set_title _
  | Cmd.Set_status _
  | Cmd.Set_document_attribute _
  | Cmd.Set_cookie _
  | Cmd.Resume _
  | Cmd.Revert_navigation
  | Cmd.Preload _
  | Cmd.Remount
  | Cmd.Measure _
  | Cmd.Copy _
  | Cmd.Load_script _
  | Cmd.Load_chunk _ ->
      None

let host_of_cmd : type e. e Cmd.host -> host option = function
  | Cmd.Run command ->
      host_of_command command
  | Cmd.Start_timer _
  | Cmd.Start_call _
  | Cmd.Start_live _
  | Cmd.Start_source _
  | Cmd.Stop _ ->
      None

let host_to_cmd : type e. host -> e Cmd.t = function
  | Focus r ->
      Cmd.Focus r
  | Blur r ->
      Cmd.Blur r
  | Scroll_into_view r ->
      Cmd.Scroll_into_view r
  | Scroll_to { target; left; top } ->
      Cmd.Scroll_to { target; left; top }
  | Navigate u ->
      Cmd.Navigate u
  | Replace u ->
      Cmd.Replace u
  | Back ->
      Cmd.Back
  | Forward ->
      Cmd.Forward
  | Reload ->
      Cmd.Reload

let int n = Json.declassify (`Int n)
let ( let* ) = Result.bind

let field name l =
  match List.assoc_opt name l with
  | Some v ->
      Ok v
  | None ->
      Error ("missing field: " ^ name)

let string_field name l =
  let* v = field name l in
  match Json.classify v with
  | `String s ->
      Ok s
  | _ ->
      Error ("field " ^ name ^ " must be a string")

let int_field name l =
  let* v = field name l in
  match Json.classify v with
  | `Int n ->
      Ok n
  | _ ->
      Error ("field " ^ name ^ " must be an integer")

let str s = Json.declassify (`String s)
let obj l = Json.declassify (`Assoc l)

let encode_host = function
  | Focus r ->
      obj [ ("focus", str (Ref.name r)) ]
  | Blur r ->
      obj [ ("blur", str (Ref.name r)) ]
  | Scroll_into_view r ->
      obj [ ("scroll", str (Ref.name r)) ]
  | Scroll_to { target; left; top } ->
      obj
        [
          ( "scroll_to",
            obj
              (( match target with
                 | Some r ->
                     [ ("target", str (Ref.name r)) ]
                 | None ->
                     []
                 )
              @ [ ("left", int left); ("top", int top) ]
              )
          );
        ]
  | Navigate u ->
      obj [ ("navigate", str (Url.to_string u)) ]
  | Replace u ->
      obj [ ("replace", str (Url.to_string u)) ]
  | Back ->
      str "back"
  | Forward ->
      str "forward"
  | Reload ->
      str "reload"

let decode_host j =
  match Json.classify j with
  | `String "back" ->
      Ok Back
  | `String "forward" ->
      Ok Forward
  | `String "reload" ->
      Ok Reload
  | `Assoc [ (k, v) ] -> (
      match (k, Json.classify v) with
      | "focus", `String s ->
          Ok (Focus (Ref.make s))
      | "blur", `String s ->
          Ok (Blur (Ref.make s))
      | "scroll", `String s ->
          Ok (Scroll_into_view (Ref.make s))
      | "scroll_to", `Assoc l ->
          let* left = int_field "left" l in
          let* top = int_field "top" l in
          let* target =
            match List.assoc_opt "target" l with
            | None ->
                Ok None
            | Some _ ->
                let* name = string_field "target" l in
                Ok (Some (Ref.make name))
          in
          Ok (Scroll_to { target; left; top })
      | "navigate", `String s ->
          Ok (Navigate (Url.of_string s))
      | "replace", `String s ->
          Ok (Replace (Url.of_string s))
      | _ ->
          Error ("unknown cmd: " ^ k)
    )
  | _ ->
      Error "invalid cmd"

let version = 1

type subscription =
  | Island of { channel : string; instance : string }
  | Topic of { name : string; key : string }

type client =
  | Hello of { v : int; csrf : string; sub : subscription }
  | Event of { id : int; event : Json.t }
  | Ping

type server =
  | Hello_ok of { v : int }
  | State of { seq : int; ack : int; state : Json.t; cmds : host list }
  | Error of { code : string }
  | Pong

let rec all = function
  | [] ->
      Ok []
  | x :: xs ->
      let* x = x in
      let* xs = all xs in
      Ok (x :: xs)

let message text =
  let* json = Json.of_string text in
  match Json.classify json with
  | `Assoc l ->
      let* tag = string_field "t" l in
      Ok (tag, l)
  | _ ->
      Error "expected an object"

let encode_sub = function
  | Island { channel; instance } ->
      obj [ ("island", str channel); ("id", str instance) ]
  | Topic { name; key } ->
      obj [ ("topic", str name); ("key", str key) ]

let decode_sub json =
  match Json.classify json with
  | `Assoc l -> (
      match (List.assoc_opt "island" l, List.assoc_opt "topic" l) with
      | Some _, _ ->
          let* channel = string_field "island" l in
          let* instance = string_field "id" l in
          Ok (Island { channel; instance })
      | None, Some _ ->
          let* name = string_field "topic" l in
          let* key = string_field "key" l in
          Ok (Topic { name; key })
      | None, None ->
          Error "sub must name an island or a topic"
    )
  | _ ->
      Error "sub must be an object"

let message_json tag fields = Json.to_string (obj (("t", str tag) :: fields))

let encode_client = function
  | Hello { v; csrf; sub } ->
      message_json "hello"
        [ ("v", int v); ("csrf", str csrf); ("sub", encode_sub sub) ]
  | Event { id; event } ->
      message_json "event" [ ("id", int id); ("event", event) ]
  | Ping ->
      message_json "ping" []

let decode_client text =
  let* tag, l = message text in
  match tag with
  | "hello" ->
      let* v = int_field "v" l in
      let* csrf = string_field "csrf" l in
      let* sub = field "sub" l in
      let* sub = decode_sub sub in
      Ok (Hello { v; csrf; sub })
  | "event" ->
      let* id = int_field "id" l in
      let* event = field "event" l in
      Ok (Event { id; event })
  | "ping" ->
      Ok Ping
  | other ->
      Error ("unknown client message: " ^ other)

let state_fields ~seq ~ack ~state cmds =
  [
    ("seq", int seq);
    ("ack", int ack);
    ("state", state);
    ("cmds", Json.declassify (`List (List.map encode_host cmds)));
  ]

let encode_server = function
  | Hello_ok { v } ->
      message_json "hello" [ ("v", int v) ]
  | State { seq; ack; state; cmds } ->
      message_json "state" (state_fields ~seq ~ack ~state cmds)
  | Error { code } ->
      message_json "error" [ ("code", str code) ]
  | Pong ->
      message_json "pong" []

let decode_server text =
  let* tag, l = message text in
  match tag with
  | "hello" ->
      let* v = int_field "v" l in
      Ok (Hello_ok { v })
  | "state" -> (
      let* seq = int_field "seq" l in
      let* ack = int_field "ack" l in
      let* state = field "state" l in
      let* cmds = field "cmds" l in
      match Json.classify cmds with
      | `List cs ->
          let* cmds = all (List.map decode_host cs) in
          Ok (State { seq; ack; state; cmds })
      | _ ->
          Error "cmds must be a list"
    )
  | "error" ->
      let* code = string_field "code" l in
      Ok (Error { code })
  | "pong" ->
      Ok Pong
  | other ->
      Error ("unknown server message: " ^ other)

let encode_event codec ~id e =
  encode_client (Event { id; event = codec.Codec.encode e })

let encode_state codec ~seq ~ack s cmds =
  message_json "state"
    (state_fields ~seq ~ack ~state:(codec.Codec.encode s) cmds)

let decode_state codec json = codec.Codec.decode json
let decode_event_payload codec json = codec.Codec.decode json

type close_code = int

let close_version = 4001
let close_hello = 4002
let close_csrf = 4003
let close_unknown = 4004
let close_session = 4005
let close_idle = 4008
let close_too_big = 1009
let close_server_error = 1011
let max_message = 65536
let hard_limit = 1048576
let ping_interval_ms = 30000
let pong_timeout_ms = 10000
