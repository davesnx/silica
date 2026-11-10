type 'a promise = 'a Js.Promise.t
type timer

external set_timeout : (unit -> unit) -> int -> timer = "setTimeout"
external clear_timeout : timer -> unit = "clearTimeout"

type init

let csrf_token : unit -> string =
  [%mel.raw
    {|function () { var meta = document.querySelector('meta[name="silica-csrf"]'); return meta ? meta.getAttribute("content") || "" : "" }|}]

let post_init : string -> init =
  [%mel.raw
    {|function (body) {
  var headers = { "content-type": "application/json" };
  var meta = document.querySelector('meta[name="silica-csrf"]');
  if (meta) headers["x-silica-csrf"] = meta.getAttribute("content") || "";
  return { method: "POST", headers: headers, body: body }
}|}]

let static_answer : string -> string -> string Js.Nullable.t =
  [%mel.raw
    {|function (path, body) {
  var meta = document.querySelector('meta[name="silica-static"]');
  if (!meta) return null;
  var bytes = new TextEncoder().encode(body);
  var hash = 0x811c9dc5;
  for (var i = 0; i < bytes.length; i++) hash = Math.imul(hash ^ bytes[i], 0x01000193);
  return (meta.getAttribute("content") || "/") + path.replace(/^\/+/, "") + "/" + (hash >>> 0).toString(16).padStart(8, "0") + ".json"
}|}]

let get_init : unit -> init =
  [%mel.raw {|function () { return { method: "GET" } }|}]

type response

external fetch : string -> init -> response promise = "fetch"
external status : response -> int = "status" [@@mel.get]
external text : response -> string promise = "text" [@@mel.send]

external then_ :
  'a promise ->
  ('a -> 'b promise) ->
  (Js.Promise.error -> 'b promise) ->
  'b promise = "then"
[@@mel.send]

type socket
type event

external web_socket : string -> socket = "WebSocket" [@@mel.new]
external ws_send : socket -> string -> unit = "send" [@@mel.send]
external set_binary_type : socket -> string -> unit = "binaryType" [@@mel.set]

let ws_data : event -> string =
  [%mel.raw
    {|function (ev) { return typeof ev.data === "string" ? ev.data : new TextDecoder().decode(ev.data) }|}]

external ws_on : socket -> string -> (event -> unit) -> unit
  = "addEventListener"
[@@mel.send]

external ws_close : socket -> int -> unit = "close" [@@mel.send]

let ws_close_code : event -> int =
  [%mel.raw
    {|function (ev) { return typeof ev.code === "number" ? ev.code : 0 }|}]

let live_url : unit -> string =
  [%mel.raw
    {|function () { return (location.protocol === "https:" ? "wss://" : "ws://") + location.host + "/_silica/live" }|}]

let schedule ms callback =
  let timer = set_timeout callback ms in
  fun () -> clear_timeout timer

let console_error : string -> unit =
  [%mel.raw {|function (message) { console.error(message) }|}]

let random_hex : int -> string =
  [%mel.raw
    {|function (n) {
  var bytes = new Uint8Array(n);
  globalThis.crypto.getRandomValues(bytes);
  var out = "";
  for (var i = 0; i < n; i++) out += (bytes[i] < 16 ? "0" : "") + bytes[i].toString(16);
  return out
}|}]

let reload_unless_recent : unit -> bool =
  [%mel.raw
    {|function () {
  try {
    var key = "silica-session-reload";
    var now = Date.now();
    var last = Number(sessionStorage.getItem(key) || 0);
    if (now - last < 10000) return false;
    sessionStorage.setItem(key, String(now));
  } catch (e) { return false }
  location.reload();
  return true
}|}]

let reload_once () =
  if not (reload_unless_recent ()) then
    console_error "silica: session gone again after a reload; not reloading"

let post ~path ~body k =
  let request =
    match Js.Nullable.toOption (static_answer path body) with
    | Some url ->
        fetch url (get_init ())
    | None ->
        fetch path (post_init body)
  in
  ignore
    (then_ request
       (fun res ->
         then_ (text res)
           (fun body ->
             k (Ok { Silica.Platform.status = status res; body });
             Js.Promise.resolve ()
           )
           (fun e ->
             k (Error (Js.String.make e));
             Js.Promise.resolve ()
           )
       )
       (fun e ->
         k (Error (Js.String.make e));
         Js.Promise.resolve ()
       )
    )

let connect (events : Silica.Platform.socket_events) : Silica.Platform.socket =
  let ws = web_socket (live_url ()) in
  set_binary_type ws "arraybuffer";
  ws_on ws "open" (fun _ -> events.on_open ());
  ws_on ws "message" (fun ev -> events.on_message (ws_data ev));
  ws_on ws "close" (fun ev -> events.on_close (ws_close_code ev));
  ws_on ws "error" (fun _ -> events.on_error ());
  { send = ws_send ws; close = ws_close ws }

let io ?(on_session_gone = reload_once) () : Silica.Platform.io =
  {
    schedule;
    post;
    connect;
    csrf = csrf_token;
    session_gone = on_session_gone;
    log = console_error;
  }
