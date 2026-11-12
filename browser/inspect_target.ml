open Silica

type socket

let open_socket : string -> (unit -> unit) -> (unit -> unit) -> socket =
  [%mel.raw
    {|function (path, opened, closed) {
  var scheme = location.protocol === "https:" ? "wss://" : "ws://";
  var socket = new WebSocket(scheme + location.host + path);
  socket.onopen = function () { opened() };
  socket.onclose = function () { closed() };
  return socket;
}|}]

let send_text : socket -> string -> unit =
  [%mel.raw
    {|function (socket, text) { if (socket.readyState === 1) socket.send(text) }|}]

let close_socket : socket -> unit =
  [%mel.raw {|function (socket) { socket.onclose = null; socket.close(1000) }|}]

let tab_name : unit -> string =
  [%mel.raw
    {|function () {
  var name = globalThis.__silica_inspect_tab;
  if (typeof name !== "string") {
    name = "tab-" + Math.random().toString(16).slice(2, 6);
    globalThis.__silica_inspect_tab = name;
  }
  return name;
}|}]

let later : int -> (unit -> unit) -> unit =
  [%mel.raw {|function (ms, run) { setTimeout(function () { run() }, ms) }|}]

let when_ready : (unit -> unit) -> unit =
  [%mel.raw
    {|function (run) {
  if (globalThis.__silica_inspect_ready) { run(); return }
  if (typeof globalThis.addEventListener !== "function") return;
  var ready = function () {
    globalThis.removeEventListener("silica-inspect", ready);
    run();
  };
  globalThis.addEventListener("silica-inspect", ready);
}|}]

let path = "/_silica/inspect"
let capacity = 5000

type state = {
  name : string;
  mutable socket : socket option;
  mutable connected : bool;
  mutable first : bool;
  mutable waiting : string list;
  mutable count : int;
  mutable stopped : bool;
  mutable asked : bool;
}

let state =
  lazy
    {
      name = tab_name ();
      socket = None;
      connected = false;
      first = true;
      waiting = [];
      count = 0;
      stopped = false;
      asked = false;
    }

let send_message state message =
  let text = Inspect.to_string message in
  match state.socket with
  | Some socket when state.connected ->
      send_text socket text
  | _ when state.first && state.count < capacity ->
      state.waiting <- text :: state.waiting;
      state.count <- state.count + 1
  | _ ->
      ()

let deliver event =
  let state = Lazy.force state in
  send_message state (Event { id = None; target = state.name; event })

let table =
  lazy
    (Inspect.Target.create ~target:(Lazy.force state).name ~clock:Js.Date.now
       ~send:deliver ()
    )

let rec connect state =
  if not state.stopped then
    state.socket <-
      Some
        (open_socket path
           (fun () ->
             Option.iter
               (fun socket ->
                 state.connected <- true;
                 send_text socket
                   (Inspect.to_string
                      (Hello_target
                         {
                           target = state.name;
                           kind = Browser;
                           url =
                             Some (Url.to_string (Dom_renderer.current_url ()));
                         }
                      )
                   );
                 if state.first then (
                   List.iter (send_text socket) (List.rev state.waiting);
                   state.waiting <- [];
                   state.first <- false
                 ) else
                   send_text socket
                     (Inspect.to_string
                        (Event
                           {
                             id = None;
                             target = state.name;
                             event =
                               Tree (Inspect.Target.tree (Lazy.force table));
                           }
                        )
                     )
               )
               state.socket
           )
           (fun () ->
             let was = state.connected in
             state.connected <- false;
             state.socket <- None;
             if was then later 1000 (fun () -> connect state)
           )
        )

let observe given =
  let state = Lazy.force state in
  ( match state.socket with
  | None when state.first && (not state.stopped) && not state.asked ->
      state.asked <- true;
      when_ready (fun () -> connect state)
  | _ ->
      ()
  );
  Some
    (Inspect.Target.combine (Inspect.Target.observer (Lazy.force table)) given)

let resumed (outcome : Handoff.outcome) =
  deliver
    (Resumed
       {
         kept = outcome.kept;
         restarted =
           List.map
             (fun (restarted : Handoff.restarted) ->
               {
                 Inspect.path = restarted.path;
                 machine = restarted.machine;
                 reason = Handoff.reason_text restarted.reason;
               }
             )
             outcome.restarted;
       }
    )

let stop () =
  let state = Lazy.force state in
  state.stopped <- true;
  Option.iter close_socket state.socket;
  state.socket <- None;
  state.connected <- false
