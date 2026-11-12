open Silica
module Json = Silica_json.Json

type js_handoff

type entry = string * string * string * string

let take_global : unit -> js_handoff Js.Nullable.t =
  [%mel.raw
    {|function () {
  var handoff = globalThis.__silica_handoff;
  delete globalThis.__silica_handoff;
  return handoff == null ? null : handoff;
}|}]

let decode : js_handoff -> entry array * (string * string) array =
  [%mel.raw
    {|function (handoff) {
  var entries = [], islands = [];
  if (handoff && Array.isArray(handoff.instances))
    handoff.instances.forEach(function (entry) {
      if (entry && typeof entry.path === "string" && typeof entry.machine === "string"
          && typeof entry.kind === "string" && typeof entry.text === "string")
        entries.push([entry.path, entry.machine, entry.kind, entry.text]);
    });
  if (handoff && Array.isArray(handoff.islands))
    handoff.islands.forEach(function (island) {
      if (island && typeof island.channel === "string" && typeof island.instance === "string")
        islands.push([island.channel, island.instance]);
    });
  return [entries, islands];
}|}]

let encode : entry array -> (string * string) array -> string -> js_handoff =
  [%mel.raw
    {|function (entries, islands, url) {
  return {
    instances: entries.map(function (entry) {
      return { path: entry[0], machine: entry[1], kind: entry[2], text: entry[3] };
    }),
    islands: islands.map(function (island) { return { channel: island[0], instance: island[1] } }),
    url: url
  };
}|}]

let publish : (unit -> js_handoff) -> unit =
  [%mel.raw
    {|function (suspend) {
  globalThis.__silica_dev = { suspend: function () { return suspend() } };
}|}]

let report : string -> unit =
  [%mel.raw
    {|function (text) {
  if (typeof globalThis.dispatchEvent === "function" && typeof CustomEvent === "function")
    globalThis.dispatchEvent(new CustomEvent("silica-resumed", { detail: text }));
}|}]

let entry_of (path, (entry : Handoff.entry)) : entry =
  let kind, text =
    match entry.snapshot with
    | Encoded text ->
        ("encoded", text)
    | At_start ->
        ("start", "")
    | Not_encoded reason ->
        ("none", reason)
  in
  (path, entry.machine, kind, text)

let handoff_of (entries, islands) : Handoff.t =
  {
    instances =
      Array.to_list
        (Array.map
           (fun (path, machine, kind, text) ->
             ( path,
               {
                 Handoff.machine;
                 snapshot =
                   ( match kind with
                   | "encoded" ->
                       Encoded text
                   | "start" ->
                       At_start
                   | _ ->
                       Not_encoded text
                   );
               }
             )
           )
           entries
        );
    islands = Array.to_list islands;
    url = Dom_renderer.current_url ();
  }

let outcome_json (outcome : Handoff.outcome) =
  let string text = Json.declassify (`String text) in
  Json.to_string
    (Json.declassify
       (`Assoc
          [
            ("t", string "resumed");
            ("kept", Json.declassify (`List (List.map string outcome.kept)));
            ( "restarted",
              Json.declassify
                (`List
                   (List.map
                      (fun (restarted : Handoff.restarted) ->
                        Json.declassify
                          (`Assoc
                             [
                               ("path", string restarted.path);
                               ("machine", string restarted.machine);
                               ( "reason",
                                 string (Handoff.reason_text restarted.reason)
                               );
                             ]
                          )
                      )
                      outcome.restarted
                   )
                )
            );
          ]
       )
    )

let remove_portals : unit -> unit =
  [%mel.raw
    {|function () {
  document.querySelectorAll("[data-silica-portal]").forEach(function (portal) { portal.remove() });
}|}]

let cleanups : (unit -> unit) list ref = ref []

let run_cleanups () =
  let all = !cleanups in
  cleanups := [];
  List.iter (fun cleanup -> cleanup ()) all

let close_sessions () =
  let all = List.rev !Hot_hook.sessions in
  Hot_hook.sessions := [];
  List.filter_map
    (fun (node, close) ->
      close ();
      match
        ( Dom.opt (Dom.get_attribute node "data-silica-live"),
          Dom.opt (Dom.get_property node "__silica_instance")
        )
      with
      | Some channel, Some instance ->
          Some (channel, instance)
      | _ ->
          None
    )
    all

type router =
  | Router : 'r Router.routes * (unit, 'r, Router.event) Machine.t -> router

let routers : router list ref = ref []

let codecs =
  let state (type p s e) (machine : (p, s, e) Machine.t) :
      (s Codec.t, string) result =
    let rec find = function
      | [] ->
          Machine.state_codec machine
      | Router (routes, router) :: rest -> (
          match
            Type.Id.provably_equal (Machine.type_id machine)
              (Machine.type_id router)
          with
          | Some Type.Equal ->
              Ok (Router.state_codec routes)
          | None ->
              find rest
        )
    in
    find !routers
  in
  { Handoff.state }

let resume () =
  match Js.Nullable.toOption (take_global ()) with
  | None ->
      None
  | Some handoff ->
      let handoff = handoff_of (decode handoff) in
      let hook, outcome = Handoff.hook ~codecs handoff in
      Vdom.adopter :=
        Some
          {
            adopt =
              (fun (hooks, namespace, root, nodes) ->
                Vdom.adopt_changed hooks namespace root nodes
              );
          };
      Some (hook, outcome)

let guarded (platform : Instance.message Platform.t) =
  let halted = ref false in
  let timers = Hashtbl.create 16 and next_timer = ref 0 in
  let sockets = ref [] in
  let renderers = ref [] in
  let io = platform.io in
  let schedule ms run =
    incr next_timer;
    let key = !next_timer in
    let cancel =
      io.schedule ms (fun () ->
          Hashtbl.remove timers key;
          if not !halted then run ()
      )
    in
    Hashtbl.replace timers key cancel;
    fun () ->
      Hashtbl.remove timers key;
      cancel ()
  in
  let post ~path ~body answer =
    io.post ~path ~body (fun result -> if not !halted then answer result)
  in
  let connect (events : Platform.socket_events) =
    let alive () = not !halted in
    let socket =
      io.connect
        {
          on_open = (fun () -> if alive () then events.on_open ());
          on_message = (fun text -> if alive () then events.on_message text);
          on_close = (fun code -> if alive () then events.on_close code);
          on_error = (fun () -> if alive () then events.on_error ());
        }
    in
    sockets := socket :: !sockets;
    socket
  in
  let renderer ~on_route ~dispatch =
    let sources = Hashtbl.create 8 in
    let inner : Instance.message Renderer.t =
      platform.renderer ~on_route ~dispatch:(fun event ->
          if not !halted then dispatch event
      )
    in
    renderers := (inner, sources) :: !renderers;
    {
      inner with
      run = (fun action -> if not !halted then inner.run action);
      start_source =
        (fun ~id source ->
          Hashtbl.replace sources id ();
          inner.start_source ~id source
        );
      stop_source =
        (fun ~id ->
          Hashtbl.remove sources id;
          inner.stop_source ~id
        );
    }
  in
  let halt () =
    halted := true;
    let cancels = Hashtbl.fold (fun _ cancel all -> cancel :: all) timers [] in
    Hashtbl.reset timers;
    List.iter (fun cancel -> cancel ()) cancels;
    List.iter (fun (socket : Platform.socket) -> socket.close 1000) !sockets;
    sockets := [];
    List.iter
      (fun ((renderer : Instance.message Renderer.t), sources) ->
        let ids = Hashtbl.fold (fun id () all -> id :: all) sources [] in
        Hashtbl.reset sources;
        List.iter (fun id -> renderer.stop_source ~id) ids
      )
      !renderers
  in
  ({ platform with renderer; io = { io with schedule; post; connect } }, halt)

let remove_answers () =
  match Dom.opt (Dom.get_by_id Dom.document Answers.script_id) with
  | Some script ->
      Option.iter
        (fun parent -> Dom.remove_child parent script)
        (Dom.opt (Dom.parent_node script))
  | None ->
      ()

let after_mount : (unit -> unit) -> unit =
  [%mel.raw
    {|function (run) { Promise.resolve().then(function () { run() }) }|}]

let wrap platform create =
  let resumed = resume () in
  Option.iter
    (fun (hook, _) ->
      Hot_hook.pending := Some hook;
      remove_answers ()
    )
    resumed;
  let platform, halt = guarded platform in
  let tree = ref None in
  publish (fun () ->
      Inspect_target.stop ();
      halt ();
      run_cleanups ();
      remove_portals ();
      let islands = close_sessions () in
      let handoff =
        Handoff.of_running ~codecs ~islands
          ~url:(Dom_renderer.current_url ())
          (match !tree with Some tree -> Instance.running tree | None -> [])
      in
      encode
        (Array.of_list (List.map entry_of handoff.instances))
        (Array.of_list handoff.islands)
        (Url.to_string handoff.url)
  );
  Option.iter
    (fun (_, outcome) ->
      after_mount (fun () ->
          let outcome = outcome () in
          report (outcome_json outcome);
          Inspect_target.resumed outcome
      )
    )
    resumed;
  ( platform,
    fun ~dev ~on_error ->
      let created = create ~dev ~on_error in
      tree := Some created;
      created
  )

let () =
  Router.on_make :=
    {
      made =
        (fun routes machine -> routers := Router (routes, machine) :: !routers);
    };
  Dom_renderer.cleanups := Some (fun cleanup -> cleanups := cleanup :: !cleanups);
  Hot_hook.current := Some { wrap; observe = Inspect_target.observe }
