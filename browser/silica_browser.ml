open Silica

module Document = Document

external node_of_element : Dom.element -> Dom_bindings.node = "%identity"
external element_of_node : Dom_bindings.node -> Dom.element = "%identity"

let by_id id =
  match Dom_bindings.opt (Dom_bindings.get_by_id Dom_bindings.document id) with
  | Some root ->
      element_of_node root
  | None ->
      failwith ("silica: no element with id " ^ id)

type 'p app = { send : 'p Runtime.event -> unit; state : unit -> Runtime.state }

let send app event = app.send event
let state app = app.state ()

let of_runtime runtime =
  { send = Runtime.send runtime; state = (fun () -> Runtime.state runtime) }

let read_answers () =
  match
    Dom_bindings.opt
      (Dom_bindings.get_by_id Dom_bindings.document Answers.script_id)
  with
  | None ->
      Answers.empty
  | Some element -> (
      match Answers.of_string (Dom_bindings.text_content element) with
      | Ok table ->
          table
      | Error message ->
          Web.console_error ("silica: bad answers table: " ^ message);
          Answers.empty
    )

type live_config = Live_client.config = { ping_ms : int; pong_ms : int }

type live = Live : ('s, 'e) Live.t -> live

let sessions = Hot_hook.sessions

let stop_session el =
  match List.find_opt (fun (n, _) -> n == el) !sessions with
  | Some (_, close) ->
      sessions := List.filter (fun (n, _) -> n != el) !sessions;
      close ()
  | None ->
      ()

let instance_id root =
  match
    Dom_bindings.opt (Dom_bindings.get_property root "__silica_instance")
  with
  | Some instance ->
      instance
  | None ->
      let instance = Web.random_hex 8 in
      Dom_bindings.set_property root "__silica_instance" instance;
      instance

let press_early : unit -> unit =
  [%mel.raw
    {|function () {
  var e = window.__silica_early, n = Date.now();
  if (!e) return;
  delete window.__silica_early;
  e.stop();
  e.q.slice(-16).forEach(function (p) {
    var t = p[0];
    if (n - p[1] < 10000 && t.isConnected) p[2] ? t.requestSubmit() : t.click();
  });
}|}]

let rec session :
    'a 'b.
    ?live_config:live_config ->
    ?on_session_gone:(unit -> unit) ->
    dev:bool ->
    document:Document.t ->
    lives:live list ->
    ('a, 'b) Live.t ->
    Dom_bindings.node ->
    unit Runtime.event ->
    unit =
 fun ?live_config ?on_session_gone ~dev ~document ~lives live root ->
  let client = ref None in
  let renderer =
    Dom_renderer.make ~dev ~adopt:true
      ~hooks:(hooks ?live_config ?on_session_gone ~dev ~document lives)
      ~document ~root ~on_route:None ~dispatch:(fun e ->
        Option.iter (fun c -> Live_client.send_event c live.event e) !client
    )
  in
  let on_state ~seq:_ ~state ~actions =
    match Protocol.decode_state live.state state with
    | Error m ->
        Js.log2 "silica: bad state message" m
    | Ok s ->
        Dom_bindings.remove_attribute root "data-silica-offline";
        renderer.paint (live.component.view () s);
        List.iter (fun h -> renderer.run (Protocol.host_to_action h)) actions
  in
  let c =
    Live_client.start ?config:live_config
      ~io:(Web.io ?on_session_gone ())
      ~sub:
        (Protocol.Island { channel = live.channel; instance = instance_id root })
      ~on_state
      ~on_down:(fun () ->
        Dom_bindings.set_attribute root "data-silica-offline" ""
      )
      ~on_fatal:(fun _ -> Js.log "silica: live connection closed")
      ()
  in
  client := Some c;
  function
  | Runtime.Props () ->
      ()
  | Stop ->
      Live_client.stop c;
      renderer.paint (Node.list []);
      renderer.release ();
      Dom_bindings.remove_attribute root "data-silica-offline"
  | Suspend ->
      Live_client.stop c;
      renderer.release ()

and hooks ?live_config ?on_session_gone ~dev ~document lives =
  {
    Vdom.start =
      (fun el channel ->
        match
          List.find_opt (fun (Live l) -> l.Live.channel = channel) lives
        with
        | Some (Live l) ->
            let stop =
              session ?live_config ?on_session_gone ~dev ~document ~lives l el
            in
            sessions := (el, fun () -> stop Suspend) :: !sessions
        | None ->
            Js.log2 "silica: no live registered for channel" channel
      );
    stop = stop_session;
    viewport = ignore;
  }

let platform ?live_config ?on_session_gone ~dev ~hydrate ~document ~lives root =
  {
    Platform.renderer =
      Dom_renderer.make ~dev ~adopt:hydrate
        ~hooks:(hooks ?live_config ?on_session_gone ~dev ~document lives)
        ~document ~root;
    location = Dom_renderer.current_url;
    io = Web.io ?on_session_gone ();
  }

let run (type p s e) ?live_config ?on_session_gone ?(dev = false) ?on_error
    ?(hydrate = true) ?(document = Document.browser) ~lives ~root ~(props : p)
    ~(create :
       dev:bool ->
       on_error:(exn -> Node.error -> unit) option ->
       p ->
       (p, s, e) Instance.t
       ) ~(on_route : (Router.event -> e) option) () =
  let root = node_of_element root in
  let platform =
    platform ?live_config ?on_session_gone ~dev ~hydrate ~document ~lives root
  in
  let create =
    match !Hot_hook.current with Some hot -> hot.wrap create | None -> create
  in
  let runtime =
    Runtime.start ?live_config ~dev ?on_error ~answers:(read_answers ())
      ?on_route ~platform ~props ~create ()
  in
  Dom_bindings.set_attribute root "data-silica-ready" "";
  let app =
    {
      send =
        (fun event ->
          Runtime.send runtime event;
          match Runtime.state runtime with
          | Stopped ->
              Dom_bindings.remove_attribute root "data-silica-ready"
          | Running | Failed ->
              ()
        );
      state = (fun () -> Runtime.state runtime);
    }
  in
  Option.iter
    (fun (hot : Hot_hook.t) -> hot.started (fun () -> app.send Suspend))
    !Hot_hook.current;
  press_early ();
  app

let recorder ?capacity ?data ~build () =
  Trace.Recorder.create ?capacity ?data ~clock:Js.Date.now ~platform:"browser"
    ~build ()

let mount_with_props ?live_config ?on_session_gone ?dev ?on_error ?hydrate
    ?document ?(lives = []) ?observe ~props ~root
    (component : (_, _, _) Component.t) =
  run ?live_config ?on_session_gone ?dev ?on_error ?hydrate ?document ~lives
    ~root ~props
    ~create:(fun ~dev ~on_error props ->
      Instance.mount_with_props ~dev ?on_error
        ?observe:(Hot_hook.observe observe) ?resume:(Hot_hook.take ()) ~props
        component
    )
    ~on_route:None ()

let mount ?live_config ?on_session_gone ?dev ?on_error ?hydrate ?document ?lives
    ?observe ~root component =
  mount_with_props ?live_config ?on_session_gone ?dev ?on_error ?hydrate
    ?document ?lives ?observe ~props:() ~root component

let mount_app ?live_config ?on_session_gone ?dev ?on_error ?hydrate ?document
    ?(lives = []) ?observe ~root
    (app : Url.t -> (_, _, Router.event) Component.t) =
  run ?live_config ?on_session_gone ?dev ?on_error ?hydrate ?document ~lives
    ~root ~props:()
    ~create:(fun ~dev ~on_error () ->
      Instance.mount ~dev ?on_error ?observe:(Hot_hook.observe observe)
        ?resume:(Hot_hook.take ())
        (app (Dom_renderer.current_url ()))
    )
    ~on_route:(Some Fun.id) ()

let mount_live ?live_config ?on_session_gone ?(dev = false)
    ?(document = Document.browser) ~root (live : _ Live.t) =
  let stop =
    session ?live_config ?on_session_gone ~dev ~document ~lives:[ Live live ]
      live (node_of_element root)
  in
  let lifecycle = ref Runtime.Running in
  press_early ();
  {
    send =
      (fun event ->
        match (event, !lifecycle) with
        | _, Runtime.Stopped | Runtime.Props (), _ ->
            ()
        | (Stop | Suspend), (Running | Failed) ->
            lifecycle := Stopped;
            stop event
      );
    state = (fun () -> !lifecycle);
  }

let mount_with_renderer ?dev ?on_error ?answers ?observe ~renderer
    (component : (_, _, _) Component.t) =
  of_runtime
    (Runtime.start ?dev ?on_error ?answers
       ~platform:
         {
           Platform.renderer = (fun ~on_route:_ -> renderer);
           location = Dom_renderer.current_url;
           io = Web.io ();
         }
       ~props:()
       ~create:(fun ~dev ~on_error () ->
         Instance.mount ~dev ?on_error ?observe component
       )
       ()
    )

let cookie name = Js.Nullable.toOption (Dom_bindings.cookie name)

let import (load : unit -> 'a Js.Promise.t) : 'a Chunk.t =
  Chunk.deferred (fun answer ->
      ignore
        (Js.Promise.catch
           (fun error ->
             answer (Error (Js.String.make error));
             Js.Promise.resolve ()
           )
           (Js.Promise.then_
              (fun value ->
                answer (Ok value);
                Js.Promise.resolve ()
              )
              (load ())
           )
        )
  )
