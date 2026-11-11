open Silica

let root_of id =
  match Dom.opt (Dom.get_by_id Dom.document id) with
  | Some r ->
      r
  | None ->
      failwith ("silica: no element with id " ^ id)

let read_answers () =
  match Dom.opt (Dom.get_by_id Dom.document Answers.script_id) with
  | None ->
      Answers.empty
  | Some element -> (
      match Answers.of_string (Dom.text_content element) with
      | Ok table ->
          table
      | Error message ->
          Web.console_error ("silica: bad answers table: " ^ message);
          Answers.empty
    )

type live_config = Live_client.config = { ping_ms : int; pong_ms : int }

type live = Live : ('s, 'e) Live.t -> live

let sessions : (Dom.node * (unit -> unit)) list ref = ref []

let stop_session el =
  match List.find_opt (fun (n, _) -> n == el) !sessions with
  | Some (_, close) ->
      sessions := List.filter (fun (n, _) -> n != el) !sessions;
      close ()
  | None ->
      ()

let instance_id () = Web.random_hex 8

let rec session :
    'a 'b.
    ?live_config:live_config ->
    ?on_session_gone:(unit -> unit) ->
    dev:bool ->
    lives:live list ->
    ('a, 'b) Live.t ->
    Dom.node ->
    unit ->
    unit =
 fun ?live_config ?on_session_gone ~dev ~lives live root ->
  let client = ref None in
  let renderer =
    Dom_renderer.make ~dev ~adopt:true
      ~hooks:(hooks ?live_config ?on_session_gone ~dev lives)
      ~root ~on_route:None ~dispatch:(fun e ->
        Option.iter (fun c -> Live_client.send_event c live.event e) !client
    )
  in
  let on_state ~seq:_ ~state ~actions =
    match Protocol.decode_state live.state state with
    | Error m ->
        Js.log2 "silica: bad state message" m
    | Ok s ->
        Dom.remove_attribute root "data-silica-offline";
        renderer.paint (live.component.view () s);
        List.iter (fun h -> renderer.run (Protocol.host_to_action h)) actions
  in
  let c =
    Live_client.start ?config:live_config
      ~io:(Web.io ?on_session_gone ())
      ~sub:
        (Protocol.Island { channel = live.channel; instance = instance_id () })
      ~on_state
      ~on_down:(fun () -> Dom.set_attribute root "data-silica-offline" "")
      ~on_fatal:(fun _ -> Js.log "silica: live connection closed")
      ()
  in
  client := Some c;
  fun () -> Live_client.stop c

and hooks ?live_config ?on_session_gone ~dev lives =
  {
    Vdom.start =
      (fun el channel ->
        match
          List.find_opt (fun (Live l) -> l.Live.channel = channel) lives
        with
        | Some (Live l) ->
            sessions :=
              (el, session ?live_config ?on_session_gone ~dev ~lives l el)
              :: !sessions
        | None ->
            Js.log2 "silica: no live registered for channel" channel
      );
    stop = stop_session;
    viewport = ignore;
  }

let platform ?live_config ?on_session_gone ~dev ~hydrate ~lives root =
  {
    Platform.renderer =
      Dom_renderer.make ~dev ~adopt:hydrate
        ~hooks:(hooks ?live_config ?on_session_gone ~dev lives)
        ~root;
    location = Dom_renderer.current_url;
    io = Web.io ?on_session_gone ();
  }

let run (type p s e) ?live_config ?on_session_gone ?(dev = false) ?on_error
    ?(hydrate = true) ~lives ~root
    ~(create :
       dev:bool ->
       on_error:(exn -> Node.error -> unit) option ->
       (p, s, e) Instance.t
       ) ~(on_route : (Router.event -> e) option) () =
  let root = root_of root in
  Runtime.start ?live_config ~dev ?on_error ~answers:(read_answers ()) ?on_route
    ~platform:(platform ?live_config ?on_session_gone ~dev ~hydrate ~lives root)
    ~create ();
  Dom.set_attribute root "data-silica-ready" ""

let recorder ?capacity ?data ~build () =
  Trace.Recorder.create ?capacity ?data ~clock:Js.Date.now ~platform:"browser"
    ~build ()

let mount ?live_config ?on_session_gone ?dev ?on_error ?hydrate ?(lives = [])
    ?observe ~root (component : (_, _, _) Component.t) =
  run ?live_config ?on_session_gone ?dev ?on_error ?hydrate ~lives ~root
    ~create:(fun ~dev ~on_error ->
      Instance.mount ~dev ?on_error ?observe component
    )
    ~on_route:None ()

let mount_app ?live_config ?on_session_gone ?dev ?on_error ?hydrate
    ?(lives = []) ?observe ~root
    (app : Url.t -> (_, _, Router.event) Component.t) =
  run ?live_config ?on_session_gone ?dev ?on_error ?hydrate ~lives ~root
    ~create:(fun ~dev ~on_error ->
      Instance.mount ~dev ?on_error ?observe (app (Dom_renderer.current_url ()))
    )
    ~on_route:(Some Fun.id) ()

let mount_live ?live_config ?on_session_gone ?(dev = false) ~root
    (live : _ Live.t) =
  let (_ : unit -> unit) =
    session ?live_config ?on_session_gone ~dev ~lives:[ Live live ] live
      (root_of root)
  in
  ()

let mount_with_renderer ?dev ?on_error ?answers ?observe ~renderer
    (component : (_, _, _) Component.t) =
  Runtime.start ?dev ?on_error ?answers
    ~platform:
      {
        Platform.renderer = (fun ~on_route:_ -> renderer);
        location = Dom_renderer.current_url;
        io = Web.io ();
      }
    ~create:(fun ~dev ~on_error ->
      Instance.mount ~dev ?on_error ?observe component
    )
    ()

let cookie name = Js.Nullable.toOption (Dom.cookie name)

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
