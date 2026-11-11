type running = (string, unit -> unit) Hashtbl.t

let call (type e) (io : Platform.io) ~(deliver : e -> unit) ep input (k : _ -> e)
    =
  let fail err = deliver (k (Remote.Failed err)) in
  io.post ~path:(Remote.path ep)
    ~body:(Codec.to_string (Remote.input ep) input)
    (function
      | Ok { Platform.status = 200; body } -> (
          match Codec.of_string (Remote.output ep) body with
          | Ok o ->
              deliver (k (Remote.Ok o))
          | Error m ->
              fail (Remote.Decode m)
        )
      | Ok { Platform.status = 422; body } -> (
          match Codec.of_string (Remote.rejected ep) body with
          | Ok x ->
              deliver (k (Remote.Rejected x))
          | Error m ->
              fail (Remote.Decode m)
        )
      | Ok { status; _ } ->
          fail (Remote.Http status)
      | Error message ->
          fail (Remote.Network message)
      )

type answer = (Platform.reply, string) result -> unit

type preload = {
  mutable waiter : answer option;
  mutable kept : (string * Platform.cancel) option;
}

let preload_ms = 30_000

let preloads (io : Platform.io) =
  let table : (string * string, preload) Hashtbl.t = Hashtbl.create 8 in
  let owned key entry =
    match Hashtbl.find_opt table key with
    | Some current ->
        current == entry
    | None ->
        false
  in
  let drop key entry = if owned key entry then Hashtbl.remove table key in
  let start ~path ~body =
    let key = (path, body) in
    if not (Hashtbl.mem table key) then (
      let entry = { waiter = None; kept = None } in
      Hashtbl.replace table key entry;
      io.post ~path ~body (fun result ->
          match[@warning "-4"] (entry.waiter, result) with
          | Some answer, Ok { Platform.status = 200; _ } ->
              answer result
          | Some answer, _ ->
              io.post ~path ~body answer
          | None, Ok { Platform.status = 200; body = output }
            when owned key entry ->
              entry.kept <-
                Some (output, io.schedule preload_ms (fun () -> drop key entry))
          | None, _ ->
              drop key entry
      )
    )
  in
  let post ~path ~body (answer : answer) =
    let key = (path, body) in
    match Hashtbl.find_opt table key with
    | Some { kept = Some (output, cancel); _ } ->
        Hashtbl.remove table key;
        cancel ();
        answer (Ok { Platform.status = 200; body = output })
    | Some entry ->
        Hashtbl.remove table key;
        entry.waiter <- Some answer
    | None ->
        io.post ~path ~body answer
  in
  (start, post)

let stop (running : running) id =
  match Hashtbl.find_opt running id with
  | Some cancel ->
      Hashtbl.remove running id;
      cancel ()
  | None ->
      ()

let error_view (error : Node.error) ~retry ~reload : Instance.message Node.t =
  let el = Node.element and text = Node.text in
  el "div"
    [ Attr.string "role" "alert"; Attr.string "data-silica-error" error.id ]
    [
      el "h1" [] [ text "Something went wrong" ];
      el "p" [] [ text ("Error id: " ^ error.id) ];
      ( match error.detail with
      | Some detail ->
          el "pre" [] [ text detail ]
      | None ->
          Node.list []
      );
      el "button" [ Attr.on_press retry ] [ text "Try again" ];
      el "button" [ Attr.on_press reload ] [ text "Reload the page" ];
    ]

let max_replay_rounds = 200

let answered : type e.
    Answers.t -> e Action.host -> (string * e * string * string) option =
 fun table -> function[@warning "-4"]
  | Action.Start_call { id; endpoint; input; on_result; _ } -> (
      let path = Remote.path endpoint in
      let encoded = Codec.to_string (Remote.input endpoint) input in
      match Answers.find table ~path ~input:encoded with
      | None ->
          None
      | Some output -> (
          match Codec.of_string (Remote.output endpoint) output with
          | Ok value ->
              Some (id, on_result (Remote.Ok value), path, encoded)
          | Error _ ->
              None
        )
    )
  | _ ->
      None

let live_actions actions =
  List.fold_left
    (fun live action ->
      match[@warning "-4"] action with
      | Action.Start_call { id; _ } ->
          List.filter
            (function[@warning "-4"]
              | Action.Start_call { id = other; _ } -> other <> id | _ -> true
              )
            live
          @ [ action ]
      | Action.Stop id ->
          List.filter
            (function[@warning "-4"]
              | Action.Start_call { id = other; _ } -> other <> id | _ -> true
              )
            live
      | _ ->
          live
    )
    [] actions

let start (type p s e) ?(dev = false) ?on_error ?(answers = Answers.empty)
    ?live_config ?(on_route : (Router.event -> e) option)
    ~(platform : Instance.message Platform.t)
    ~(create :
       dev:bool ->
       on_error:(exn -> Node.error -> unit) option ->
       (p, s, e) Instance.t
       ) () =
  let hook exn (error : Node.error) =
    platform.io.log
      ("silica: error " ^ error.id ^ ": "
      ^
      match error.detail with
      | Some detail ->
          detail
      | None ->
          Printexc.to_string exn
      );
    Option.iter (fun f -> f exn error) on_error
  in
  let tree = ref (create ~dev ~on_error:(Some hook)) in
  let running : running = Hashtbl.create 8 in
  let queue = Queue.create () in
  let busy = ref false in
  let failed = ref false in
  let first_paint_done = ref false in
  let answers = ref answers in
  let preload, post = preloads platform.io in
  let calls = { platform.io with post } in
  let rec send message =
    Queue.add message queue;
    if not !busy then drain ()
  and renderer_value =
    lazy
      (platform.renderer
         ~on_route:
           (Option.map
              (fun on_route event -> Instance.message !tree (on_route event))
              on_route
           )
         ~dispatch:send
      )
  and exec_one (action : Instance.message Action.host) =
    let r = Lazy.force renderer_value in
    match[@warning "-4"] action with
    | Action.Run (Action.Preload (Remote.Request (endpoint, input))) ->
        preload ~path:(Remote.path endpoint)
          ~body:(Codec.to_string (Remote.input endpoint) input)
    | Action.Run (Action.Load_chunk { chunk = Chunk.Any chunk; on_ready }) ->
        Chunk.load chunk (fun () -> Option.iter send on_ready)
    | Action.Run action ->
        r.run action
    | Action.Start_source { id; source } ->
        r.start_source ~id source
    | Action.Start_timer { id; ms; event } ->
        stop running id;
        let cancel =
          platform.io.schedule ms (fun () ->
              Hashtbl.remove running id;
              send event
          )
        in
        Hashtbl.replace running id cancel
    | Action.Start_call { id; endpoint; input; timeout_ms; on_result } ->
        stop running id;
        let live = ref true in
        let cancel_timeout =
          Option.map
            (fun ms ->
              platform.io.schedule ms (fun () ->
                  if !live then (
                    live := false;
                    Hashtbl.remove running id;
                    send (on_result (Remote.Failed (Remote.Network "timeout")))
                  )
              )
            )
            timeout_ms
        in
        Hashtbl.replace running id (fun () ->
            live := false;
            Option.iter (fun cancel -> cancel ()) cancel_timeout
        );
        call calls
          ~deliver:(fun message ->
            if !live then (
              live := false;
              Option.iter (fun cancel -> cancel ()) cancel_timeout;
              Hashtbl.remove running id;
              send message
            )
          )
          endpoint input on_result
    | Action.Start_live { id; topic; key; on_update } ->
        stop running id;
        let live = ref true in
        let stop_subscription =
          Live_client.subscribe ?config:live_config ~io:platform.io topic key
            ~deliver:(fun update -> if !live then send (on_update update)
          )
        in
        Hashtbl.replace running id (fun () ->
            live := false;
            stop_subscription ()
        )
    | Action.Stop id ->
        stop running id;
        r.stop_source ~id
  and exec actions = List.iter exec_one actions
  and paint node = (Lazy.force renderer_value).paint node
  and step message =
    let node, actions = Instance.update !tree message in
    paint_unless_waiting node;
    exec actions
  and paint_unless_waiting node =
    if !first_paint_done || not (Instance.waiting !tree) then (
      first_paint_done := true;
      paint node
    )
  and fail exn =
    Queue.clear queue;
    let error = Instance.report !tree exn in
    Hashtbl.iter (fun _ cancel -> cancel ()) running;
    Hashtbl.reset running;
    failed := true;
    let retry = Instance.callback (fun () -> restart ()) in
    let reload =
      Instance.callback (fun () -> (Lazy.force renderer_value).run Action.Reload)
    in
    paint (error_view error ~retry ~reload)
  and guarded f = try f () with exn -> fail exn
  and restart () =
    Queue.clear queue;
    ignore (Instance.unmount !tree);
    tree := create ~dev ~on_error:(Some hook);
    failed := false;
    guarded (fun () -> step None)
  and drain () =
    busy := true;
    match
      while not (Queue.is_empty queue) do
        let message = Queue.pop queue in
        if !failed then
          Instance.perform message
        else
          guarded (fun () -> step (Some message))
      done
    with
    | () ->
        busy := false
    | exception error ->
        busy := false;
        raise error
  in
  let replay_first () =
    let rec loop message pending round =
      let node, actions = Instance.update !tree message in
      let pending = pending @ actions in
      let next =
        List.find_map
          (fun action -> answered !answers action)
          (live_actions pending)
      in
      match next with
      | Some (id, event, path, encoded) when round < max_replay_rounds ->
          answers := Answers.remove !answers ~path ~input:encoded;
          let pending =
            List.filter
              (function[@warning "-4"]
                | Action.Start_call { id = other; _ } -> other <> id | _ -> true
                )
              pending
          in
          loop (Some event) pending (round + 1)
      | _ ->
          (node, pending)
    in
    loop None [] 0
  in
  let first_failure = ref None in
  busy := true;
  ( match
      try
        let node, actions = replay_first () in
        paint_unless_waiting node;
        exec actions
      with exn ->
        first_failure := Some exn;
        fail exn
    with
  | () ->
      busy := false
  | exception error ->
      busy := false;
      raise error
  );
  drain ();
  match !first_failure with Some exn when dev -> raise exn | _ -> ()
