module Rpc = Dune_rpc.V1

type progress =
  | Waiting
  | Running of { complete : int; remaining : int }
  | Failed
  | Interrupted
  | Success

type bridge = {
  lock : Mutex.t;
  signal : Condition.t;
  inbox : Csexp.t option Queue.t;
  mutable waiting : Csexp.t option Fiber.Ivar.t option;
  output : out_channel;
}

let read_loop bridge input =
  let rec loop () =
    let item =
      match Csexp.input_opt input with Ok item -> item | Error _ -> None
    in
    Mutex.protect bridge.lock (fun () ->
        Queue.push item bridge.inbox;
        Condition.signal bridge.signal
    );
    if Option.is_some item then loop ()
  in
  try loop ()
  with Sys_error _ ->
    Mutex.protect bridge.lock (fun () ->
        Queue.push None bridge.inbox;
        Condition.signal bridge.signal
    )

module Channel = struct
  type t = bridge

  let write bridge items =
    ( try
        List.iter (Csexp.to_channel bridge.output) items;
        flush bridge.output
      with Sys_error _ -> ()
    );
    Fiber.return ()

  let close _ = Fiber.return ()

  let read bridge =
    Mutex.lock bridge.lock;
    if Queue.is_empty bridge.inbox then (
      let ivar = Fiber.Ivar.create () in
      bridge.waiting <- Some ivar;
      Mutex.unlock bridge.lock;
      Fiber.Ivar.read ivar
    ) else
      let item = Queue.pop bridge.inbox in
      Mutex.unlock bridge.lock;
      Fiber.return item
end

module Client =
  Rpc.Client.Make
    (struct
      include Fiber

      let parallel_iter next ~f =
        Fiber.Stream.In.parallel_iter (Fiber.Stream.In.create next) ~f

      let collect_errors run =
        Fiber.map (Fiber.collect_errors run)
          ~f:
            (Result.map_error
               (List.map (fun (error : Stdune.Exn_with_backtrace.t) -> error.exn)
               )
            )
    end)
    (Channel)

let next_fill bridge () =
  Mutex.lock bridge.lock;
  while Queue.is_empty bridge.inbox || Option.is_none bridge.waiting do
    Condition.wait bridge.signal bridge.lock
  done;
  let item = Queue.pop bridge.inbox in
  let ivar = Option.get bridge.waiting in
  bridge.waiting <- None;
  Mutex.unlock bridge.lock;
  Stdune.Nonempty_list.[ Fiber.Fill (ivar, item) ]

module Diagnostic_table = Hashtbl.Make (struct
  type t = Rpc.Diagnostic.Id.t

  let equal left right = Rpc.Diagnostic.Id.compare left right = Ordering.Eq
  let hash = Rpc.Diagnostic.Id.hash
end)

let message_text diagnostic =
  String.trim
    (Format.asprintf "%a" Pp.to_fmt (Rpc.Diagnostic.message diagnostic))

let error_of_diagnostic ~root diagnostic =
  let severity =
    match Rpc.Diagnostic.severity diagnostic with
    | Some Rpc.Diagnostic.Warning ->
        Dev_event.Warning
    | Some Rpc.Diagnostic.Error | None ->
        Dev_event.Error
  in
  let location =
    Option.map
      (fun loc ->
        let start = Rpc.Loc.start loc and stop = Rpc.Loc.stop loc in
        let file = Source_location.source ~root start.pos_fname in
        {
          Dev_event.file;
          line = start.pos_lnum;
          column = start.pos_cnum - start.pos_bol + 1;
          end_line = stop.pos_lnum;
          end_column = stop.pos_cnum - stop.pos_bol + 1;
          excerpt =
            Source_location.excerpt
              ~path:(Filename.concat root file)
              ~line:start.pos_lnum;
        }
      )
      (Rpc.Diagnostic.loc diagnostic)
  in
  { Dev_event.location; severity; message = message_text diagnostic }

let errors ~root table =
  Diagnostic_table.to_seq_values table
  |> List.of_seq
  |> List.map (error_of_diagnostic ~root)
  |> List.sort_uniq compare

let subscribe ~root ~on_progress ~on_errors client =
  let open Fiber.O in
  let progress () =
    let* stream = Client.poll client Rpc.Sub.progress in
    match stream with
    | Error _ ->
        Fiber.return ()
    | Ok stream ->
        let rec loop () =
          let* next = Client.Stream.next stream in
          match next with
          | None ->
              Fiber.return ()
          | Some progress ->
              on_progress
                ( match progress with
                | Rpc.Progress.Waiting ->
                    Waiting
                | In_progress { complete; remaining; failed = _ } ->
                    Running { complete; remaining }
                | Failed ->
                    Failed
                | Interrupted ->
                    Interrupted
                | Success ->
                    Success
                );
              loop ()
        in
        loop ()
  in
  let diagnostics () =
    let table = Diagnostic_table.create 16 in
    let* stream = Client.poll client Rpc.Sub.diagnostic in
    match stream with
    | Error _ ->
        Fiber.return ()
    | Ok stream ->
        let rec loop () =
          let* next = Client.Stream.next stream in
          match next with
          | None ->
              Fiber.return ()
          | Some events ->
              List.iter
                (function
                  | Rpc.Diagnostic.Event.Add diagnostic ->
                      Diagnostic_table.replace table
                        (Rpc.Diagnostic.id diagnostic)
                        diagnostic
                  | Remove diagnostic ->
                      Diagnostic_table.remove table
                        (Rpc.Diagnostic.id diagnostic)
                  )
                events;
              on_errors (errors ~root table);
              loop ()
        in
        loop ()
  in
  Fiber.fork_and_join_unit progress diagnostics

let socket_path = Filename.concat "_build" (Filename.concat ".rpc" "dune")

let connect () =
  match Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 with
  | exception Unix.Unix_error _ ->
      None
  | socket -> (
      match Unix.connect socket (Unix.ADDR_UNIX socket_path) with
      | () ->
          Some socket
      | exception Unix.Unix_error _ ->
          Unix.close socket;
          None
    )

let session ~root ~on_progress ~on_errors socket =
  let bridge =
    {
      lock = Mutex.create ();
      signal = Condition.create ();
      inbox = Queue.create ();
      waiting = None;
      output = Unix.out_channel_of_descr socket;
    }
  in
  ignore (Thread.create (read_loop bridge) (Unix.in_channel_of_descr socket));
  Fiber.run ~iter:(next_fill bridge)
    (Client.connect bridge
       (Rpc.Request.Initialize.create
          ~id:(Rpc.Request.Id.make (Csexp.Atom "silica-dev"))
       )
       ~f:(subscribe ~root ~on_progress ~on_errors)
    )

type t = { dune : Child_process.t }

let tail path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text ->
      let lines = String.split_on_char '\n' (String.trim text) in
      let count = List.length lines in
      String.concat "\n"
        (List.filteri (fun index _ -> index >= count - 30) lines)
  | exception Sys_error _ ->
      ""

let start ~root ~targets ~log ~on_progress ~on_errors ~on_end =
  let output =
    Unix.openfile log
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC ]
      0o644
  in
  let dune =
    Child_process.spawn ~program:"dune"
      ~arguments:([ "build"; "--root"; "."; "--watch" ] @ targets)
      ~environment:(Unix.environment ()) ~output ~on_exit:ignore
  in
  Unix.close output;
  let deadline = Unix.gettimeofday () +. 60. in
  let rec attach () =
    if Option.is_some (Child_process.status dune) then
      Error ("dune stopped:\n" ^ tail log)
    else if Unix.gettimeofday () > deadline then (
      Child_process.stop dune ~grace:3.;
      Error ("dune opened no RPC socket at " ^ socket_path ^ " within 60 s")
    ) else
      match connect () with
      | Some socket ->
          ignore
            (Thread.create
               (fun () ->
                 ( try session ~root ~on_progress ~on_errors socket
                   with exception_ ->
                     prerr_endline
                       ("silica dev: dune RPC: " ^ Printexc.to_string exception_)
                 );
                 on_end ()
               )
               ()
            );
          Ok { dune }
      | None ->
          Thread.delay 0.1;
          attach ()
  in
  attach ()

let stop watch = Child_process.stop watch.dune ~grace:3.
let log_tail = tail
