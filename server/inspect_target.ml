open Silica

type t = {
  path : string;
  lock : Mutex.t;
  mutable socket : Unix.file_descr option;
  mutable last_try : float;
  mutable table : Inspect.Target.t option;
}

let name = "server"

let close target =
  Option.iter
    (fun socket -> try Unix.close socket with Unix.Unix_error _ -> ())
    target.socket;
  target.socket <- None

let write socket message =
  let line = Inspect.to_string message ^ "\n" in
  let rec loop offset =
    if offset < String.length line then
      loop
        (offset
        + Unix.write_substring socket line offset (String.length line - offset)
        )
  in
  loop 0

let connected target =
  match target.socket with
  | Some socket ->
      Some socket
  | None when Unix.gettimeofday () -. target.last_try < 1. ->
      None
  | None -> (
      target.last_try <- Unix.gettimeofday ();
      match Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 with
      | exception Unix.Unix_error _ ->
          None
      | socket -> (
          match
            Unix.connect socket (Unix.ADDR_UNIX target.path);
            write socket
              (Hello_target { target = name; kind = Server; url = None });
            write socket
              (Event
                 {
                   id = None;
                   target = name;
                   event =
                     Tree
                       ( match target.table with
                       | Some table ->
                           Inspect.Target.tree table
                       | None ->
                           []
                       );
                 }
              )
          with
          | () ->
              target.socket <- Some socket;
              Some socket
          | exception Unix.Unix_error _ ->
              (try Unix.close socket with Unix.Unix_error _ -> ());
              None
        )
    )

let deliver target event =
  match connected target with
  | Some socket -> (
      try write socket (Event { id = None; target = name; event })
      with Unix.Unix_error _ -> close target
    )
  | None ->
      ()

let start ~path =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let target =
    {
      path;
      lock = Mutex.create ();
      socket = None;
      last_try = neg_infinity;
      table = None;
    }
  in
  target.table <-
    Some
      (Inspect.Target.create ~target:name
         ~clock:(fun () -> Unix.gettimeofday () *. 1000.)
         ~send:(deliver target) ()
      );
  Mutex.protect target.lock (fun () -> ignore (connected target));
  target

let island target ~instance app =
  match target.table with
  | None ->
      app
  | Some table ->
      let inspector =
        Inspect.Target.observer ~path:(fun path -> path ^ "#" ^ instance) table
      in
      Some
        (Inspect.Target.combine
           {
             inspector with
             observe =
               (fun observed ->
                 Mutex.protect target.lock (fun () -> inspector.observe observed)
               );
           }
           app
        )
