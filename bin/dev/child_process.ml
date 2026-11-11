type t = { pid : int; status : Unix.process_status option Atomic.t }

let rec wait_for pid =
  match Unix.waitpid [] pid with
  | _, status ->
      status
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      wait_for pid

let spawn ~program ~arguments ~environment ~output ~on_exit =
  let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
  match Unix.fork () with
  | 0 -> (
      try
        ignore (Unix.setsid ());
        Sys.set_signal Sys.sigpipe Sys.Signal_default;
        Unix.dup2 ~cloexec:false null Unix.stdin;
        Unix.dup2 ~cloexec:false output Unix.stdout;
        Unix.dup2 ~cloexec:false output Unix.stderr;
        Unix.execvpe program (Array.of_list (program :: arguments)) environment
      with _ -> Unix._exit 127
    )
  | pid ->
      Unix.close null;
      let child = { pid; status = Atomic.make None } in
      ignore
        (Thread.create
           (fun () ->
             let status = wait_for pid in
             Atomic.set child.status (Some status);
             on_exit status
           )
           ()
        );
      child

let pid child = child.pid
let status child = Atomic.get child.status

let signal_group child signal =
  try Unix.kill (-child.pid) signal with Unix.Unix_error _ -> ()

let rec wait_until ~deadline condition =
  if condition () then
    true
  else if Unix.gettimeofday () >= deadline then
    false
  else (
    Thread.delay 0.02;
    wait_until ~deadline condition
  )

let stop child ~grace =
  let exited () = Option.is_some (status child) in
  if not (exited ()) then (
    signal_group child Sys.sigterm;
    if not (wait_until ~deadline:(Unix.gettimeofday () +. grace) exited) then (
      signal_group child Sys.sigkill;
      ignore (wait_until ~deadline:(Unix.gettimeofday () +. 5.) exited)
    )
  );
  signal_group child Sys.sigkill

let describe = function
  | Unix.WEXITED code ->
      Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
      let names =
        [
          (Sys.sigkill, "SIGKILL");
          (Sys.sigterm, "SIGTERM");
          (Sys.sigint, "SIGINT");
          (Sys.sigsegv, "SIGSEGV");
          (Sys.sigabrt, "SIGABRT");
          (Sys.sigbus, "SIGBUS");
        ]
      in
      "signal "
      ^ Option.value
          (List.assoc_opt signal names)
          ~default:(string_of_int signal)
