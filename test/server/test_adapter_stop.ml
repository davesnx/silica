module Adapter = Silica_adapter
module Server = Adapter.Server

let report = Filename.temp_file "silica" "on_stop"

let serve stdout_writer =
  Unix.dup2 stdout_writer Unix.stdout;
  let finished = Atomic.make 0 in
  let config =
    {
      (Silica_server.default_config ~origins:Silica_server.Localhost
         ~secret:"0123456789abcdef0123"
      )
      with
      observer =
        { Silica_server.stderr_observer with log = (fun ~source:_ _ -> ()) };
    }
  in
  let server =
    Server.create config
      ~read:(fun _ -> None)
      ~handlers:[] ~islands:[] ~topics:[]
      ~page:(fun _ ->
        Thread.delay 0.5;
        Atomic.incr finished;
        { Silica_server.status = 200; headers = []; body = "slow page" }
      )
      ()
  in
  let on_stop () =
    Out_channel.with_open_gen [ Open_append; Open_creat ] 0o644 report
      (fun channel ->
        Printf.fprintf channel "on_stop after %d finished\n"
          (Atomic.get finished)
    );
    Thread.delay 0.5
  in
  Adapter.run ~port:0 ~on_stop server

let read_report () =
  match In_channel.with_open_bin report In_channel.input_all with
  | content ->
      content
  | exception Sys_error _ ->
      ""

let rec wait_for_report attempts =
  match read_report () with
  | "" when attempts > 0 ->
      Unix.sleepf 0.02;
      wait_for_report (attempts - 1)
  | content ->
      content

let listening_port channel =
  let line = input_line channel in
  let colon = String.rindex line ':' in
  int_of_string (String.sub line (colon + 1) (String.length line - colon - 1))

let read_all descriptor =
  let buffer = Buffer.create 256 and chunk = Bytes.create 4096 in
  let rec go () =
    match Unix.read descriptor chunk 0 (Bytes.length chunk) with
    | 0 ->
        Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer chunk 0 count;
        go ()
  in
  go ()

let stop_test pid port () =
  let open Alcotest in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  let request =
    "GET / HTTP/1.1\r\nhost: 127.0.0.1\r\nconnection: close\r\n\r\n"
  in
  ignore (Unix.write_substring socket request 0 (String.length request));
  Unix.sleepf 0.1;
  Unix.kill pid Sys.sigterm;
  let response = read_all socket in
  Unix.close socket;
  check bool "the request in flight finished" true
    (String.starts_with ~prefix:"HTTP/1.1 200" response);
  check string "on_stop ran after the drain" "on_stop after 1 finished\n"
    (wait_for_report 250);
  Unix.kill pid Sys.sigterm;
  let _, status = Unix.waitpid [] pid in
  check bool "exit code 0" true (status = Unix.WEXITED 0);
  check string "on_stop ran one time" "on_stop after 1 finished\n"
    (read_report ())

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.remove report;
  let reader, writer = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 ->
      Unix.close reader;
      serve writer
  | pid ->
      Unix.close writer;
      let port = listening_port (Unix.in_channel_of_descr reader) in
      Alcotest.run "silica-adapter-stop"
        [
          ( "run",
            [
              Alcotest.test_case
                "SIGTERM: on_stop runs once after the drain, a second SIGTERM \
                 does not run it again"
                `Quick (stop_test pid port);
            ]
          );
        ]
