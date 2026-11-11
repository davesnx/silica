let respond socket body =
  let response =
    Printf.sprintf
      "HTTP/1.1 200 OK\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      (String.length body) body
  in
  ignore (Unix.write_substring socket response 0 (String.length response))

let () =
  match Sys.getenv_opt "FAKE_APP_MODE" with
  | Some "crash" ->
      print_endline "boom";
      exit 3
  | _ ->
      let port = int_of_string (Sys.getenv "PORT") in
      Sys.set_signal Sys.sigterm
        (Sys.Signal_handle
           (fun _ ->
             Unix.sleepf 0.3;
             exit 0
           )
        );
      let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt listener Unix.SO_REUSEADDR true;
      Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      Unix.listen listener 16;
      let buffer = Bytes.create 4096 in
      while true do
        match Unix.accept listener with
        | socket, _ ->
            let count =
              try Unix.read socket buffer 0 4096 with Unix.Unix_error _ -> 0
            in
            let head = Bytes.sub_string buffer 0 count in
            if String.starts_with ~prefix:"GET /_silica/health " head then
              respond socket "ok"
            else
              respond socket ("pid " ^ string_of_int (Unix.getpid ()));
            Unix.close socket
        | exception Unix.Unix_error (Unix.EINTR, _, _) ->
            ()
      done
