open Silica

let state ?(final = false) ?(timers = []) ?(sources = []) name : Spec.state =
  { name; final; timers; sources; held = [] }

let door : Spec.t =
  {
    id = "door";
    init = "closed";
    states =
      [
        state "closed";
        state "open" ~timers:[ 300 ] ~sources:[ "/api/x" ];
        state "gone" ~final:true;
      ];
    edges = [ ("closed", "open"); ("open", "closed"); ("open", "gone") ];
  }

let dead_end : Spec.t =
  {
    id = "dead_end";
    init = "start";
    states = [ state "start"; state "stuck" ];
    edges = [ ("start", "stuck") ];
  }

let write name text =
  Out_channel.with_open_text name (fun channel ->
      Out_channel.output_string channel text
  )

let () =
  write "door.qnt" (Spec.to_quint door);
  write "dead_end.qnt" (Spec.to_quint dead_end)
