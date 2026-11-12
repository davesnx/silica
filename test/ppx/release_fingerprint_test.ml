open Silica

type state = Idle | Saved of string [@@deriving fingerprint]

let machine =
  Machine.make ~id:"release" ~init:Idle
    ~name:(function Idle -> "idle" | Saved _ -> "saved")
    ~fingerprint:state_fingerprint
    (fun state () -> Machine.Go state)

let manual : int Fingerprint.t =
  [%fingerprint failwith "a release build computes no fingerprint"]

let () =
  assert (
    match Option.map Fingerprint.digest (Machine.fingerprint machine) with
    | Some (Error _) ->
        true
    | Some (Ok _) | None ->
        false
  );
  assert (Result.is_error (Fingerprint.digest manual));
  print_endline "a release build has no fingerprint"
