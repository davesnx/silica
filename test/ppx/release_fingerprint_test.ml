open Silica

type state = Idle | Saved of string [@@deriving state_shape]

let machine =
  Machine.make ~id:"release" ~init:Idle ~shape:state_shape (fun state () ->
      Machine.Go state
  )

let manual : int Fingerprint.t =
  [%fingerprint failwith "a release build computes no fingerprint"]

let () =
  assert (Option.is_none (Machine.fingerprint machine));
  assert (Result.is_error (Fingerprint.digest manual));
  print_endline "a release build has no fingerprint"
