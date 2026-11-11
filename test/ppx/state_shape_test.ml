open Silica

type state = Idle | Saving | Saved of string | Not_found of { path : string }
[@@deriving state_shape]

type t = Open | Closed [@state_name "shut"] [@@deriving state_shape]

let () =
  let name = Machine.shape_name state_shape in
  assert (name Idle = "idle");
  assert (name Saving = "saving");
  assert (name (Saved "noon") = "saved");
  assert (name (Not_found { path = "/" }) = "not_found");
  assert (
    Machine.shape_names state_shape = [ "idle"; "saving"; "saved"; "not_found" ]
  );
  assert (Machine.shape_name shape Open = "open");
  assert (Machine.shape_name shape Closed = "shut");
  assert (Machine.shape_names shape = [ "open"; "shut" ])
