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

type event = Save | Answer of (string, string) Remote.answer | Go_back
[@@deriving event_shape]

module Door = struct
  type t = Push | Pull [@event_name "tug"] [@@deriving event_shape]
end

let () =
  let name = Machine.shape_name event_shape in
  assert (name Save = "save");
  assert (name (Answer (Ok "noon")) = "answer");
  assert (name Go_back = "go_back");
  assert (Machine.shape_names event_shape = []);
  assert (Machine.shape_name Door.shape Door.Push = "push");
  assert (Machine.shape_name Door.shape Door.Pull = "tug");
  assert (Machine.shape_names Door.shape = [])

module Loader = struct
  type event =
    | Loaded of (int, string) Remote.answer
        [@event_name_of
          function
          | Remote.Ok _ ->
              "loaded ok"
          | Remote.Rejected _ | Remote.Failed _ ->
              "loaded error"]
    | Moved of int * bool
        [@event_name_of
          fun step forward -> Printf.sprintf "moved %d %b" step forward]
    | Retry
  [@@deriving event_shape]
end

let () =
  let name = Machine.shape_name Loader.event_shape in
  assert (name (Loader.Loaded (Ok 1)) = "loaded ok");
  assert (name (Loader.Loaded (Failed (Http 500))) = "loaded error");
  assert (name (Loader.Moved (2, true)) = "moved 2 true");
  assert (name Loader.Retry = "retry")
