open Silica

let digest = Fingerprint.digest
let ok fingerprint = Result.is_ok (digest fingerprint)
let int_fingerprint : int Fingerprint.t = Fingerprint.declared "int" []
let string_fingerprint : string Fingerprint.t = Fingerprint.declared "string" []

module First = struct
  type state =
    | Idle
    | Saved of string list
    | Failed of { reason : string; code : int }
  [@@deriving fingerprint]
end

module Second = struct
  (** The same state as [First], written another way. *)

  type state =
    | Idle  (** Nothing. *)
    | Saved of string list
    | Failed of { reason : string; code : int  (** The code. *) }
  [@@deriving fingerprint]
end

module Renamed_constructor = struct
  type state =
    | Waiting
    | Saved of string list
    | Failed of { reason : string; code : int }
  [@@deriving fingerprint]
end

module Changed_field = struct
  type state =
    | Idle
    | Saved of string list
    | Failed of { reason : string; code : string }
  [@@deriving fingerprint]
end

module Mutable_field = struct
  type state =
    | Idle
    | Saved of string list
    | Failed of { reason : string; mutable code : int }
  [@@deriving fingerprint]
end

module Renamed_field = struct
  type state =
    | Idle
    | Saved of string list
    | Failed of { reason : string; code : int [@mel.as "status"] }
  [@@deriving fingerprint]
end

let () =
  let first = digest First.state_fingerprint in
  assert (Result.is_ok first);
  assert (digest Second.state_fingerprint = first);
  let machine =
    Machine.make ~id:"first" ~init:First.Idle
      ~name:(function
        | First.Idle ->
            "idle"
        | First.Saved _ ->
            "saved"
        | First.Failed _ ->
            "failed"
        )
      ~fingerprint:First.state_fingerprint
      (fun _ () -> Machine.Ignore)
  in
  assert (Option.map digest (Machine.fingerprint machine) = Some first);
  List.iter
    (fun other -> assert (Result.is_ok other && other <> first))
    [
      digest Renamed_constructor.state_fingerprint;
      digest Changed_field.state_fingerprint;
      digest Mutable_field.state_fingerprint;
      digest Renamed_field.state_fingerprint;
    ]

let () =
  let one = digest Followed_one.state_fingerprint in
  assert (Result.is_ok one);
  assert (digest Followed_same.state_fingerprint = one);
  let two = digest Followed_two.state_fingerprint in
  assert (Result.is_ok two && two <> one)

module Box_a = struct
  type 'a box = { content : 'a; count : int } [@@deriving fingerprint]
end

module Box_b = struct
  type 'value box = { content : 'value; count : int } [@@deriving fingerprint]
end

let () =
  let a = digest (Box_a.box_fingerprint int_fingerprint) in
  assert (Result.is_ok a);
  assert (digest (Box_b.box_fingerprint int_fingerprint) = a);
  assert (digest (Box_a.box_fingerprint string_fingerprint) <> a)

module Holds_function = struct
  type state = Idle | Saved of (int -> int) [@@deriving fingerprint]
end

module Holds_exception = struct
  type state = Idle | Failed of exn [@@deriving fingerprint]
end

module Holds_lazy = struct
  type state = Idle | Later of int Lazy.t [@@deriving fingerprint]
end

module Holds_open_variant = struct
  type 'a state = Idle | Tagged of ([> `A ] as 'a) [@@deriving fingerprint]
end

module Holds_object = struct
  type state = Idle | Saved of < name : string > [@@deriving fingerprint]
end

module Gadt = struct
  type _ state = Idle : int state [@@deriving fingerprint]
end

let () =
  assert (
    digest Holds_function.state_fingerprint
    = Error "its state type holds a function"
  );
  assert (
    digest Holds_exception.state_fingerprint
    = Error "its state type holds an exception"
  );
  assert (
    digest Holds_lazy.state_fingerprint
    = Error "its state type holds a lazy value"
  );
  assert (
    digest (Holds_open_variant.state_fingerprint (Fingerprint.declared "a" []))
    = Error "its state type holds an open polymorphic variant"
  );
  assert (
    digest Holds_object.state_fingerprint
    = Error "its state type holds an object"
  );
  assert (
    digest (Gadt.state_fingerprint int_fingerprint)
    = Error "its state type holds the GADT constructor Idle"
  )

type closed = [ `A | `B of int ]

module Polymorphic_variant = struct
  type state = Idle | Tagged of [ closed | `C ] [@@deriving fingerprint]
end

type secret
type extensible = ..
type twice = Twice

module Twice = struct
  type twice = Again
end

module Same_file_problems = struct
  type abstract = Hidden of secret [@@deriving fingerprint]
  type open_type = Extended of extensible [@@deriving fingerprint]
  type ambiguous = Named of twice [@@deriving fingerprint]
end

let () =
  assert (ok Polymorphic_variant.state_fingerprint);
  assert (
    digest Same_file_problems.abstract_fingerprint
    = Error "its state type mentions secret, an abstract type"
  );
  assert (
    digest Same_file_problems.open_type_fingerprint
    = Error "its state type mentions extensible, an extensible type"
  );
  assert (
    digest Same_file_problems.ambiguous_fingerprint
    = Error
        "its state type mentions twice (declared more than once in this file), \
         which has no fingerprint"
  )

module Uses_boxes = struct
  type state = Idle | Counted of int Boxes.box [@@deriving fingerprint]
  type other = Idle | Counted of string Boxes.box [@@deriving fingerprint]
  type no_box = Idle | Plain of Boxes.plain [@@deriving fingerprint]
end

module Opened = struct
  open Boxes

  type state = Idle | Plain of plain [@@deriving fingerprint]
end

let () =
  let state = digest Uses_boxes.state_fingerprint in
  assert (Result.is_ok state);
  assert (ok Uses_boxes.other_fingerprint);
  assert (digest Uses_boxes.other_fingerprint <> state);
  assert (
    digest Uses_boxes.no_box_fingerprint
    = Error "its state type mentions Boxes.plain, which has no fingerprint"
  );
  assert (
    digest Opened.state_fingerprint
    = Error "its state type mentions plain, which has no fingerprint"
  );
  assert (
    digest (Boxes.box_fingerprint int_fingerprint)
    <> digest (Boxes.box_fingerprint string_fingerprint)
  )

module Uses_silica = struct
  type state =
    | Idle
    | Moved of Nav.t
    | Failed of Remote.error
    | Loading of int Status.t
  [@@deriving fingerprint]
end

let () = assert (ok Uses_silica.state_fingerprint)

let manual : int Fingerprint.t = [%fingerprint Fingerprint.declared "manual" []]
let () = assert (digest manual = Ok "manual")

module Uses_mel_as_one = struct
  type state = Holding of Mel_as_one.t [@@deriving fingerprint]
end

module Uses_mel_as_two = struct
  type state = Holding of Mel_as_one.t [@@deriving fingerprint]
end

module Uses_mel_as_changed = struct
  type state = Holding of Mel_as_two.t [@@deriving fingerprint]
end

let () =
  assert (ok Uses_mel_as_one.state_fingerprint);
  assert (
    digest Uses_mel_as_one.state_fingerprint
    = digest Uses_mel_as_two.state_fingerprint
  );
  assert (
    digest Uses_mel_as_one.state_fingerprint
    <> digest Uses_mel_as_changed.state_fingerprint
  )

let () =
  assert (ok Placeholder_one.state_fingerprint);
  assert (ok Placeholder_two.state_fingerprint);
  assert (
    digest Placeholder_one.state_fingerprint
    <> digest Placeholder_two.state_fingerprint
  )
