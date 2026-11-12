open Silica

type t = {
  wrap :
    'p 's 'e.
    Instance.message Platform.t ->
    (dev:bool ->
    on_error:(exn -> Node.error -> unit) option ->
    ('p, 's, 'e) Instance.t
    ) ->
    Instance.message Platform.t
    * (dev:bool ->
      on_error:(exn -> Node.error -> unit) option ->
      ('p, 's, 'e) Instance.t
      );
}

let current : t option ref = ref None
let pending : Instance.resume option ref = ref None

let take () =
  let resume = !pending in
  pending := None;
  resume

let sessions : (Dom.node * (unit -> unit)) list ref = ref []
