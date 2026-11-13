open Silica

type t = {
  wrap :
    'p 's 'e.
    (dev:bool ->
    on_error:(exn -> Node.error -> unit) option ->
    'p ->
    ('p, 's, 'e) Instance.t
    ) ->
    dev:bool ->
    on_error:(exn -> Node.error -> unit) option ->
    'p ->
    ('p, 's, 'e) Instance.t;
  started : (unit -> unit) -> unit;
  observe : Instance.observer option -> Instance.observer option;
}

let current : t option ref = ref None

let observe given =
  match !current with Some hot -> hot.observe given | None -> given
let pending : Instance.resume option ref = ref None

let take () =
  let resume = !pending in
  pending := None;
  resume

let sessions : (Dom_bindings.node * (unit -> unit)) list ref = ref []
