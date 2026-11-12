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

let sessions : (Dom.node * (unit -> unit)) list ref = ref []
