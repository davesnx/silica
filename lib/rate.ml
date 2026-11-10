type t = Throttle of int | Debounce of int

type scheduler = int -> (unit -> unit) -> unit -> unit

let throttle ~schedule ms deliver =
  let window_open = ref false in
  let held = ref None in
  let rec open_window () =
    window_open := true;
    let (_ : unit -> unit) =
      schedule ms (fun () ->
          match !held with
          | Some value ->
              held := None;
              deliver value;
              open_window ()
          | None ->
              window_open := false
      )
    in
    ()
  in
  fun value ->
    if !window_open then
      held := Some value
    else (
      deliver value;
      open_window ()
    )

let debounce ~schedule ms deliver =
  let cancel = ref (fun () -> ()) in
  fun value ->
    !cancel ();
    cancel := schedule ms (fun () -> deliver value)

let limiter ~schedule rate deliver =
  match rate with
  | Throttle ms ->
      throttle ~schedule ms deliver
  | Debounce ms ->
      debounce ~schedule ms deliver
