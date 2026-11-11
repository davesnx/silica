type 'a state =
  | Here of 'a
  | Idle
  | Loading of (unit -> unit) list
  | Failed of string

type 'a t = {
  mutable state : 'a state;
  loader : (('a, string) result -> unit) -> unit;
}

let ready value = { state = Here value; loader = (fun _ -> ()) }
let deferred loader = { state = Idle; loader }
let get t =
  match t.state with
  | Here value ->
      Some value
  | Idle | Loading _ | Failed _ ->
      None
let failure t =
  match t.state with
  | Failed message ->
      Some message
  | Here _ | Idle | Loading _ ->
      None

let forget_failure t =
  match t.state with
  | Failed _ ->
      t.state <- Idle
  | Here _ | Idle | Loading _ ->
      ()

let load t on_done =
  match t.state with
  | Here _ | Failed _ ->
      on_done ()
  | Loading waiting ->
      t.state <- Loading (on_done :: waiting)
  | Idle ->
      t.state <- Loading [ on_done ];
      let settled = ref false in
      t.loader (fun result ->
          if not !settled then (
            settled := true;
            let waiting =
              match t.state with
              | Loading waiting ->
                  waiting
              | Here _ | Idle | Failed _ ->
                  []
            in
            t.state <-
              ( match result with
              | Ok value ->
                  Here value
              | Error message ->
                  Failed message
              );
            List.iter (fun f -> f ()) (List.rev waiting)
          )
      )

type any = Any : 'a t -> any
