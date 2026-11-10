type 'a t =
  | Idle
  | Loading of 'a option
  | Ready of { data : 'a; stale : bool }
  | Failed of { error : Remote.error; last : 'a option }

let data = function
  | Idle ->
      None
  | Loading old ->
      old
  | Ready { data; _ } ->
      Some data
  | Failed { last; _ } ->
      last

let is_loading = function Loading _ -> true | _ -> false
let is_stale = function Ready { stale; _ } -> stale | _ -> false

let map f = function
  | Idle ->
      Idle
  | Loading old ->
      Loading (Option.map f old)
  | Ready { data; stale } ->
      Ready { data = f data; stale }
  | Failed { error; last } ->
      Failed { error; last = Option.map f last }
