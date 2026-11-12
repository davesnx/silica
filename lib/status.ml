type 'a t =
  | Idle
  | Loading of 'a option
  | Ready of { data : 'a; stale : bool }
  | Failed of { error : Endpoint.error; last : 'a option }

let data = function
  | Idle ->
      None
  | Loading old ->
      old
  | Ready { data; _ } ->
      Some data
  | Failed { last; _ } ->
      last

let is_loading = function
  | Loading _ ->
      true
  | Idle | Ready _ | Failed _ ->
      false
let is_stale = function
  | Ready { stale; _ } ->
      stale
  | Idle | Loading _ | Failed _ ->
      false

let map f = function
  | Idle ->
      Idle
  | Loading old ->
      Loading (Option.map f old)
  | Ready { data; stale } ->
      Ready { data = f data; stale }
  | Failed { error; last } ->
      Failed { error; last = Option.map f last }

let ( let* ) = Result.bind

let codec (data : 'a Codec.t) : 'a t Codec.t =
  let last = Codec.option data in
  Codec.make
    ~encode:(function
      | Idle ->
          Codec.constructor "Idle" []
      | Loading old ->
          Codec.constructor "Loading" [ last.encode old ]
      | Ready { data = value; stale } ->
          Codec.constructor "Ready"
            [
              Silica_json.Json.declassify
                (`Assoc
                   [
                     ("data", data.encode value);
                     ("stale", Codec.bool.encode stale);
                   ]
                );
            ]
      | Failed { error; last = value } ->
          Codec.constructor "Failed"
            [
              Silica_json.Json.declassify
                (`Assoc
                   [
                     ("error", Endpoint.error_codec.encode error);
                     ("last", last.encode value);
                   ]
                );
            ]
      )
    ~decode:(fun json ->
      let* name, arguments = Codec.constructor_of json in
      match (name, arguments) with
      | "Idle", [] ->
          Ok Idle
      | "Loading", [ old ] ->
          Result.map (fun old -> Loading old) (last.decode old)
      | "Ready", [ fields ] ->
          let* value = Codec.field "data" data fields in
          let* stale = Codec.field "stale" Codec.bool fields in
          Ok (Ready { data = value; stale })
      | "Failed", [ fields ] ->
          let* error = Codec.field "error" Endpoint.error_codec fields in
          let* value = Codec.field "last" last fields in
          Ok (Failed { error; last = value })
      | name, _ ->
          Error ("unknown constructor " ^ name)
    )

let fingerprint (Fingerprint.Fingerprint data : _ Fingerprint.t) :
    _ t Fingerprint.t =
  Fingerprint (Silica ("Status.t", [ data ]))
