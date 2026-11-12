open Silica

let counter_state : Counter.state Codec.t =
  Codec.make
    ~encode:(fun (state : Counter.state) ->
      let constructor =
        match state with
        | Counting _ ->
            "Counting"
        | Saving _ ->
            "Saving"
        | Saved _ ->
            "Saved"
        | Save_failed _ ->
            "Save_failed"
      in
      Codec.constructor constructor [ Codec.int.encode (Counter.count state) ]
    )
    ~decode:(fun json ->
      Result.bind (Codec.constructor_of json) (function
        | constructor, [ count ] ->
            Result.bind (Codec.int.decode count) (fun count ->
                match constructor with
                | "Counting" ->
                    Ok (Counter.Counting count)
                | "Saving" ->
                    Ok (Counter.Saving count)
                | "Saved" ->
                    Ok (Counter.Saved count)
                | "Save_failed" ->
                    Ok (Counter.Save_failed count)
                | _ ->
                    Error ("unknown constructor " ^ constructor)
            )
        | constructor, _ ->
            Error ("unknown constructor " ^ constructor)
        )
    )

let register () =
  Machine.register_codecs Counter.machine ~state:counter_state ()
