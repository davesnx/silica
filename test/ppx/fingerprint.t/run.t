Without SILICA_FINGERPRINTS, the deriver gives fingerprints.

  $ SILICA_FINGERPRINTS=true ../standalone.exe --impl release.ml -o out.ml
  $ ocamlformat --enable-outside-detected-project --impl out.ml | sed -E 's/"[0-9a-f]{32}"/"<digest>"/'
  type item = { name : string }
  type state = Idle | Saved of item [@@deriving fingerprint]
  
  include struct
    [@@@ocaml.warning "-60"]
  
    let _ = fun (_ : state) -> ()
  
    let state_fingerprint =
      ((let module Silica_check = struct
          type silica_check_item = item = { name : string }
  
          let _ = fun (value : string) : Stdlib.String.t -> value
        end in
       Silica.Fingerprint.declared "<digest>" [])
       [@ocaml.warning "-32-33-34-37-38-60-69"]
        : state Silica.Fingerprint.t)
  
    let _ = state_fingerprint
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
  type 'a box = { content : 'a; items : 'a Other.list } [@@deriving fingerprint]
  
  include struct
    [@@@ocaml.warning "-60"]
  
    let _ = fun (_ : 'a box) -> ()
  
    let box_fingerprint =
      (fun parameter_0 ->
         (Silica.Fingerprint.declared "<digest>"
            [
              Silica.Fingerprint.part parameter_0;
              Silica.Fingerprint.part
                ((let module Silica_default = struct
                    let list_fingerprint _ =
                      Silica.Fingerprint.unknown "Other.list"
  
                    let _ = list_fingerprint
                  end in
                 let open! Silica_default in
                 let open! Other in
                 list_fingerprint)
                   parameter_0
                  : 'a Other.list Silica.Fingerprint.t);
            ] [@ocaml.warning "-32-33-34-37-38-60-69"])
        : 'a Silica.Fingerprint.t -> 'a box Silica.Fingerprint.t)
  
    let _ = box_fingerprint
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

With SILICA_FINGERPRINTS=false, as in the release profile, [@@deriving
fingerprint] gives Fingerprint.off.

  $ SILICA_FINGERPRINTS=false ../standalone.exe --impl release.ml -o out.ml
  $ ocamlformat --enable-outside-detected-project --impl out.ml
  type item = { name : string }
  type state = Idle | Saved of item [@@deriving fingerprint]
  
  include struct
    let _ = fun (_ : state) -> ()
    let state_fingerprint = (Silica.Fingerprint.off : state Silica.Fingerprint.t)
    let _ = state_fingerprint
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
  type 'a box = { content : 'a; items : 'a Other.list } [@@deriving fingerprint]
  
  include struct
    let _ = fun (_ : 'a box) -> ()
  
    let box_fingerprint =
      (fun _ -> Silica.Fingerprint.off
        : 'a Silica.Fingerprint.t -> 'a box Silica.Fingerprint.t)
  
    let _ = box_fingerprint
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

The checks compile when the names in scope are the scanned declarations.

  $ ocamlc -c silica.ml other.ml
  $ SILICA_FINGERPRINTS=true ../standalone.exe --impl unshadowed.ml -o unshadowed_out.ml
  $ ocamlc -c unshadowed_out.ml

A same-file name that an open or an include after its declaration may shadow
gives no fingerprint: the machine is not handed over.

  $ SILICA_FINGERPRINTS=true ../standalone.exe --impl shadow_open.ml -o shadow_open_out.ml
  $ tr -s ' \n' ' ' < shadow_open_out.ml | grep -o 'unknown "[^"]*"'
  unknown "item (an open or include after its declaration may hide it)"

  $ SILICA_FINGERPRINTS=true ../standalone.exe --impl shadow_include.ml -o shadow_include_out.ml
  $ tr -s ' \n' ' ' < shadow_include_out.ml | grep -o 'unknown "[^"]*"'
  unknown "item (an open or include after its declaration may hide it)"

  $ SILICA_FINGERPRINTS=true ../standalone.exe --impl shadow_alias.ml -o shadow_alias_out.ml
  $ tr -s ' \n' ' ' < shadow_alias_out.ml | grep -o 'unknown "[^"]*"'
  unknown "count (an open or include after its declaration may hide it)"

A locally abstract type that shadows a same-file name at the derived item fails
to compile.

  $ SILICA_FINGERPRINTS=true ../standalone.exe --impl shadow_newtype.ml -o shadow_newtype_out.ml
  $ ocamlc -c shadow_newtype_out.ml
  File "shadow_newtype_out.ml", lines 16-17, characters 19-35:
  16 | ...................type silica_check_item = item = {
  17 |                      name: string }
  Error: This variant or record definition does not match that of type item
         The original is abstract, but this is a record.
  [2]

  $ SILICA_FINGERPRINTS=true ../standalone.exe --impl shadow_predefined.ml -o shadow_predefined_out.ml
  $ ocamlc -c shadow_predefined_out.ml
  File "shadow_predefined_out.ml", line 13, characters 66-71:
  13 |              let _ = fun (value : 'p0 list) : 'p0 Stdlib.List.t-> value
                                                                         ^^^^^
  Error: The value value has type 'p0 Other.list = 'p0 array
         but an expression was expected of type 'p0 List.t = 'p0 list/2
         File "_none_", line 1:
           Definition of type list/2
  [2]
