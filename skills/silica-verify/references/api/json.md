# API: the library silica.json

## Silica_json.Json

JSON values. The implementation is `Jsonkit.t` on both native and Melange, selected by dune through the virtual library `silica.json`.

```ocaml
type t
```

```ocaml
type view =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `List of t list
  | `Assoc of (string * t) list ]
```

```ocaml
val classify : t -> view
```

```ocaml
val declassify : view -> t
```

```ocaml
val to_string : t -> string
```

```ocaml
val of_string : string -> (t, string) result
```
