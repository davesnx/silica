  $ ../standalone.exe --impl unknown.ml -o out.ml
  $ ocamlformat --enable-outside-detected-project --impl out.ml
  let a =
    [%ocaml.error
      "[silica] The attribute 'type_' is not valid on a 'select' element.\n\
       Hint: Maybe you mean 'typeof'?\n\n\
       If this is not correct, please open an issue at \
       https://github.com/davesnx/silica/issues."]
  $ ../standalone.exe --impl onclick.ml -o out.ml
  $ ocamlformat --enable-outside-detected-project --impl out.ml
  let a =
    [%ocaml.error
      "[silica] The DOM event attribute 'onclick' is not supported. Use \
       'on_press' with an event value instead."]
  
  let b =
    [%ocaml.error
      "[silica] The DOM event attribute 'onmouseover' is not supported and \
       Silica has no equivalent. Supported events: on_press, on_double_press, \
       on_submit, on_focus, on_blur, on_input, on_key_down, on_key_up, \
       on_pointer_enter, on_pointer_leave."]
  $ ../standalone.exe --impl modifiers.ml -o out.ml
  $ ocamlformat --enable-outside-detected-project --impl out.ml
  let a =
    [%ocaml.error
      "[silica] The attribute 'on_press_prevent' is not supported: only \
       on_key_down, on_key_up, on_pointer_enter and on_pointer_leave take a \
       prevent flag."]
  
  let b =
    [%ocaml.error
      "[silica] The attribute 'on_key_down_rate' needs 'on_key_down' on the same \
       element."]
  $ ../standalone.exe --impl same_name.ml -o out.ml
  File "same_name.ml", line 1, characters 18-47:
  1 | type state = Open | Closed [@state_name "open"] [@@deriving state_shape]
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [silica] the states Open and Closed have the same name "open"
  [1]
  $ ../standalone.exe --impl no_path.ml -o out.ml
  File "no_path.ml", line 1, characters 38-52:
  1 | type route = Home [@route Route.root] | Issue of int [@@deriving routes]
                                            ^^^^^^^^^^^^^^
  Error: [silica] the route Issue needs a path: [@route Route.(...)]
  [1]
  $ ../standalone.exe --impl gadt_state.ml -o out.ml
  File "gadt_state.ml", line 1, characters 15-32:
  1 | type _ state = Idle : unit state | Loaded : int -> int state
                     ^^^^^^^^^^^^^^^^^
  Error: [silica] [@@deriving state_shape] needs a plain variant; Idle is a GADT constructor
  [1]
  $ ../standalone.exe --impl empty_state.ml -o out.ml
  File "empty_state.ml", lines 1-2, characters 0-24:
  1 | type state = |
  2 | [@@deriving state_shape]
  Error: [silica] [@@deriving state_shape] needs at least one state
  [1]
  $ ../standalone.exe --impl parameter_state.ml -o out.ml
  File "parameter_state.ml", line 1, characters 0-69:
  1 | type 'data state = Waiting | Loaded of 'data [@@deriving state_shape]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [silica] [@@deriving state_shape] needs a state type with no parameter: a shape is one value for one type
  [1]
