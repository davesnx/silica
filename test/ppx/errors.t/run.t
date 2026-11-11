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
  $ ../standalone.exe --impl same_event_name.ml -o out.ml
  File "same_event_name.ml", line 1, characters 18-46:
  1 | type event = Save | Saved [@event_name "save"] [@@deriving event_shape]
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [silica] the events Save and Saved have the same name "save"
  [1]
  $ ../standalone.exe --impl parameter_event.ml -o out.ml
  File "parameter_event.ml", line 1, characters 0-59:
  1 | type 'data event = Loaded of 'data [@@deriving event_shape]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [silica] [@@deriving event_shape] needs an event type with no parameter: a shape is one value for one type
  [1]
  $ ../standalone.exe --impl name_of_record.ml -o out.ml
  File "name_of_record.ml", line 1, characters 13-68:
  1 | type event = Saved of { at : int } [@event_name_of fun _ -> "saved"]
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [silica] [@event_name_of] needs a constructor with tuple data; Saved has a record
  [1]
  $ ../standalone.exe --impl name_of_constant.ml -o out.ml
  File "name_of_constant.ml", line 1, characters 13-52:
  1 | type event = Retry [@event_name_of fun _ -> "retry"] [@@deriving event_shape]
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [silica] [@event_name_of] needs a constructor with tuple data; Retry has none
  [1]
  $ ../standalone.exe --impl name_of_both.ml -o out.ml
  File "name_of_both.ml", line 1, characters 13-77:
  1 | type event = Moved of int [@event_name "move"] [@event_name_of string_of_int]
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [silica] Moved has both [@event_name] and [@event_name_of]; keep one
  [1]
