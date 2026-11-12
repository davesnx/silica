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
  $ ../standalone.exe --impl no_path.ml -o out.ml
  File "no_path.ml", line 1, characters 38-52:
  1 | type route = Home [@route Route.root] | Issue of int [@@deriving routes]
                                            ^^^^^^^^^^^^^^
  Error: [silica] the route Issue needs a path: [@route Route.(...)]
  [1]
