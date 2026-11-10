  $ ../standalone.exe --impl input.ml -o out.ml
  $ ocamlformat --enable-outside-detected-project --impl out.ml
  let element =
    Silica.Node.element "div"
      [
        Silica.Attr.string "class" ("box" : string);
        Silica.Attr.string "tabindex" (Stdlib.string_of_int (1 : int));
        Silica.Attr.bool "hidden" (true : bool);
      ]
      [ Silica.Node.text "hi" ]
  
  let nested =
    Silica.Node.element "ul" []
      [
        Silica.Node.element "li" [] [ Silica.Node.text "one" ];
        Silica.Node.element "li" [] [ text ];
      ]
  
  let fragment =
    Silica.Node.list
      [
        Silica.Node.element "span" [] [ Silica.Node.text "a" ];
        Silica.Node.text "b";
      ]
  
  let empty_fragment = Silica.Node.list []
  
  let component =
    Card.make
      ~children:(Silica.Node.list [ Silica.Node.text "x"; y ])
      ~title:"t" ()
  
  let single_child = Card.make ~children:z ()
  
  let svg_attr =
    Silica.Node.element "svg"
      [ Silica.Attr.string "viewBox" ("0 0 1 1" : string) ]
      []
  
  let optional =
    Silica.Node.element "div"
      (Stdlib.List.filter_map Stdlib.Fun.id
         [
           Stdlib.Option.map
             (fun v -> Silica.Attr.string "id" v)
             (id : string option);
           Some (Silica.Attr.string "class" ("a" : string));
         ])
      []
  
  let press = Silica.Node.element "button" [ Silica.Attr.on_press Clicked ] []
  
  let double =
    Silica.Node.element "button" [ Silica.Attr.on_double_press Edit ] []
  
  let submit = Silica.Node.element "form" [ Silica.Attr.on_submit Save ] []
  
  let focus =
    Silica.Node.element "input"
      [ Silica.Attr.on_focus Focused; Silica.Attr.on_blur Blurred ]
      []
  
  let on_input =
    Silica.Node.element "input" [ Silica.Attr.on_input (fun s -> Typed s) ] []
  
  let ref_ = Silica.Node.element "input" [ Silica.Attr.ref_ name_ref ] []
  
  let style =
    Silica.Node.element "div" [ Silica.Attr.style ("color: red" : string) ] []
  
  let optional_press =
    Silica.Node.element "button"
      (Stdlib.List.filter_map Stdlib.Fun.id
         [ Stdlib.Option.map (fun v -> Silica.Attr.on_press v) on_press ])
      []
  
  let svg_xlink =
    Silica.Node.element "use"
      [ Silica.Attr.string "xlink:href" ("#a" : string) ]
      []
  
  let svg_dashed =
    Silica.Node.element "path"
      [
        Silica.Attr.string "clip-path" ("url(#c)" : string);
        Silica.Attr.string "fill-opacity" ("0.5" : string);
        Silica.Attr.string "stroke-width" ("2" : string);
      ]
      []
  
  let key_down =
    Silica.Node.element "input"
      [ Silica.Attr.on_key_down (fun key -> Some (Down key)) ]
      []
  
  let key_up =
    Silica.Node.element "input"
      [ Silica.Attr.on_key_up (fun key -> Some (Up key)) ]
      []
  
  let pointer =
    Silica.Node.element "div"
      [
        Silica.Attr.on_pointer_enter (fun p -> Some (In p));
        Silica.Attr.on_pointer_leave (fun p -> Some (Out p));
      ]
      []
  
  let reset_button = Silica.Node.element "button" [ reset ] []
  
  let key_down_prevent =
    Silica.Node.element "input"
      [
        Silica.Attr.on_key_down ~prevent:true ~rate:(Silica.Rate.Throttle 50)
          (fun key -> Some (Down key));
      ]
      []
  
  let pointer_optional =
    Silica.Node.element "div"
      [ Silica.Attr.on_pointer_enter ?prevent ?rate (fun p -> Some (In p)) ]
      []
  
  let input_rate =
    Silica.Node.element "input"
      [ Silica.Attr.on_input ~rate:(Silica.Rate.Debounce 30) (fun s -> Typed s) ]
      []
  
  let press_rate =
    Silica.Node.element "button"
      [
        Silica.Attr.rated (Silica.Rate.Throttle 100)
          (Silica.Attr.on_press Clicked);
      ]
      []
  
  let optional_press_rate =
    Silica.Node.element "button"
      (Stdlib.List.filter_map Stdlib.Fun.id
         [
           Stdlib.Option.map
             (fun v ->
               let silica_attr = Silica.Attr.on_press v in
               match rate with
               | Some silica_rate -> Silica.Attr.rated silica_rate silica_attr
               | None -> silica_attr)
             on_press;
         ])
      []
