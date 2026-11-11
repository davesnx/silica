open Ppxlib
open Ast_builder.Default
module List = ListLabels

let issues_url = "https://github.com/davesnx/silica/issues"

(* There's no pexp_list on Ppxlib since is not a constructor of the Parsetree *)
let pexp_list ~loc xs =
  List.fold_left (List.rev xs) ~init:[%expr []] ~f:(fun xs x ->
      [%expr [%e x] :: [%e xs]]
  )

exception Error of expression

let raise_errorf ~loc fmt =
  let open Ast_builder.Default in
  Printf.ksprintf
    (fun msg ->
      let expr =
        pexp_extension ~loc (Location.error_extensionf ~loc "[silica] %s" msg)
      in
      raise (Error expr)
    )
    fmt

let rec unwrap_children ~f children = function
  | { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ } ->
      List.rev children
  | {
      pexp_desc =
        Pexp_construct
          ( { txt = Lident "::"; _ },
            Some { pexp_desc = Pexp_tuple [ child; next ]; _ }
          );
      _;
    } ->
      unwrap_children ~f (f child :: children) next
  | e ->
      raise_errorf ~loc:e.pexp_loc "children prop should be a list"

let is_jsx = function
  | { attr_name = { txt = "JSX"; _ }; _ } ->
      true
  | _ ->
      false

let has_jsx_attr attrs = List.exists ~f:is_jsx attrs

let rewrite_component ~loc tag args children =
  let component = pexp_ident ~loc tag in
  let props =
    match children with
    | None ->
        args
    | Some [ children ] ->
        (Labelled "children", children) :: args
    | Some children ->
        ( Labelled "children",
          [%expr Silica.Node.list [%e pexp_list ~loc children]]
        )
        :: args
  in
  pexp_apply ~loc component props

let validate_attr ~loc id name =
  match Html.findByName id name with
  | Ok p ->
      p
  | Error `ElementNotFound ->
      raise_errorf ~loc
        {|HTML tag '%s' doesn't exist.

If this is not correct, please open an issue at %s|}
        id issues_url
  | Error (`AttributeNotFound suggestion) ->
      let suggestion =
        match suggestion with
        | Some suggestion ->
            Printf.sprintf "Hint: Maybe you mean '%s'?\n" suggestion
        | None ->
            ""
      in

      raise_errorf ~loc
        {|The attribute '%s' is not valid on a '%s' element.
%s
If this is not correct, please open an issue at %s.|}
        name id suggestion issues_url

let build_polyvariant_type ~loc options =
  let row_fields =
    List.map options ~f:(fun (opt : Html_attributes.polyvariant) ->
        {
          prf_desc = Rtag ({ loc; txt = opt.type_ }, true, []);
          prf_loc = loc;
          prf_attributes = [];
        }
    )
  in
  ptyp_variant ~loc row_fields Closed None

let add_attribute_type_constraint ~loc ~is_optional
    (type_ : Html_attributes.kind) value =
  match (type_, is_optional) with
  | String, true ->
      [%expr ([%e value] : string option)]
  | String, false ->
      [%expr ([%e value] : string)]
  | Int, false ->
      [%expr ([%e value] : int)]
  | Int, true ->
      [%expr ([%e value] : int option)]
  | Bool, false ->
      [%expr ([%e value] : bool)]
  | Bool, true ->
      [%expr ([%e value] : bool option)]
  | BooleanishString, false ->
      [%expr ([%e value] : bool)]
  | BooleanishString, true ->
      [%expr ([%e value] : bool option)]
  | Polyvariant options, false ->
      let poly_type = build_polyvariant_type ~loc options in
      pexp_constraint ~loc value poly_type
  | Polyvariant options, true ->
      let poly_type = build_polyvariant_type ~loc options in
      let option_type =
        ptyp_constr ~loc { loc; txt = Lident "option" } [ poly_type ]
      in
      pexp_constraint ~loc value option_type

let polyvariant_to_string_match ~loc options scrutinee =
  let poly_type = build_polyvariant_type ~loc options in
  let constrained = pexp_constraint ~loc scrutinee poly_type in
  let cases =
    List.map options ~f:(fun (opt : Html_attributes.polyvariant) ->
        case
          ~lhs:(ppat_variant ~loc opt.type_ None)
          ~guard:None ~rhs:(estring ~loc opt.jsxName)
    )
  in
  let match_expr = pexp_match ~loc constrained cases in
  {
    match_expr with
    pexp_attributes =
      {
        attr_name = { loc; txt = "warning" };
        attr_payload = PStr [ pstr_eval ~loc (estring ~loc "-8") [] ];
        attr_loc = loc;
      }
      :: match_expr.pexp_attributes;
  }

let silica_attribute = function
  | ( "on_press"
    | "on_double_press"
    | "on_submit"
    | "on_focus"
    | "on_blur"
    | "on_input"
    | "on_key_down"
    | "on_key_up"
    | "on_pointer_enter"
    | "on_pointer_leave"
    | "on_reset"
    | "style" ) as name ->
      Some name
  | "ref" ->
      Some "ref_"
  | _ ->
      None

let is_event name =
  match silica_attribute name with
  | Some ("style" | "ref_") | None ->
      false
  | Some _ ->
      true

let takes_prevent = function
  | "on_key_down" | "on_key_up" | "on_pointer_enter" | "on_pointer_leave" ->
      true
  | _ ->
      false

let takes_rate name = String.equal name "on_input" || takes_prevent name

type modifier = Prevent | Rate

let modifier_of_name name =
  let strip suffix =
    let n = String.length name and k = String.length suffix in
    if n > k && String.equal (String.sub name (n - k) k) suffix then
      Some (String.sub name 0 (n - k))
    else
      None
  in
  match (strip "_prevent", strip "_rate") with
  | Some base, _ when is_event base ->
      Some (base, Prevent)
  | _, Some base when is_event base ->
      Some (base, Rate)
  | _ ->
      None

let silica_event_hint = function
  | "onclick" ->
      Some "on_press"
  | "ondblclick" ->
      Some "on_double_press"
  | "onsubmit" ->
      Some "on_submit"
  | "onfocus" ->
      Some "on_focus"
  | "onblur" ->
      Some "on_blur"
  | "oninput" | "onchange" ->
      Some "on_input"
  | "onkeydown" ->
      Some "on_key_down"
  | "onkeyup" ->
      Some "on_key_up"
  | "onpointerenter" | "onmouseenter" ->
      Some "on_pointer_enter"
  | "onpointerleave" | "onmouseleave" ->
      Some "on_pointer_leave"
  | _ ->
      None

let reject_event ~loc name =
  match silica_event_hint name with
  | Some hint ->
      raise_errorf ~loc
        "The DOM event attribute '%s' is not supported. Use '%s' with an event \
         value instead."
        name hint
  | None ->
      raise_errorf ~loc
        "The DOM event attribute '%s' is not supported and Silica has no \
         equivalent. Supported events: on_press, on_double_press, on_submit, \
         on_focus, on_blur, on_input, on_key_down, on_key_up, \
         on_pointer_enter, on_pointer_leave."
        name

let build_attribute ~loc ~is_optional build value =
  if is_optional then
    [%expr Stdlib.Option.map (fun v -> [%e build [%expr v]]) [%e value]]
  else
    build value

let modifier_args ~loc:_ modifiers kind label_name =
  List.filter_map modifiers ~f:(fun (found, label, value) ->
      if found = kind then
        let label =
          match label with
          | Optional _ ->
              Optional label_name
          | Nolabel | Labelled _ ->
              Labelled label_name
        in
        Some (label, value)
      else
        None
  )

let silica_attribute_expr ~loc ~modifiers fn value =
  let prevent = modifier_args ~loc modifiers Prevent "prevent" in
  let rate = modifier_args ~loc modifiers Rate "rate" in
  if String.equal fn "on_reset" then
    value
  else if takes_rate fn then
    pexp_apply ~loc
      (evar ~loc ("Silica.Attr." ^ fn))
      (prevent @ rate @ [ (Nolabel, value) ])
  else
    let attr =
      pexp_apply ~loc (evar ~loc ("Silica.Attr." ^ fn)) [ (Nolabel, value) ]
    in
    match rate with
    | [] ->
        attr
    | (Optional _, rate) :: _ ->
        [%expr
          let silica_attr = [%e attr] in
          match [%e rate] with
          | Some silica_rate ->
              Silica.Attr.rated silica_rate silica_attr
          | None ->
              silica_attr]
    | (_, rate) :: _ ->
        [%expr Silica.Attr.rated [%e rate] [%e attr]]

let html_attribute_expr ~loc (kind : Html_attributes.kind) name value =
  match kind with
  | String ->
      [%expr Silica.Attr.string [%e name] [%e value]]
  | Int ->
      [%expr Silica.Attr.string [%e name] (Stdlib.string_of_int [%e value])]
  | Bool ->
      [%expr Silica.Attr.bool [%e name] [%e value]]
  | BooleanishString ->
      [%expr Silica.Attr.string [%e name] (Stdlib.Bool.to_string [%e value])]
  | Polyvariant options ->
      [%expr
        Silica.Attr.string [%e name]
          [%e polyvariant_to_string_match ~loc options value]]

let is_optional = function Optional _ -> true | _ -> false

let transform_labelled ~loc:_parentLoc ~tag_name ~wrap_some ~modifiers props
    (prop_label, value) =
  let loc = props.pexp_loc in
  match prop_label with
  | Nolabel ->
      props
  | Optional name | Labelled name ->
      let is_optional = is_optional prop_label in
      let attribute_final =
        match silica_attribute name with
        | Some fn ->
            let value =
              if String.equal fn "style" then
                add_attribute_type_constraint ~loc ~is_optional String value
              else
                value
            in
            let modifiers =
              List.filter_map modifiers ~f:(fun (base, kind, label, value) ->
                  if String.equal base name then
                    Some (kind, label, value)
                  else
                    None
              )
            in
            build_attribute ~loc ~is_optional
              (silica_attribute_expr ~loc ~modifiers fn)
              value
        | None -> (
            match validate_attr ~loc tag_name name with
            | Event _ ->
                reject_event ~loc name
            | (Rich_attribute { type_; _ } | Attribute { type_; _ }) as
              attribute ->
                let name_expr = estring ~loc (Html.getName attribute) in
                let value =
                  add_attribute_type_constraint ~loc ~is_optional type_ value
                in
                build_attribute ~loc ~is_optional
                  (html_attribute_expr ~loc type_ name_expr)
                  value
          )
      in
      let cell =
        if is_optional || not wrap_some then
          attribute_final
        else
          [%expr Some [%e attribute_final]]
      in
      [%expr [%e cell] :: [%e props]]

let label_name = function
  | Nolabel ->
      None
  | Labelled name | Optional name ->
      Some name

let transform_attributes ~loc ~tag_name attrs =
  let modifiers =
    List.filter_map attrs ~f:(fun (label, value) ->
        match Option.bind (label_name label) modifier_of_name with
        | Some (base, kind) ->
            Some (base, kind, label, value)
        | None ->
            None
    )
  in
  List.iter modifiers ~f:(fun (base, kind, label, value) ->
      let loc = value.pexp_loc in
      let name = Option.value (label_name label) ~default:"" in
      if
        not
          (List.exists attrs ~f:(fun (other, _) -> label_name other = Some base))
      then
        raise_errorf ~loc "The attribute '%s' needs '%s' on the same element."
          name base;
      if kind = Prevent && not (takes_prevent base) then
        raise_errorf ~loc
          "The attribute '%s' is not supported: only on_key_down, on_key_up, \
           on_pointer_enter and on_pointer_leave take a prevent flag."
          name
  );
  let attrs =
    List.filter attrs ~f:(fun (label, _) ->
        Option.bind (label_name label) modifier_of_name = None
    )
  in
  let has_optional =
    List.exists attrs ~f:(fun (label, _) -> is_optional label)
  in
  let attrs_expr =
    List.rev attrs
    |> List.fold_left
         ~f:
           (transform_labelled ~loc ~tag_name ~wrap_some:has_optional ~modifiers)
         ~init:[%expr []]
  in
  match attrs_expr with
  | [%expr []] ->
      [%expr []]
  | attrs_expr when has_optional ->
      [%expr Stdlib.List.filter_map Stdlib.Fun.id [%e attrs_expr]]
  | attrs_expr ->
      attrs_expr

let rewrite_node ~loc tag_name args children =
  let dom_node_name = estring ~loc tag_name in
  let attributes = transform_attributes ~loc ~tag_name args in
  let children = pexp_list ~loc (Option.value children ~default:[]) in
  [%expr Silica.Node.element [%e dom_node_name] [%e attributes] [%e children]]

let text_child e =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string _) ->
      let loc = e.pexp_loc in
      [%expr Silica.Node.text [%e e]]
  | _ ->
      e

let split_args ~mapper args =
  let children = ref (Location.none, []) in
  let rest =
    List.filter_map args ~f:(function
      | Labelled "children", children_expression ->
          let children' =
            unwrap_children []
              ~f:(fun e -> mapper (text_child e))
              children_expression
          in
          children := (children_expression.pexp_loc, children');
          None
      | arg_label, expression ->
          Some (arg_label, mapper expression)
      )
  in
  let children_prop =
    match !children with _, [] -> None | _loc, children -> Some children
  in
  (children_prop, rest)

let reverse_pexp_list ~loc expr =
  let rec go acc = function
    | [%expr []] ->
        acc
    | [%expr [%e? hd] :: [%e? tl]] ->
        go [%expr [%e hd] :: [%e acc]] tl
    | expr ->
        expr
  in
  go [%expr []] expr

let list_have_tail expr =
  match expr with
  | Pexp_construct
      ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple _; _ })
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) ->
      false
  | _ ->
      true

let transform_items_of_list ~loc ~mapper children =
  let rec run_mapper children accum =
    match children with
    | [%expr []] ->
        reverse_pexp_list ~loc accum
    | [%expr [%e? v] :: [%e? acc]] when list_have_tail acc.pexp_desc ->
        [%expr [%e mapper#expression (text_child v)]]
    | [%expr [%e? v] :: [%e? acc]] ->
        run_mapper acc
          [%expr [%e mapper#expression (text_child v)] :: [%e accum]]
    | notAList ->
        mapper#expression notAList
  in
  run_mapper children [%expr []]

let rewrite_jsx =
  object (self)
    inherit Ast_traverse.map as super

    method! expression expr =
      try
        match expr.pexp_desc with
        | Pexp_apply (({ pexp_desc = Pexp_ident _; _ } as tag), args)
          when has_jsx_attr expr.pexp_attributes -> (
            let children, rest_of_args =
              split_args ~mapper:self#expression args
            in
            match tag.pexp_desc with
            (* div() [@JSX] *)
            | Pexp_ident { txt = Lident name; loc = name_loc }
              when Html.is_html_element name || Html.is_svg_element name ->
                rewrite_node ~loc:name_loc name rest_of_args children
            (* Reason adds `createElement` as default when an uppercase is found,
               we change it back to make *)
            (* Foo.createElement() [@JSX] *)
            | Pexp_ident
                { txt = Ldot (modulePath, ("createElement" | "make")); loc } ->
                let id = { loc; txt = Ldot (modulePath, "make") } in
                rewrite_component ~loc:tag.pexp_loc id rest_of_args children
            (* local_function() [@JSX] *)
            | Pexp_ident id ->
                rewrite_component ~loc:tag.pexp_loc id rest_of_args children
            | _ ->
                assert false
          )
        (* div() [@JSX] *)
        | Pexp_apply (_tag, _props) when has_jsx_attr expr.pexp_attributes ->
            raise_errorf ~loc:expr.pexp_loc "tag should be an identifier"
        (* <> </> is represented as a list in the Parsetree with [@JSX] *)
        | Pexp_construct
            ({ txt = Lident "::"; loc }, Some { pexp_desc = Pexp_tuple _; _ })
        | Pexp_construct ({ txt = Lident "[]"; loc }, None) -> (
            match List.partition ~f:is_jsx expr.pexp_attributes with
            | [], _ ->
                super#expression expr
            | _ ->
                [%expr
                  Silica.Node.list
                    [%e transform_items_of_list ~loc ~mapper:self expr]]
          )
        | _ ->
            super#expression expr
      with Error err -> [%expr [%e err]]
  end

let () =
  Driver.register_transformation "silica.ppx"
    ~preprocess_impl:rewrite_jsx#structure

let () =
  ignore
    ( (Shape_deriver.state_deriver, Shape_deriver.event_deriver)
      : Deriving.t * Deriving.t
      )
let () = ignore (Routes.deriver : Deriving.t)

module Svg_names = Svg_names

let svg_attributes () =
  Stdlib.List.concat_map
    (fun (element : Html_attributes.element) ->
      Stdlib.List.filter_map
        (function
          | Html_attributes.Event _ ->
              None
          | prop ->
              Some (Html.getJSXName prop, Html.getName prop)
          )
        element.attributes
    )
    Html.svgElements
  |> Stdlib.List.sort_uniq Stdlib.compare
