open Silica

type item = {
  value : string;
  label : string;
  keywords : string list;
  disabled : bool;
}

let item ?(keywords = []) ?(disabled = false) value label =
  { value; label; keywords; disabled }

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let matches query item =
  let query = String.lowercase_ascii query in
  List.exists
    (fun text -> contains (String.lowercase_ascii text) query)
    (item.label :: item.keywords)

type classes = {
  root : string;
  input : string;
  list : string;
  option : string;
  empty : string;
}

let no_classes = { root = ""; input = ""; list = ""; option = ""; empty = "" }

type props = {
  id : string;
  label : string;
  placeholder : string;
  empty : string;
  items : item list;
  inline : bool;
  classes : classes;
}

type data = {
  query : string;
  highlighted : string option;
  selected : string option;
}

type event =
  | Typed of string
  | Move of [ `Next | `Previous | `First | `Last ]
  | Show
  | Highlight of string
  | Choose_highlighted
  | Choose of string
  | Escape
  | Outside_press
  | Focus_out

type state = Closed of data | Open of data | Inline of data
[@@deriving fingerprint]

let name = function
  | Closed _ ->
      "closed"
  | Open _ ->
      "open"
  | Inline _ ->
      "inline"

let states = [ "closed"; "open"; "inline" ]

let event_name = function
  | Typed _ ->
      "typed"
  | Move `Next ->
      "move next"
  | Move `Previous ->
      "move previous"
  | Move `First ->
      "move first"
  | Move `Last ->
      "move last"
  | Show ->
      "show"
  | Highlight _ ->
      "highlight"
  | Choose_highlighted ->
      "choose_highlighted"
  | Choose _ ->
      "choose"
  | Escape ->
      "escape"
  | Outside_press ->
      "outside_press"
  | Focus_out ->
      "focus_out"

let input_id (props : props) = props.id ^ "-input"
let listbox_id (props : props) = props.id ^ "-listbox"
let option_id (props : props) index =
  props.id ^ "-option-" ^ string_of_int index
let input_ref props = Ref.make (input_id props)

let parts (props : props) =
  {
    (Popup.parts props.id) with
    trigger = input_ref props;
    trigger_id = input_id props;
  }

let shown (props : props) data =
  List.filter
    (fun (_, item) -> matches data.query item)
    (List.mapi (fun index item -> (index, item)) props.items)

let enabled props data =
  List.filter_map
    (fun (_, item) ->
      if item.disabled then
        None
      else
        Some item.value
    )
    (shown props data)

let index_of (props : props) value =
  List.find_map
    (fun (index, item) ->
      if item.value = value then
        Some index
      else
        None
    )
    (List.mapi (fun index item -> (index, item)) props.items)

let valid props data =
  match data.highlighted with
  | Some value when List.mem value (enabled props data) ->
      data
  | Some _ | None ->
      { data with highlighted = None }

let auto props data =
  match (valid props data).highlighted with
  | Some _ ->
      data
  | None ->
      { data with highlighted = List.nth_opt (enabled props data) 0 }

let rec position value i = function
  | [] ->
      None
  | v :: rest ->
      if v = value then
        Some i
      else
        position value (i + 1) rest

let move props (direction : [ `Next | `Previous | `First | `Last ]) data =
  let values = enabled props data in
  let count = List.length values in
  let target =
    match ((valid props data).highlighted, direction) with
    | _, _ when count = 0 ->
        None
    | _, `First ->
        Some 0
    | _, `Last ->
        Some (count - 1)
    | None, `Next ->
        Some 0
    | None, `Previous ->
        Some (count - 1)
    | Some value, `Next ->
        Option.map (fun i -> (i + 1) mod count) (position value 0 values)
    | Some value, `Previous ->
        Option.map (fun i -> (i + count - 1) mod count) (position value 0 values)
  in
  let highlighted = Option.map (List.nth values) target in
  let actions =
    match Option.bind highlighted (index_of props) with
    | Some index ->
        [ Action.Scroll_into_view (Ref.make (option_id props index)) ]
    | None ->
        []
  in
  ({ data with highlighted }, actions)

let choose (props : props) value =
  match List.find_opt (fun item -> item.value = value) props.items with
  | Some item when not item.disabled ->
      Some { query = item.label; selected = Some value; highlighted = None }
  | Some _ | None ->
      None

let settle (props : props) data : (state, event) Machine.next =
  if props.inline then
    Go (Inline (auto props data))
  else
    Go (Closed { data with highlighted = None })

let machine =
  Machine.make_with_props ~id:"combobox"
    ~init:(Closed { query = ""; highlighted = None; selected = None })
    ~name ~states ~event_name ~fingerprint:state_fingerprint
    ~during:(fun props -> function
      | Open _ ->
          Popup.layer ~outside:Outside_press ~focus_out:Focus_out (parts props)
      | Closed _ | Inline _ ->
          []
      )
    ~on_props:(fun _ (props : props) -> function
      | Open data when not props.inline ->
          Go (Open (valid props data))
      | Closed data | Open data | Inline data ->
          settle props data
      )
    (fun props state event ->
      match (state, event) with
      | (Closed data | Open data), Typed query ->
          Go (Open { data with query; highlighted = None })
      | (Closed data | Open data), Move direction ->
          let data, actions = move props direction data in
          Go_with (Open data, actions)
      | Closed data, Show ->
          Go (Open data)
      | Closed data, Escape when data.query <> "" ->
          Go (Closed { data with query = "" })
      | Open data, Highlight value ->
          Go (Open (valid props { data with highlighted = Some value }))
      | Open data, Choose_highlighted -> (
          match Option.bind data.highlighted (choose props) with
          | Some data ->
              Go (Closed data)
          | None ->
              Ignore
        )
      | Open _, Choose value -> (
          match choose props value with
          | Some data ->
              Go_with (Closed data, [ Action.Focus (input_ref props) ])
          | None ->
              Ignore
        )
      | Open data, (Escape | Outside_press | Focus_out) ->
          Go (Closed { data with highlighted = None })
      | Inline data, Typed query ->
          Go (Inline (auto props { data with query; highlighted = None }))
      | Inline data, Move direction ->
          let data, actions = move props direction data in
          Go_with (Inline data, actions)
      | Inline data, Highlight value ->
          Go (Inline (auto props { data with highlighted = Some value }))
      | Inline data, Choose_highlighted -> (
          match Option.bind data.highlighted (choose props) with
          | Some data ->
              Go (Inline (auto props data))
          | None ->
              Ignore
        )
      | Inline _, Choose value -> (
          match choose props value with
          | Some data ->
              Go_with
                (Inline (auto props data), [ Action.Focus (input_ref props) ])
          | None ->
              Ignore
        )
      | Inline data, Escape when data.query <> "" ->
          Go (Inline (auto props { data with query = ""; highlighted = None }))
      | Closed _, Escape
      | Closed _, (Highlight _ | Choose_highlighted | Choose _)
      | Closed _, (Outside_press | Focus_out)
      | Open _, Show
      | Inline _, (Escape | Show | Outside_press | Focus_out) ->
          Ignore
    )

let el = Node.element
let flag name on =
  if on then
    [ Attr.string name "" ]
  else
    []

let keys data ~popup_open ~shown (key : Key.t) =
  match key.key with
  | "ArrowDown" when key.alt ->
      if shown then
        None
      else
        Some Show
  | "ArrowDown" ->
      Some (Move `Next)
  | "ArrowUp" ->
      Some (Move `Previous)
  | "Home" when shown && data.highlighted <> None ->
      Some (Move `First)
  | "End" when shown && data.highlighted <> None ->
      Some (Move `Last)
  | "Enter" when shown && data.highlighted <> None ->
      Some Choose_highlighted
  | "Escape" when popup_open || data.query <> "" ->
      Some Escape
  | _ ->
      None

let input props data ~popup_open ~shown =
  el "input"
    ([
       Attr.string "id" (input_id props);
       Attr.ref_ (input_ref props);
       Attr.string "type" "text";
       Attr.string "role" "combobox";
       Attr.string "aria-autocomplete" "list";
       Attr.string "aria-expanded" (string_of_bool shown);
       Attr.string "aria-label" props.label;
       Attr.string "autocomplete" "off";
       Attr.string "value" data.query;
       Attr.on_input (fun text -> Typed text);
       Attr.on_key_down ~prevent:true (keys data ~popup_open ~shown);
       Attr.on_key_down (fun key ->
           if popup_open && key.key = "Tab" then
             Some Focus_out
           else
             None
       );
     ]
    @ ( if props.placeholder = "" then
          []
        else
          [ Attr.string "placeholder" props.placeholder ]
      )
    @ ( if shown then
          Attr.string "aria-controls" (listbox_id props)
          ::
          ( match Option.bind data.highlighted (index_of props) with
          | Some index ->
              [ Attr.string "aria-activedescendant" (option_id props index) ]
          | None ->
              []
          )
        else
          []
      )
    @ Popup.class_attr props.classes.input
    )
    []

let option_ props data (index, item) =
  let id = option_id props index in
  let highlighted = data.highlighted = Some item.value
  and selected = data.selected = Some item.value in
  el "li"
    ([
       Attr.string "id" id;
       Attr.ref_ (Ref.make id);
       Attr.string "role" "option";
       Attr.string "aria-selected" (string_of_bool selected);
     ]
    @ ( if item.disabled then
          [ Attr.string "aria-disabled" "true"; Attr.string "data-disabled" "" ]
        else
          [
            Attr.on_press (Choose item.value);
            Attr.on_pointer_enter (fun _ -> Some (Highlight item.value));
          ]
      )
    @ flag "data-highlighted" highlighted
    @ flag "data-selected" selected
    @ Popup.class_attr props.classes.option
    )
    [ Node.text item.label ]

let popup props data =
  let options = shown props data in
  let parts = parts props in
  el "div"
    ([ Attr.string "id" parts.popup_id; Attr.ref_ parts.popup ]
    @ Popup.open_attrs true
    @ Popup.class_attr props.classes.list
    )
    [
      el "ul"
        [
          Attr.string "id" (listbox_id props);
          Attr.string "role" "listbox";
          Attr.string "aria-label" props.label;
        ]
        (List.map (option_ props data) options);
      el "div"
        ([
           Attr.string "id" (props.id ^ "-status"); Attr.string "role" "status";
         ]
        @ Popup.class_attr props.classes.empty
        )
        ( if options = [] then
            [ Node.text props.empty ]
          else
            []
        );
    ]

let root (props : props) children =
  el "div" (Popup.class_attr props.classes.root) children

let view props = function
  | Closed data ->
      root props [ input props data ~popup_open:false ~shown:false ]
  | Open data ->
      root props
        [
          input props data ~popup_open:true ~shown:true;
          Node.portal [ popup props data ];
        ]
  | Inline data ->
      root props
        [ input props data ~popup_open:false ~shown:true; popup props data ]

let component = Component.make_with_props machine view

let props ~id ~label ?(placeholder = "") ?(empty = "No results")
    ?(inline = false) ?(classes = no_classes) items =
  { id; label; placeholder; empty; items; inline; classes }

let make ?key ?on_value_change props =
  let output event (Closed data | Open data | Inline data) =
    match (event, on_value_change, data.selected) with
    | (Choose _ | Choose_highlighted), Some f, Some value ->
        [ f value ]
    | (Choose _ | Choose_highlighted), (Some _ | None), (Some _ | None)
    | ( ( Typed _
        | Move _
        | Show
        | Highlight _
        | Escape
        | Outside_press
        | Focus_out ),
        _,
        _ ) ->
        []
  in
  Component.node_with_props ?key ~props ~output component
