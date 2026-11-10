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
  props : props;
  query : string;
  highlighted : string option;
  selected : string option;
}

type event =
  | Props of props
  | Typed of string
  | Move of [ `Next | `Previous | `First | `Last ]
  | Show
  | Highlight of string
  | Choose_highlighted
  | Choose of string
  | Escape
  | Outside_press
  | Focus_out

module State = struct
  type _ t = Idle : unit t | Closed : data t | Open : data t | Inline : data t
end

module M = Machine.Make (State)

type snapshot = M.snapshot = At : 'data State.t * 'data -> snapshot

let input_id (props : props) = props.id ^ "-input"
let listbox_id (props : props) = props.id ^ "-listbox"
let option_id (props : props) index =
  props.id ^ "-option-" ^ string_of_int index
let input_ref props = Ref.make (input_id props)

let template_input = Ref.make "input"

and template_popup = Ref.make "popup"

let sources =
  List.map
    (Popup.scoped (fun (data : data) -> data.props.id))
    [
      Source.dismiss ~outside:Outside_press ~focus_out:Focus_out
        [ template_input; template_popup ];
      Source.anchor ~anchor:template_input template_popup;
    ]

let shown data =
  List.filter
    (fun (_, item) -> matches data.query item)
    (List.mapi (fun index item -> (index, item)) data.props.items)

let enabled data =
  List.filter_map
    (fun (_, item) ->
      if item.disabled then
        None
      else
        Some item.value
    )
    (shown data)

let index_of data value =
  List.find_map
    (fun (index, item) ->
      if item.value = value then
        Some index
      else
        None
    )
    (List.mapi (fun index item -> (index, item)) data.props.items)

let valid data =
  match data.highlighted with
  | Some value when List.mem value (enabled data) ->
      data
  | _ ->
      { data with highlighted = None }

let auto data =
  match (valid data).highlighted with
  | Some _ ->
      data
  | None ->
      { data with highlighted = List.nth_opt (enabled data) 0 }

let rec position value i = function
  | [] ->
      None
  | v :: rest ->
      if v = value then
        Some i
      else
        position value (i + 1) rest

let move (direction : [ `Next | `Previous | `First | `Last ]) data =
  let values = enabled data in
  let count = List.length values in
  let target =
    match ((valid data).highlighted, direction) with
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
  let cmds =
    match Option.bind highlighted (index_of data) with
    | Some index ->
        [ Cmd.Scroll_into_view (Ref.make (option_id data.props index)) ]
    | None ->
        []
  in
  ({ data with highlighted }, cmds)

let choose data value =
  match List.find_opt (fun item -> item.value = value) data.props.items with
  | Some item when not item.disabled ->
      Some
        {
          data with
          query = item.label;
          selected = Some value;
          highlighted = None;
        }
  | _ ->
      None

let start props = { props; query = ""; highlighted = None; selected = None }

let settle data =
  if data.props.inline then
    M.goto Inline (auto data)
  else
    M.goto Closed { data with highlighted = None }

let machine =
  M.make ~id:"combobox" ~init:(M.at Idle ())
    [
      M.state Idle "idle" ~to_:[ Closed; Inline ] (fun () -> function
        | Props props ->
            settle (start props)
        | _ ->
            M.unhandled
        );
      M.state Closed "closed" ~to_:[ Open; Inline ] (fun data -> function
        | Props props ->
            settle { data with props }
        | Typed query ->
            M.goto Open { data with query; highlighted = None }
        | Move direction ->
            let data, cmds = move direction data in
            M.goto Open data ~cmds
        | Show ->
            M.goto Open data
        | Escape when data.query <> "" ->
            M.goto Closed { data with query = "" }
        | Escape
        | Highlight _
        | Choose_highlighted
        | Choose _
        | Outside_press
        | Focus_out ->
            M.unhandled
        );
      M.state Open "open" ~to_:[ Closed; Inline ] ~invoke:sources
        (fun data -> function
        | Props props when props.inline ->
            settle { data with props }
        | Props props ->
            M.goto Open (valid { data with props })
        | Typed query ->
            M.goto Open { data with query; highlighted = None }
        | Move direction ->
            let data, cmds = move direction data in
            M.goto Open data ~cmds
        | Highlight value ->
            M.goto Open (valid { data with highlighted = Some value })
        | Choose_highlighted -> (
            match Option.bind data.highlighted (choose data) with
            | Some data ->
                M.goto Closed data
            | None ->
                M.unhandled
          )
        | Choose value -> (
            match choose data value with
            | Some data ->
                M.goto Closed data ~cmds:[ Cmd.Focus (input_ref data.props) ]
            | None ->
                M.unhandled
          )
        | Escape | Outside_press | Focus_out ->
            M.goto Closed { data with highlighted = None }
        | Show ->
            M.unhandled
        );
      M.state Inline "inline" ~to_:[ Closed ] (fun data -> function
        | Props props ->
            settle { data with props }
        | Typed query ->
            M.goto Inline (auto { data with query; highlighted = None })
        | Move direction ->
            let data, cmds = move direction data in
            M.goto Inline data ~cmds
        | Highlight value ->
            M.goto Inline (auto { data with highlighted = Some value })
        | Choose_highlighted -> (
            match Option.bind data.highlighted (choose data) with
            | Some data ->
                M.goto Inline (auto data)
            | None ->
                M.unhandled
          )
        | Choose value -> (
            match choose data value with
            | Some data ->
                M.goto Inline (auto data)
                  ~cmds:[ Cmd.Focus (input_ref data.props) ]
            | None ->
                M.unhandled
          )
        | Escape when data.query <> "" ->
            M.goto Inline (auto { data with query = ""; highlighted = None })
        | Escape | Show | Outside_press | Focus_out ->
            M.unhandled
        );
    ]

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

let input data ~popup_open ~shown =
  let props = data.props in
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
          ( match Option.bind data.highlighted (index_of data) with
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

let option_ data (index, item) =
  let props = data.props in
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

let popup data =
  let props = data.props in
  let options = shown data in
  el "div"
    ([
       Attr.string "id" (props.id ^ "-popup");
       Attr.ref_ (Ref.make (props.id ^ "-popup"));
     ]
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
        (List.map (option_ data) options);
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

let root data children =
  el "div" (Popup.class_attr data.props.classes.root) children

let view : snapshot -> event Node.t = function
  | At (Idle, ()) ->
      Node.list []
  | At (Closed, data) ->
      root data [ input data ~popup_open:false ~shown:false ]
  | At (Open, data) ->
      root data
        [ input data ~popup_open:true ~shown:true; Node.portal [ popup data ] ]
  | At (Inline, data) ->
      root data [ input data ~popup_open:false ~shown:true; popup data ]

let component = Component.make machine view

let props ~id ~label ?(placeholder = "") ?(empty = "No results")
    ?(inline = false) ?(classes = no_classes) items =
  { id; label; placeholder; empty; items; inline; classes }

let selected : snapshot -> string option = function
  | At (Idle, ()) ->
      None
  | At (Closed, data) ->
      data.selected
  | At (Open, data) ->
      data.selected
  | At (Inline, data) ->
      data.selected

let make ?key ?on_value_change props =
  let output event snapshot =
    match (event, on_value_change, selected snapshot) with
    | (Choose _ | Choose_highlighted), Some f, Some value ->
        [ f value ]
    | _ ->
        []
  in
  Component.node ?key ~props:(Props props) ~output component
