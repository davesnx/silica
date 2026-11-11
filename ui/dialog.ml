open Silica

type classes = {
  trigger : string;
  backdrop : string;
  popup : string;
  title : string;
  description : string;
  close : string;
}

let no_classes =
  {
    trigger = "";
    backdrop = "";
    popup = "";
    title = "";
    description = "";
    close = "";
  }

type ('content, 'ce) props = {
  id : string;
  title : string;
  description : string option;
  trigger : string option;
  close : string option;
  open_ : bool option;
  dismissible : bool;
  alert : bool;
  classes : classes;
  content : 'content;
}

type 'ce event =
  | Open_request of Popup.reason
  | Close_request of Popup.reason
  | Content of 'ce

type state = Closed | Open [@@deriving state_shape]

let parts (props : (_, _) props) = Popup.parts props.id

let dismisses (props : (_, _) props) = function
  | Popup.Escape_key ->
      props.dismissible
  | Popup.Outside_press ->
      props.dismissible && not props.alert
  | Popup.Trigger_press
  | Popup.Trigger_hover
  | Popup.Trigger_focus
  | Popup.Close_press
  | Popup.Item_press
  | Popup.Focus_out
  | Popup.List_navigation ->
      true

let close (props : (_, _) props) : (state, _ event) Machine.next =
  Popup.close ~focus:(Option.is_some props.trigger) (parts props) Closed

let event_shape content =
  Machine.shape
    ~name:(function
      | Open_request reason ->
          "open_request " ^ Popup.reason_name reason
      | Close_request reason ->
          "close_request " ^ Popup.reason_name reason
      | Content event -> (
          match content with
          | Some content ->
              Machine.shape_name content event
          | None ->
              "content"
        )
      )
    ()

let machine ~id ~closes ?events () =
  Machine.make_with_props ~id ~init:Closed ~shape:state_shape
    ~events:(event_shape events)
    ~owns:(fun props -> function
      | Open ->
          let parts = parts props in
          [
            Source.focus_trap parts.popup;
            Source.scroll_lock;
            Popup.dismiss ~escape:(Close_request Popup.Escape_key)
              ~outside:(Close_request Popup.Outside_press) parts;
          ]
      | Closed ->
          []
      )
    ~on_props:(fun previous props state ->
      match
        ( Popup.open_change
            (fun (props : (_, _) props) -> props.open_)
            previous props,
          state
        )
      with
      | Some true, Closed ->
          Go Open
      | Some false, Open ->
          close props
      | Some _, (Open | Closed) | None, (Open | Closed) ->
          Ignore
    )
    (fun props state event ->
      match (state, event) with
      | Closed, Open_request _ ->
          Go Open
      | Open, Close_request reason when dismisses props reason ->
          close props
      | Open, Close_request _ ->
          Stay []
      | Open, Content event when closes event ->
          close props
      | Open, Content _ ->
          Stay []
      | Closed, (Close_request _ | Content _) | Open, Open_request _ ->
          Ignore
    )

let el = Node.element
let text = Node.text

let trigger (props : (_, _) props) ~open_ =
  match props.trigger with
  | None ->
      Node.list []
  | Some label ->
      el "button"
        (Attr.string "type" "button"
         ::
         ( if open_ then
             []
           else
             [ Attr.on_press (Open_request Popup.Trigger_press) ]
         )
        @ Popup.trigger_attrs (parts props) ~haspopup:"dialog" ~open_
        @ Popup.open_attrs open_
        @ Popup.class_attr props.classes.trigger
        )
        [ text label ]

let popup ~content (props : (_, _) props) =
  let parts = parts props in
  let title_id = props.id ^ "-title"
  and description_id = props.id ^ "-description" in
  Node.portal
    [
      el "div"
        ([ Attr.string "data-ui" "dialog-backdrop" ]
        @ Popup.open_attrs true
        @ Popup.class_attr props.classes.backdrop
        )
        [];
      el "div"
        ([
           Attr.string "id" parts.popup_id;
           Attr.ref_ parts.popup;
           Attr.string "role"
             ( if props.alert then
                 "alertdialog"
               else
                 "dialog"
             );
           Attr.string "aria-modal" "true";
           Attr.string "aria-labelledby" title_id;
           Attr.string "tabindex" "-1";
         ]
        @ ( match props.description with
          | Some _ ->
              [ Attr.string "aria-describedby" description_id ]
          | None ->
              []
          )
        @ Popup.open_attrs true
        @ Popup.class_attr props.classes.popup
        )
        [
          el "h2"
            (Attr.string "id" title_id :: Popup.class_attr props.classes.title)
            [ text props.title ];
          ( match props.description with
          | Some description ->
              el "p"
                (Attr.string "id" description_id
                :: Popup.class_attr props.classes.description
                )
                [ text description ]
          | None ->
              Node.list []
          );
          Node.map (fun event -> Content event) (content props.content);
          ( match props.close with
          | Some label ->
              el "button"
                ([
                   Attr.string "type" "button";
                   Attr.string "id" (props.id ^ "-close");
                   Attr.on_press (Close_request Popup.Close_press);
                 ]
                @ Popup.class_attr props.classes.close
                )
                [ text label ]
          | None ->
              Node.list []
          );
        ];
    ]

let view ~content props = function
  | Closed ->
      trigger props ~open_:false
  | Open ->
      Node.list [ trigger props ~open_:true; popup ~content props ]

type ('content, 'ce) t = {
  component : (('content, 'ce) props, state, 'ce event) Component.t;
  closes : 'ce -> bool;
}

let component ~id ~closes ?events content =
  {
    component =
      Component.make_with_props (machine ~id ~closes ?events ()) (view ~content);
    closes;
  }

let props ~id ~title ?description ?trigger ?close ?open_ ?(dismissible = true)
    ?(alert = false) ?(classes = no_classes) content =
  {
    id;
    title;
    description;
    trigger;
    close;
    open_;
    dismissible;
    alert;
    classes;
    content;
  }

let make ?key ?on_open_change ?on_content dialog props =
  let opened = Popup.report on_open_change in
  let output event state =
    match (event, state) with
    | Open_request reason, Open ->
        opened true reason
    | Close_request reason, Closed ->
        opened false reason
    | Content event, _ ->
        Option.to_list (Option.map (fun f -> f event) on_content)
        @
        if dialog.closes event then
          opened false Popup.Close_press
        else
          []
    | Open_request _, Closed | Close_request _, Open ->
        []
  in
  Component.node_with_props ?key ~props ~output dialog.component
