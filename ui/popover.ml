open Silica

type classes = {
  trigger : string;
  popup : string;
  title : string;
  close : string;
}

let no_classes = { trigger = ""; popup = ""; title = ""; close = "" }

module Make (Content : Dialog.CONTENT) = struct
  type props = {
    id : string;
    trigger : string;
    title : string option;
    close : string option;
    open_ : bool option;
    place : Place.t;
    classes : classes;
    content : Content.props;
  }

  type event =
    | Props of props
    | Open_request of Popup.reason
    | Close_request of Popup.reason
    | Content of Content.event

  module State = struct
    type _ t = Idle : unit t | Closed : props t | Open : props t
  end

  module M = Machine.Make (State)

  type snapshot = M.snapshot = At : 'data State.t * 'data -> snapshot

  let parts (props : props) = Popup.parts props.id

  let sources =
    List.map
      (Popup.scoped (fun (props : props) -> props.id))
      [
        Popup.dismiss ~escape:(Close_request Popup.Escape_key)
          ~outside:(Close_request Popup.Outside_press) Popup.template;
        Source.anchor
          ~place:(fun (props : props) -> props.place)
          ~anchor:Popup.template.trigger Popup.template.popup;
      ]

  let opened_by (previous : props option) (props : props) =
    props.open_ = Some true
    && Option.map (fun (p : props) -> p.open_) previous <> Some (Some true)

  let closed_by (previous : props) (props : props) =
    props.open_ = Some false && previous.open_ <> Some false

  let open_ (props : props) =
    M.goto Open props ~cmds:[ Cmd.Focus (parts props).popup ]

  let close ?(focus = true) (props : props) =
    let cmds =
      if focus then
        [ Cmd.Focus (parts props).trigger ]
      else
        []
    in
    M.goto Closed props ~cmds

  let machine =
    M.make ~id:Content.id ~init:(M.at Idle ())
      [
        M.state Idle "idle" ~to_:[ Closed; Open ] (fun () -> function
          | Props props when opened_by None props ->
              open_ props
          | Props props ->
              M.goto Closed props
          | _ ->
              M.unhandled
          );
        M.state Closed "closed" ~to_:[ Open ] (fun previous -> function
          | Props props when opened_by (Some previous) props ->
              open_ props
          | Props props ->
              M.goto Closed props
          | Open_request _ ->
              open_ previous
          | Close_request _ | Content _ ->
              M.unhandled
          );
        M.state Open "open" ~to_:[ Closed ] ~invoke:sources
          (fun previous -> function
          | Props props when closed_by previous props ->
              close props
          | Props props when props.place <> previous.place ->
              M.reenter Open props
          | Props props ->
              M.goto Open props
          | Close_request Popup.Outside_press ->
              close ~focus:false previous
          | Close_request _ ->
              close previous
          | Content event when Content.closes event ->
              close previous
          | Content _ ->
              M.stay []
          | Open_request _ ->
              M.unhandled
          );
      ]

  let el = Node.element
  let text = Node.text

  let trigger (props : props) ~open_ =
    el "button"
      (Attr.string "type" "button"
       :: Attr.on_press
            ( if open_ then
                Close_request Popup.Trigger_press
              else
                Open_request Popup.Trigger_press
            )
       :: Popup.trigger_attrs (parts props) ~haspopup:"dialog" ~open_
      @ Popup.open_attrs open_
      @ Popup.class_attr props.classes.trigger
      )
      [ text props.trigger ]

  let popup (props : props) =
    let parts = parts props and title_id = props.id ^ "-title" in
    Node.portal
      [
        el "div"
          ([
             Attr.string "id" parts.popup_id;
             Attr.ref_ parts.popup;
             Attr.string "role" "dialog";
             Attr.string "tabindex" "-1";
           ]
          @ ( match props.title with
            | Some _ ->
                [ Attr.string "aria-labelledby" title_id ]
            | None ->
                []
            )
          @ Popup.open_attrs true
          @ Popup.class_attr props.classes.popup
          )
          [
            ( match props.title with
            | Some title ->
                el "h2"
                  (Attr.string "id" title_id
                  :: Popup.class_attr props.classes.title
                  )
                  [ text title ]
            | None ->
                Node.list []
            );
            Node.map (fun event -> Content event) (Content.view props.content);
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

  let view : snapshot -> event Node.t = function
    | At (Idle, ()) ->
        Node.list []
    | At (Closed, props) ->
        trigger props ~open_:false
    | At (Open, props) ->
        Node.list [ trigger props ~open_:true; popup props ]

  let component = Component.make machine view

  let props ~id ~trigger ?title ?close ?open_ ?(place = Place.default)
      ?(classes = no_classes) content =
    { id; trigger; title; close; open_; place; classes; content }

  let make ?key ?on_open_change ?on_content props =
    let opened value reason =
      Option.to_list (Option.map (fun f -> f value reason) on_open_change)
    in
    let output event (snapshot : snapshot) =
      match (event, snapshot) with
      | Open_request reason, At (Open, _) ->
          opened true reason
      | Close_request reason, At (Closed, _) ->
          opened false reason
      | Content event, _ ->
          Option.to_list (Option.map (fun f -> f event) on_content)
          @
          if Content.closes event then
            opened false Popup.Close_press
          else
            []
      | _ ->
          []
    in
    Component.node ?key ~props:(Props props) ~output component
end
