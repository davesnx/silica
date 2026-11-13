module Json = Silica_json.Json

type change = Instance.change =
  | Started
  | Resumed
  | Unhandled
  | Kept
  | Entered
  | Ended

type cause =
  | Start
  | Element of {
      handler : string;
      role : string option;
      name : string;
      element : string;
      value : string option;
    }
  | Timer of { id : string; ms : int }
  | Answer of { id : string; path : string; answer : Json.t option }
  | Live of { id : string; topic : string; update : Json.t option }
  | Held of { id : string; source : string }
  | Action_result of { action : string }
  | Props
  | Child of { instance : string }
  | Url of string
  | Preload of string
  | Message
  | Ended

type t = {
  seq : int;
  time_ms : float option;
  instance : string;
  number : int;
  entry : int;
  machine : string;
  from : string;
  target : string;
  change : change;
  event : string;
  data : Json.t option;
  props : Json.t option;
  snapshot : Json.t option;
  actions : string list;
  cause : cause;
}

let change_to_string = function
  | Started ->
      "started"
  | Resumed ->
      "resumed"
  | Unhandled ->
      "unhandled"
  | Kept ->
      "kept"
  | Entered ->
      "entered"
  | Ended ->
      "ended"

let change_of_string = function
  | "started" ->
      Ok Started
  | "resumed" ->
      Ok Resumed
  | "unhandled" ->
      Ok Unhandled
  | "kept" ->
      Ok Kept
  | "entered" ->
      Ok Entered
  | "ended" ->
      Ok Ended
  | other ->
      Error ("unknown change \"" ^ other ^ "\"")

let string value = Json.declassify (`String value)
let int value = Json.declassify (`Int value)
let optional name encode = function
  | None ->
      []
  | Some value ->
      [ (name, encode value) ]
let object_of fields = Json.declassify (`Assoc fields)

let encode_cause cause =
  let typed name fields = object_of (("type", string name) :: fields) in
  match cause with
  | Start ->
      typed "start" []
  | Element { handler; role; name; element; value } ->
      typed "element"
        ([ ("handler", string handler) ]
        @ optional "role" string role
        @ [ ("name", string name); ("element", string element) ]
        @ optional "value" string value
        )
  | Timer { id; ms } ->
      typed "timer" [ ("id", string id); ("ms", int ms) ]
  | Answer { id; path; answer } ->
      typed "answer"
        ([ ("id", string id); ("path", string path) ]
        @ optional "answer" Fun.id answer
        )
  | Live { id; topic; update } ->
      typed "live"
        ([ ("id", string id); ("topic", string topic) ]
        @ optional "update" Fun.id update
        )
  | Held { id; source } ->
      typed "held" [ ("id", string id); ("source", string source) ]
  | Action_result { action } ->
      typed "action_result" [ ("action", string action) ]
  | Props ->
      typed "props" []
  | Child { instance } ->
      typed "child" [ ("instance", string instance) ]
  | Url url ->
      typed "url" [ ("url", string url) ]
  | Preload url ->
      typed "preload" [ ("url", string url) ]
  | Message ->
      typed "message" []
  | Ended ->
      typed "ended" []

let encode record =
  object_of
    ([ ("seq", int record.seq) ]
    @ optional "time_ms"
        (fun time -> Json.declassify (`Float time))
        record.time_ms
    @ [
        ("instance", string record.instance);
        ("number", int record.number);
        ("entry", int record.entry);
        ("machine", string record.machine);
        ("from", string record.from);
        ("to", string record.target);
        ("change", string (change_to_string record.change));
        ("event", string record.event);
      ]
    @ optional "data" Fun.id record.data
    @ optional "props" Fun.id record.props
    @ optional "snapshot" Fun.id record.snapshot
    @ ( match record.actions with
      | [] ->
          []
      | actions ->
          [ ("actions", Json.declassify (`List (List.map string actions))) ]
      )
    @ [ ("cause", encode_cause record.cause) ]
    )

let ( let* ) = Result.bind

let fields json =
  match Json.classify json with
  | `Assoc fields ->
      Ok fields
  | _ ->
      Error "expected a JSON object"

let lookup fields name = List.assoc_opt name fields

let required fields name decode =
  match lookup fields name with
  | Some value -> (
      match decode value with
      | Ok decoded ->
          Ok decoded
      | Error message ->
          Error ("field \"" ^ name ^ "\": " ^ message)
    )
  | None ->
      Error ("missing field \"" ^ name ^ "\"")

let maybe fields name decode =
  match lookup fields name with
  | None ->
      Ok None
  | Some value -> (
      match Json.classify value with
      | `Null ->
          Ok None
      | _ -> (
          match decode value with
          | Ok decoded ->
              Ok (Some decoded)
          | Error message ->
              Error ("field \"" ^ name ^ "\": " ^ message)
        )
    )

let as_string json =
  match Json.classify json with
  | `String value ->
      Ok value
  | _ ->
      Error "expected a string"

let as_int json =
  match Json.classify json with
  | `Int value ->
      Ok value
  | `Float value when Float.is_integer value ->
      Ok (int_of_float value)
  | _ ->
      Error "expected an integer"

let as_float json =
  match Json.classify json with
  | `Int value ->
      Ok (float_of_int value)
  | `Float value ->
      Ok value
  | _ ->
      Error "expected a number"

let as_json json = Ok json

let as_strings json =
  match Json.classify json with
  | `List items ->
      List.fold_right
        (fun item rest ->
          let* rest = rest in
          let* item = as_string item in
          Ok (item :: rest)
        )
        items (Ok [])
  | _ ->
      Error "expected a list of strings"

let decode_cause json =
  let* fields = fields json in
  let text name = required fields name as_string in
  let* kind = text "type" in
  match kind with
  | "start" ->
      Ok Start
  | "element" ->
      let* handler = text "handler" in
      let* role = maybe fields "role" as_string in
      let* name = text "name" in
      let* element = text "element" in
      let* value = maybe fields "value" as_string in
      Ok (Element { handler; role; name; element; value })
  | "timer" ->
      let* id = text "id" in
      let* ms = required fields "ms" as_int in
      Ok (Timer { id; ms })
  | "answer" ->
      let* id = text "id" in
      let* path = text "path" in
      let* answer = maybe fields "answer" as_json in
      Ok (Answer { id; path; answer })
  | "live" ->
      let* id = text "id" in
      let* topic = text "topic" in
      let* update = maybe fields "update" as_json in
      Ok (Live { id; topic; update })
  | "held" ->
      let* id = text "id" in
      let* source = text "source" in
      Ok (Held { id; source })
  | "action_result" ->
      let* action = text "action" in
      Ok (Action_result { action })
  | "props" ->
      Ok Props
  | "child" ->
      let* instance = text "instance" in
      Ok (Child { instance })
  | "url" ->
      let* url = text "url" in
      Ok (Url url)
  | "preload" ->
      let* url = text "url" in
      Ok (Preload url)
  | "message" ->
      Ok Message
  | "ended" ->
      Ok Ended
  | other ->
      Error ("unknown cause \"" ^ other ^ "\"")

let decode json =
  let* fields = fields json in
  let text name = required fields name as_string in
  let* seq = required fields "seq" as_int in
  let* time_ms = maybe fields "time_ms" as_float in
  let* instance = text "instance" in
  let* number = required fields "number" as_int in
  let* entry = required fields "entry" as_int in
  let* machine = text "machine" in
  let* from = text "from" in
  let* target = text "to" in
  let* change = text "change" in
  let* change = change_of_string change in
  let* event = text "event" in
  let* data = maybe fields "data" as_json in
  let* props = maybe fields "props" as_json in
  let* snapshot = maybe fields "snapshot" as_json in
  let* actions = maybe fields "actions" as_strings in
  let* cause = required fields "cause" decode_cause in
  Ok
    {
      seq;
      time_ms;
      instance;
      number;
      entry;
      machine;
      from;
      target;
      change;
      event;
      data;
      props;
      snapshot;
      actions = Option.value actions ~default:[];
      cause;
    }

let codec = { Codec.encode; decode }

let describe_action : type m. m Action.t -> string = function
  | Action.Focus target ->
      "focus " ^ Ref.name target
  | Action.Blur target ->
      "blur " ^ Ref.name target
  | Action.Scroll_into_view target ->
      "scroll_into_view " ^ Ref.name target
  | Action.Scroll_to { target; left; top } ->
      "scroll_to "
      ^ (match target with Some target -> Ref.name target ^ " " | None -> "")
      ^ string_of_int left ^ " " ^ string_of_int top
  | Action.Measure { target; _ } ->
      "measure " ^ Ref.name target
  | Action.Copy _ ->
      "copy"
  | Action.Load_script { src; _ } ->
      "load_script " ^ src
  | Action.Navigate url ->
      "navigate " ^ Url.to_string url
  | Action.Replace url ->
      "replace " ^ Url.to_string url
  | Action.Back ->
      "back"
  | Action.Forward ->
      "forward"
  | Action.Reload ->
      "reload"
  | Action.Resume _ ->
      "resume"
  | Action.Revert_navigation ->
      "revert_navigation"
  | Action.Preload (Endpoint.Request (endpoint, _)) ->
      "preload " ^ Endpoint.path endpoint
  | Action.Load_chunk _ ->
      "load_chunk"
  | Action.Set_title title ->
      "set_title " ^ title
  | Action.Set_status status ->
      "set_status " ^ string_of_int status
  | Action.Set_document_attribute { name; value } -> (
      "set_document_attribute " ^ name
      ^ match value with Some value -> "=" ^ value | None -> ""
    )
  | Action.Set_cookie { name; _ } ->
      "set_cookie " ^ name
  | Action.Remount ->
      "remount"

let describe : type m. m Action.host -> string = function
  | Action.Run action ->
      describe_action action
  | Action.Start_source { id; source } ->
      "start source " ^ id ^ " " ^ Action.source_name source
  | Action.Start_timer { id; ms; _ } ->
      "start timer " ^ id ^ " " ^ string_of_int ms ^ "ms"
  | Action.Start_call { id; endpoint; _ } ->
      "start call " ^ id ^ " " ^ Endpoint.path endpoint
  | Action.Start_live { id; topic; key; _ } ->
      "start live " ^ id ^ " " ^ Topic.name topic ^ " " ^ key
  | Action.Stop id ->
      "stop " ^ id

let cause_label = function
  | Start ->
      "start"
  | Element { handler; role; name; value; _ } ->
      String.concat " "
        (List.filter
           (fun part -> part <> "")
           [
             handler;
             ( match (handler, value) with
             | ("key_down" | "key_up"), Some key ->
                 key
             | _ ->
                 ""
             );
             Option.value role ~default:"";
             ( if name = "" then
                 ""
               else
                 "\"" ^ name ^ "\""
             );
           ]
        )
  | Timer { ms; _ } ->
      "after " ^ string_of_int ms ^ "ms"
  | Answer { path; answer; _ } ->
      let outcome =
        match Option.map Codec.constructor_of answer with
        | Some (Ok (constructor, _)) ->
            String.lowercase_ascii constructor
        | Some (Error _) | None ->
            "answer"
      in
      outcome ^ " " ^ path
  | Live { topic; _ } ->
      "live " ^ topic
  | Held { source; _ } ->
      source
  | Action_result { action } ->
      "result " ^ action
  | Props ->
      "props"
  | Child { instance } ->
      "child " ^ instance
  | Url url ->
      "url " ^ url
  | Preload url ->
      "preload " ^ url
  | Message ->
      "message"
  | Ended ->
      "end"

let label record =
  match (record.change, record.event) with
  | (Started | Resumed | Ended), _ ->
      ""
  | (Unhandled | Kept | Entered), "" ->
      cause_label record.cause
  | (Unhandled | Kept | Entered), event ->
      event

let strip record =
  let cause =
    match record.cause with
    | Answer answer ->
        Answer { answer with answer = None }
    | Live live ->
        Live { live with update = None }
    | Element ({ handler = "input"; _ } as element) ->
        Element { element with value = None }
    | ( Start
      | Element _
      | Timer _
      | Held _
      | Action_result _
      | Props
      | Child _
      | Url _
      | Preload _
      | Message
      | Ended ) as cause ->
        cause
  in
  { record with data = None; props = None; snapshot = None; cause }

let rec handler_name : type m. m Node.attr -> string = function
  | Node.On_press _ ->
      "press"
  | Node.On_double_press _ ->
      "double_press"
  | Node.On_input _ ->
      "input"
  | Node.On_submit _ ->
      "submit"
  | Node.On_focus _ ->
      "focus"
  | Node.On_blur _ ->
      "blur"
  | Node.On_key_down _ ->
      "key_down"
  | Node.On_key_up _ ->
      "key_up"
  | Node.On_pointer_enter _ ->
      "pointer_enter"
  | Node.On_pointer_leave _ ->
      "pointer_leave"
  | Node.Rated (_, attr) ->
      handler_name attr
  | Node.String _ | Node.Bool _ | Node.Ref _ | Node.On_reset ->
      ""

let step_of siblings node tag =
  let count = ref 0 and position = ref 0 in
  let rec visit other =
    match Node.view other with
    | Node.Element (other_tag, _, _) ->
        if other_tag = tag then (
          incr count;
          if other == node then position := !count
        )
    | Node.List nodes ->
        List.iter visit nodes
    | Node.Keyed (_, other) ->
        visit other
    | Node.Boundary { body; _ } ->
        visit body
    | Node.Text _ | Node.Portal _ | Node.Component _ | Node.Chunk _ ->
        ()
  in
  List.iter visit siblings;
  if !count > 1 then
    tag ^ "[" ^ string_of_int !position ^ "]"
  else
    tag

type 'm located = {
  top : 'm Node.t list;
  portal : bool;
  chain : 'm Node.t list;
  closed : bool;
}

let rec search : type m.
    (m Node.attr -> bool) ->
    m Node.t list ->
    bool ->
    m Node.t ->
    m located option =
 fun found top portal node ->
  let inside located =
    if located.closed then
      Some located
    else
      Some { located with chain = located.chain @ [ node ] }
  in
  match Node.view node with
  | Node.Element (_, attrs, children) -> (
      if List.exists found attrs then
        Some { top; portal; chain = [ node ]; closed = false }
      else
        match List.find_map (search found top portal) children with
        | Some located ->
            inside located
        | None ->
            None
    )
  | Node.List nodes ->
      List.find_map (search found top portal) nodes
  | Node.Keyed (_, node) ->
      search found top portal node
  | Node.Boundary { body; _ } ->
      search found top portal body
  | Node.Portal (_, nodes) ->
      Option.map
        (fun located -> { located with closed = true })
        (List.find_map (search found nodes true) nodes)
  | Node.Text _ | Node.Component _ | Node.Chunk _ ->
      None

let element_path located =
  let rec steps siblings = function
    | [] ->
        []
    | node :: rest -> (
        match[@warning "-4"] Node.view node with
        | Node.Element (tag, _, children) ->
            step_of siblings node tag :: steps children rest
        | _ ->
            []
      )
  in
  ( if located.portal then
      "portal > "
    else
      ""
  )
  ^ String.concat " > " (steps located.top (List.rev located.chain))

let attribute attrs name =
  List.find_map
    (function[@warning "-4"]
      | Node.String (key, value) when key = name ->
          Some value
      | Node.Bool (key, true) when key = name ->
          Some ""
      | _ ->
          None
      )
    attrs

let field_tags = [ "input"; "textarea"; "select" ]

let rec mentions name node =
  match[@warning "-4"] Node.view node with
  | Node.Element (_, attrs, children) ->
      Option.is_some (attribute attrs name)
      || List.exists (mentions name) children
  | Node.List nodes ->
      List.exists (mentions name) nodes
  | Node.Keyed (_, node) ->
      mentions name node
  | _ ->
      false

let needs_document located =
  match located.chain with
  | [] ->
      true
  | target :: ancestors -> (
      mentions "aria-labelledby" target
      ||
      match[@warning "-4"] Node.view target with
      | Node.Element (tag, attrs, _) when List.mem tag field_tags ->
          Option.is_some (attribute attrs "id")
          || List.exists
               (fun ancestor ->
                 match[@warning "-4"] Node.view ancestor with
                 | Node.Element ("label", _, _) ->
                     true
                 | _ ->
                     false
               )
               ancestors
      | _ ->
          false
    )

let pruned located =
  let nested =
    List.fold_left
      (fun inner node ->
        match[@warning "-4"] Node.view node with
        | Node.Element (tag, attrs, _) ->
            Node.element tag attrs [ inner ]
        | _ ->
            inner
      )
      (List.hd located.chain) (List.tl located.chain)
  in
  if located.portal then
    Node.portal [ nested ]
  else
    nested

let element_details found element ~value =
  let attrs = Accessibility.attributes element in
  let handler =
    match List.find_opt found attrs with
    | Some attr ->
        handler_name attr
    | None ->
        ""
  in
  (handler, Accessibility.role element, Accessibility.name element, value)

let find_in : type m. (m Node.attr -> bool) -> m Node.t -> m located option =
 fun found view -> search found [ view ] false view

let password located =
  match located.chain with
  | target :: _ -> (
      match[@warning "-4"] Node.view target with
      | Node.Element (tag, attrs, _) ->
          Secret.is_password ~tag ~kind:(attribute attrs "type")
      | _ ->
          false
    )
  | [] ->
      false

let deferred : type m.
    (m Node.attr -> bool) ->
    m Node.t ->
    m located ->
    string option ->
    unit ->
    cause =
 fun found view located value ->
  let path = element_path located in
  let source =
    if needs_document located then
      view
    else
      pruned located
  in
  fun () ->
    match
      List.find_opt
        (fun element -> List.exists found (Accessibility.attributes element))
        (Accessibility.elements source)
    with
    | Some element ->
        let handler, role, name, value = element_details found element ~value in
        Element
          {
            handler;
            role = Option.map Accessibility.role_to_string role;
            name;
            element = path;
            value;
          }
    | None ->
        Message

let locate found view value =
  Option.bind view (fun view ->
      Option.map
        (fun located -> deferred found view located value)
        (find_in found view)
  )

let rec delivers : type m. m -> m Node.attr -> bool =
 fun message -> function
  | Node.On_press other
  | Node.On_double_press other
  | Node.On_submit other
  | Node.On_focus other
  | Node.On_blur other ->
      other == message
  | Node.Rated (_, attr) ->
      delivers message attr
  | Node.String _
  | Node.Bool _
  | Node.Ref _
  | Node.On_input _
  | Node.On_key_down _
  | Node.On_key_up _
  | Node.On_pointer_enter _
  | Node.On_pointer_leave _
  | Node.On_reset ->
      false

type pending = { mutable record : t; mutable element : (unit -> cause) option }

let ready record = { record; element = None }
let peek pending = pending.record

let resolve pending =
  ( match pending.element with
  | Some find ->
      pending.element <- None;
      pending.record <- { pending.record with cause = find () }
  | None ->
      ()
  );
  pending.record

let make_cause ~mask ~view ~message (cause : Instance.cause) =
  match cause with
  | Start ->
      (Start, None, None)
  | Element (attr, input, key) ->
      let found other = other == attr in
      let located = Option.bind view (fun view -> find_in found view) in
      let password_field =
        match located with
        | Some located ->
            mask && password located
        | None ->
            false
      in
      let secret =
        match (input, located) with
        | Some text, Some located when password_field ->
            Some (element_path located, text)
        | _ ->
            None
      in
      let value =
        match (input, key) with
        | Some input, _ ->
            Some
              ( if secret = None then
                  input
                else
                  Secret.hide input
              )
        | None, Some key ->
            let printed = Key.to_string key in
            Some
              ( if password_field && Secret.is_typed_key printed then
                  Secret.hide printed
                else
                  printed
              )
        | None, None ->
            None
      in
      ( Element
          {
            handler = handler_name attr;
            role = None;
            name = "";
            element = "";
            value;
          },
        ( match (view, located) with
        | Some view, Some located ->
            Some (deferred found view located value)
        | _ ->
            None
        ),
        secret
      )
  | Timer (id, ms) ->
      (Timer { id; ms }, None, None)
  | Answer (id, endpoint, answer) ->
      ( Answer
          {
            id;
            path = Endpoint.path endpoint;
            answer =
              Some
                ((Endpoint.answer_codec (Endpoint.output endpoint)
                    (Endpoint.rejected endpoint)
                 )
                   .encode
                   answer
                );
          },
        None,
        None
      )
  | Live (id, topic, update) ->
      ( Live
          {
            id;
            topic = Topic.name topic;
            update =
              Some ((Topic.update_codec (Topic.state topic)).encode update);
          },
        None,
        None
      )
  | Source (id, source) ->
      (Held { id; source = Action.source_name source }, None, None)
  | Result action ->
      (Action_result { action = describe_action action }, None, None)
  | Props ->
      (Props, None, None)
  | Child cell ->
      (Child { instance = Instance.cell_path cell }, None, None)
  | Route (Router.Url_changed url) ->
      (Url (Url.to_string url), None, None)
  | Route (Router.Preload url) ->
      (Preload (Url.to_string url), None, None)
  | Handler -> (
      match message with
      | Some message ->
          (Message, locate (delivers message) view None, None)
      | None ->
          (Message, None, None)
    )
  | Message ->
      (Message, None, None)
  | End ->
      (Ended, None, None)

let encoded codec value =
  match codec with
  | Ok codec ->
      Some (codec.Codec.encode value)
  | Error _ ->
      None

let make ~secrets ~seq ~time_ms observed =
  let (Instance.Inspected
         {
           instance;
           number;
           entry;
           machine;
           props;
           from;
           snapshot;
           change;
           event;
           actions;
           cause;
           view;
           message;
         }
        ) =
    Instance.inspect observed
  in
  let cause, element, secret =
    make_cause ~mask:(secrets <> None) ~view ~message cause
  in
  let encoded codec value =
    match secrets with
    | Some secrets ->
        Option.iter
          (fun (path, text) ->
            Secret.learn secrets ~field:(instance ^ " " ^ path) text
          )
          secret;
        Option.map (Secret.scrub secrets) (encoded codec value)
    | None ->
        encoded codec value
  in
  let with_props =
    match[@warning "-4"] (change, cause) with
    | (Started | Resumed), _ | _, Props ->
        encoded (Machine.props_codec machine) props
    | _ ->
        None
  in
  {
    record =
      {
        seq;
        time_ms;
        instance;
        number;
        entry;
        machine = Machine.id machine;
        from =
          (match from with Some from -> Machine.name machine from | None -> "");
        target = Machine.name machine snapshot;
        change;
        event =
          ( match event with
          | Some event ->
              Option.value ~default:"" (Machine.event_name machine event)
          | None ->
              ""
          );
        data =
          ( match[@warning "-4"] (event, cause) with
          | ( _,
              Element { handler = "key_down" | "key_up"; value = Some value; _ }
            )
            when value = Secret.mask ->
              None
          | Some event, _ ->
              encoded (Machine.event_codec machine) event
          | None, _ ->
              None
          );
        props = with_props;
        snapshot = encoded (Machine.state_codec machine) snapshot;
        actions = List.map describe actions;
        cause;
      };
    element;
  }

let learn secrets observed =
  let (Instance.Inspected { instance; cause; view; _ }) =
    Instance.inspect observed
  in
  match[@warning "-4"] (cause, view) with
  | Element (attr, Some text, _), Some view -> (
      match find_in (fun other -> other == attr) view with
      | Some located when password located ->
          Secret.learn secrets
            ~field:(instance ^ " " ^ element_path located)
            text
      | Some _ | None ->
          ()
    )
  | _ ->
      ()

let element_message (make : Instance.cause -> 'e -> Instance.message) self input
    key event =
  make (Instance.Element (!self, input, key)) event

let at_input make self (input : (_, _) Node.handler) key =
  {
    input with
    Node.run =
      (fun value ->
        Option.map (element_message make self None (key value)) (input.run value)
      );
  }

let rec at_element make self : _ Node.attr -> Instance.message Node.attr =
  function
  | Node.On_input text ->
      Node.On_input
        (fun input -> element_message make self (Some input) None (text input))
  | Node.On_key_down input ->
      Node.On_key_down (at_input make self input Option.some)
  | Node.On_key_up input ->
      Node.On_key_up (at_input make self input Option.some)
  | Node.On_pointer_enter input ->
      Node.On_pointer_enter (at_input make self input (fun _ -> None))
  | Node.On_pointer_leave input ->
      Node.On_pointer_leave (at_input make self input (fun _ -> None))
  | Node.Rated (rate, attr) ->
      Node.Rated (rate, at_element make self attr)
  | ( Node.String _
    | Node.Bool _
    | Node.Ref _
    | Node.On_reset
    | Node.On_press _
    | Node.On_double_press _
    | Node.On_submit _
    | Node.On_focus _
    | Node.On_blur _ ) as attr ->
      Node.map_attr (make Instance.Handler) attr

let handler make =
  let from_view = make Instance.Handler in
  fun attr ->
    match attr with
    | Node.String _
    | Node.Bool _
    | Node.Ref _
    | Node.On_reset
    | Node.On_press _
    | Node.On_double_press _
    | Node.On_submit _
    | Node.On_focus _
    | Node.On_blur _ ->
        Node.map_attr from_view attr
    | Node.On_input _
    | Node.On_key_down _
    | Node.On_key_up _
    | Node.On_pointer_enter _
    | Node.On_pointer_leave _
    | Node.Rated _ ->
        let self = ref Node.On_reset in
        let resolved = at_element make self attr in
        self := resolved;
        resolved

let observer ?clock ?(mask = true) ?(secrets = Secret.create ()) record =
  let secrets =
    if mask then
      Some secrets
    else
      None
  in
  let seq = ref 0 in
  {
    Instance.observe =
      (fun observed ->
        incr seq;
        record
          (make ~secrets ~seq:!seq
             ~time_ms:(Option.map (fun clock -> clock ()) clock)
             observed
          )
      );
    handler;
  }

let path_only url =
  let cut = String.length url in
  let cut = Option.value (String.index_opt url '?') ~default:cut |> min cut in
  let cut = Option.value (String.index_opt url '#') ~default:cut |> min cut in
  String.sub url 0 cut

let find_from text start pattern =
  let size = String.length pattern and length = String.length text in
  let rec look index =
    if index + size > length then
      None
    else if String.sub text index size = pattern then
      Some index
    else
      look (index + 1)
  in
  look start

let tail text index = String.sub text index (String.length text - index)

let numbered_suffix text =
  text = ""
  || String.length text > 1
     && text.[0] = '#'
     && String.for_all (fun c -> c >= '0' && c <= '9') (tail text 1)

let live_id rest =
  match find_from rest 0 "/live:" with
  | None ->
      None
  | Some marker -> (
      let start = marker + 6 in
      match String.index_from_opt rest start ' ' with
      | None ->
          None
      | Some space ->
          let topic = String.sub rest start (space - start) in
          let between = " " ^ topic ^ " " in
          let rec candidate from =
            match find_from rest from between with
            | None ->
                None
            | Some at ->
                let id = String.sub rest 0 at in
                let key = tail rest (at + String.length between) in
                let in_id = tail id (space + 1) in
                if
                  String.starts_with ~prefix:key in_id
                  && numbered_suffix (tail in_id (String.length key))
                then
                  Some (id, topic)
                else
                  candidate (at + 1)
          in
          candidate (space + 1)
    )

let renamer () =
  let table = Hashtbl.create 16 in
  fun id ->
    let number =
      match Hashtbl.find_opt table id with
      | Some number ->
          number
      | None ->
          let number = Hashtbl.length table + 1 in
          Hashtbl.add table id number;
          number
    in
    let kept =
      match String.index_opt id '/' with
      | None ->
          ""
      | Some first -> (
          match String.index_from_opt id (first + 1) '/' with
          | Some second ->
              String.sub id 0 (second + 1)
          | None ->
              String.sub id 0 (first + 1)
        )
    in
    kept ^ "#" ^ string_of_int number

let names_only_action rename action =
  let after prefix =
    if String.starts_with ~prefix action then
      Some (tail action (String.length prefix))
    else
      None
  in
  let with_last verb rest =
    match String.rindex_opt rest ' ' with
    | Some space ->
        verb ^ " " ^ rename (String.sub rest 0 space) ^ tail rest space
    | None ->
        verb
  in
  match
    List.find_map
      (fun (prefix, rewrite) -> Option.map rewrite (after prefix))
      [
        ("stop ", fun id -> "stop " ^ rename id);
        ("start timer ", with_last "start timer");
        ("start call ", with_last "start call");
        ("start source ", with_last "start source");
        ( "start live ",
          fun rest ->
            match live_id rest with
            | Some (id, topic) ->
                "start live " ^ rename id ^ " " ^ topic
            | None ->
                "start live"
        );
        ("set_title ", fun _ -> "set_title");
        ("navigate ", fun url -> "navigate " ^ path_only url);
        ("replace ", fun url -> "replace " ^ path_only url);
        ( "set_document_attribute ",
          fun rest ->
            "set_document_attribute "
            ^
            match String.index_opt rest '=' with
            | Some equal ->
                String.sub rest 0 equal
            | None ->
                rest
        );
      ]
  with
  | Some rewritten ->
      rewritten
  | None ->
      action

let names_only records =
  let rename = renamer () in
  List.map
    (fun record ->
      let record = strip record in
      let cause =
        match record.cause with
        | Element element ->
            Element { element with name = "" }
        | Timer timer ->
            Timer { timer with id = rename timer.id }
        | Answer answer ->
            Answer { answer with id = rename answer.id }
        | Live live ->
            Live { live with id = rename live.id }
        | Held held ->
            Held { held with id = rename held.id }
        | Action_result { action } ->
            Action_result { action = names_only_action rename action }
        | Url url ->
            Url (path_only url)
        | Preload url ->
            Preload (path_only url)
        | (Start | Props | Child _ | Message | Ended) as cause ->
            cause
      in
      {
        record with
        cause;
        actions = List.map (names_only_action rename) record.actions;
      }
    )
    records
