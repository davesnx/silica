type 'e t =
  | Call : {
      endpoint : ('i, 'o, 'x) Endpoint.t;
      input : 'i;
      timeout_ms : int option;
      on_result : ('o, 'x) Endpoint.answer -> 'e;
    }
      -> 'e t
  | Live : {
      topic : 's Live_topic.t;
      key : string;
      on_update : 's Live_topic.update -> 'e;
    }
      -> 'e t
  | Held of 'e Action.source
  | Timer of { ms : int; event : 'e }

let remote_call ?timeout_ms endpoint input on_result =
  Call { endpoint; input; timeout_ms; on_result }

let after ms event = Timer { ms; event }
let topic_subscribe topic key on_update = Live { topic; key; on_update }

let focus_trap ?(return_focus = true) container =
  Held (Action.Focus_trap { container; return_focus })

let scroll_lock = Held Action.Scroll_lock
let outside_press container event =
  Held (Action.Outside_press { container; event })

let keys ?(prevent = false) ?rate bindings =
  Held (Action.Global_keys { bindings; prevent; rate })

let dismiss ?escape ?outside ?focus_out containers =
  Held
    (Action.Dismiss
       {
         containers;
         on_escape = escape;
         on_outside = outside;
         on_focus_out = focus_out;
       }
    )

let anchor ?(place = Place.default) ~anchor popup =
  Held (Action.Anchor { anchor; popup; place })

let resize ?rate target on_resize =
  Held (Action.Resize { target; rate; on_resize })

let intersection ?rate target on_change =
  Held (Action.Intersection { target; rate; on_change })

let scroll ?rate ?target on_scroll =
  Held (Action.Scroll { target; rate; on_scroll })

let visibility on_change = Held (Action.Visibility on_change)
let online on_change = Held (Action.Online on_change)
let media query on_change = Held (Action.Media { query; on_change })
let block on_attempt = Held (Action.Block { on_attempt })

let timer_name ms = "after " ^ string_of_int ms ^ "ms"

let name = function
  | Call call ->
      Endpoint.path call.endpoint
  | Live live ->
      "live:" ^ Live_topic.name live.topic
  | Held source ->
      Action.source_name source
  | Timer timer ->
      timer_name timer.ms

let refs source =
  let found = ref [] in
  ignore
    (Action.map_refs
       (fun r ->
         found := Ref.name r :: !found;
         r
       )
       source
    );
  List.rev !found

let anchor_place : type e. e Action.source -> string list =
  function[@warning "-4"]
  | Action.Anchor { place; _ } ->
      [
        Place.side_name place.side;
        Place.align_name place.align;
        Float.to_string place.offset;
        Float.to_string place.padding;
      ]
  | _ ->
      []

let key = function
  | Call call ->
      Endpoint.path call.endpoint
      ^ " "
      ^ Codec.to_string (Endpoint.input call.endpoint) call.input
  | Live live ->
      "live:" ^ Live_topic.name live.topic ^ " " ^ live.key
  | Held source ->
      String.concat " "
        ((Action.source_name source :: refs source) @ anchor_place source)
  | Timer timer ->
      timer_name timer.ms

let same first second = try first = second with Invalid_argument _ -> true

let delay = function
  | Timer timer ->
      Some timer.ms
  | Call _ | Live _ | Held _ ->
      None

let topic = function
  | Live live ->
      Some (Live_topic.name live.topic)
  | Call _ | Held _ | Timer _ ->
      None

let is_held = function Held _ -> true | Call _ | Live _ | Timer _ -> false

let held = function
  | Held source ->
      Some source
  | Call _ | Live _ | Timer _ ->
      None

let map_refs f = function
  | Held source ->
      Held (Action.map_refs f source)
  | (Call _ | Live _ | Timer _) as source ->
      source

let start id = function
  | Call call ->
      Action.Start_call
        {
          id;
          endpoint = call.endpoint;
          input = call.input;
          timeout_ms = call.timeout_ms;
          on_result = call.on_result;
        }
  | Live live ->
      Action.Start_live
        { id; topic = live.topic; key = live.key; on_update = live.on_update }
  | Held source ->
      Action.Start_source { id; source }
  | Timer timer ->
      Action.Start_timer { id; ms = timer.ms; event = timer.event }
