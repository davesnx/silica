type ('p, 'e) t =
  | Call : {
      endpoint : ('i, 'o) Remote.endpoint;
      input : 'p -> 'i;
      timeout_ms : int option;
      on_result : ('o, Remote.error) result -> 'e;
    }
      -> ('p, 'e) t
  | Live : {
      topic : 's Topic.t;
      key : 'p -> string;
      on_update : 's Topic.update -> 'e;
    }
      -> ('p, 'e) t
  | Held : { kind : string; make : 'p -> 'e Cmd.source } -> ('p, 'e) t

let call ?timeout_ms endpoint input on_result =
  Call { endpoint; input; timeout_ms; on_result }

let live topic key on_update = Live { topic; key; on_update }

let fixed source =
  Held { kind = Cmd.source_name source; make = (fun _ -> source) }

let focus_trap ?(return_focus = true) container =
  fixed (Cmd.Focus_trap { container; return_focus })

let scroll_lock =
  Held { kind = "scroll_lock"; make = (fun _ -> Cmd.Scroll_lock) }
let outside_press container event =
  fixed (Cmd.Outside_press { container; event })

let keys ?(prevent = false) ?rate bindings =
  fixed (Cmd.Global_keys { bindings; prevent; rate })

let dismiss ?escape ?outside ?focus_out containers =
  fixed
    (Cmd.Dismiss
       {
         containers;
         on_escape = escape;
         on_outside = outside;
         on_focus_out = focus_out;
       }
    )

let anchor ?(place = fun _ -> Place.default) ~anchor popup =
  Held
    {
      kind = "anchor";
      make = (fun data -> Cmd.Anchor { anchor; popup; place = place data });
    }

let resize ?rate target on_resize =
  fixed (Cmd.Resize { target; rate; on_resize })

let intersection ?rate target on_change =
  fixed (Cmd.Intersection { target; rate; on_change })

let scroll ?rate ?target on_scroll =
  fixed (Cmd.Scroll { target; rate; on_scroll })

let visibility on_change = fixed (Cmd.Visibility on_change)
let online on_change = fixed (Cmd.Online on_change)
let media query on_change = fixed (Cmd.Media { query; on_change })
let block on_attempt = fixed (Cmd.Block { on_attempt })

let name = function
  | Call c ->
      Remote.path c.endpoint
  | Live l ->
      "live:" ^ Topic.name l.topic
  | Held h ->
      h.kind

let topic = function Live l -> Some (Topic.name l.topic) | _ -> None
let is_held = function Call _ | Live _ -> false | Held _ -> true
let held data = function Call _ | Live _ -> None | Held h -> Some (h.make data)

let map_refs f = function
  | Held h ->
      Held { h with make = (fun data -> Cmd.map_refs (f data) (h.make data)) }
  | source ->
      source

let start id data = function
  | Call c ->
      Cmd.Start_call
        {
          id;
          endpoint = c.endpoint;
          input = c.input data;
          timeout_ms = c.timeout_ms;
          on_result = c.on_result;
        }
  | Live l ->
      Cmd.Start_live
        { id; topic = l.topic; key = l.key data; on_update = l.on_update }
  | Held h ->
      Cmd.Start_source { id; source = h.make data }
