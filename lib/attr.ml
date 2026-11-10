let with_rate rate attr =
  match rate with None -> attr | Some rate -> Node.Rated (rate, attr)

let string k v = Node.String (k, v)
let bool k v = Node.Bool (k, v)
let ref_ r = Node.Ref r
let style s = Node.String ("style", s)
let on_press e = Node.On_press e
let on_double_press e = Node.On_double_press e
let on_input ?rate f = with_rate rate (Node.On_input f)
let on_submit e = Node.On_submit e
let on_focus e = Node.On_focus e
let on_blur e = Node.On_blur e

let rated rate attr = Node.Rated (rate, attr)

let on_key_down ?(prevent = false) ?rate run =
  with_rate rate (Node.On_key_down { run; prevent })

let on_key_up ?(prevent = false) ?rate run =
  with_rate rate (Node.On_key_up { run; prevent })

let on_pointer_enter ?(prevent = false) ?rate run =
  with_rate rate (Node.On_pointer_enter { run; prevent })

let on_pointer_leave ?(prevent = false) ?rate run =
  with_rate rate (Node.On_pointer_leave { run; prevent })

let on_reset = Node.On_reset
