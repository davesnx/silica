type ('p, 's, 'e) t = ('p, 's, 'e) Node.component = {
  machine : ('p, 's, 'e) Machine.t;
  view : 'p -> 's -> 'e Node.t;
}

let make machine view = { machine; view = (fun _ state -> view state) }
let make_with_props machine view = { machine; view }
let node = Node.component
let node_with_props = Node.component_with_props
