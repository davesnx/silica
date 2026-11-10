type ('s, 'e) t = ('s, 'e) Node.component = {
  machine : ('s, 'e) Machine.t;
  view : 's -> 'e Node.t;
}

let make machine view = { machine; view }
let node = Node.component
