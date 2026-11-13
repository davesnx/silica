type 'e t = {
  paint : 'e Node.t -> unit;
  run : 'e Action.t -> unit;
  start_held : id:string -> 'e Action.held -> unit;
  stop_held : id:string -> unit;
}

type 'e make = dispatch:('e -> unit) -> 'e t
