type 'e t = {
  paint : 'e Node.t -> unit;
  run : 'e Action.t -> unit;
  start_source : id:string -> 'e Action.source -> unit;
  stop_source : id:string -> unit;
}

type 'e make = dispatch:('e -> unit) -> 'e t
