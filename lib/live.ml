type ('s, 'e) t = {
  channel : string;
  state : 's Codec.t;
  event : 'e Codec.t;
  component : ('s, 'e) Component.t;
}

let declare ~channel ~state ~event component =
  { channel; state; event; component }

let slot t =
  Node.element "div" [ Node.String ("data-silica-live", t.channel) ] []
