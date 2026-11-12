let endpoint =
  Silica.Remote.declare ~path:"/api/trace" ~input:Silica.Codec.string
    ~output:Silica.Codec.string ~rejected:Silica.Codec.string

let meta = "linear-trace"
