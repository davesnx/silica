let save =
  Silica.Remote.declare ~path:"/api/save" ~input:Silica.Codec.int
    ~output:Silica.Codec.unit ~rejected:Silica.Codec.string
