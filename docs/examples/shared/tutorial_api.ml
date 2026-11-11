let greeting =
  Silica.Remote.declare ~path:"/api/greeting" ~input:Silica.Codec.string
    ~output:Silica.Codec.string ~rejected:Silica.Codec.string
