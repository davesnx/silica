type route = Home [@route Route.root] | Issue of int [@@deriving routes]
