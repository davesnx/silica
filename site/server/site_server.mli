(** The server of the Silica site: the page, the documentation endpoint and the
    endpoints of the live samples. The code that listens on a port is in
    [main.ml]. *)

module Server = Silica_adapter.Server

val doc : string -> Site_doc.answer
(** The documentation page with this name, from the [.mld] files of [docs/]. *)

val create :
  Silica_server.config ->
  assets:Silica_server.Assets.t ->
  read:(string -> string option) ->
  unit ->
  Server.t
