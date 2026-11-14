(** The server of the Silica site: the page, the documentation endpoint and the
    endpoints of the live samples. The code that listens on a port is in
    [main.ml]. *)

module Server = Silica_adapter.Server

val doc : string -> Site_doc.answer
(** The documentation page with this name, from the [.mld] files of [docs/]. *)

val text_files : string list
(** The paths of the Markdown text for agents: [/llms.txt], [/llms-full.txt] and
    [/docs/<page>.md] for each page, made from [docs/*.mld]. *)

val create :
  Silica_server.config ->
  assets:Silica_server.Assets.t ->
  read:(string -> string option) ->
  unit ->
  (Server.t, string) result
(** The server, or an [Error] that names the first of [client_main.js] and
    [app.css] that [assets] does not have. *)
