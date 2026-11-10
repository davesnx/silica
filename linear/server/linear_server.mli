(** The mini Linear server: handlers, the [issues] topic and the page, over the
    mocked {!Linear_store}. The code that listens on a port is in [main.ml]. *)

module S = Silica_example_adapter.Server

val create :
  ?count:int ->
  Silica_server.config ->
  assets:Silica_server.Assets.t ->
  read:(string -> string option) ->
  unit ->
  S.t
(** A new store with [count] seed issues (see {!Linear_store.create}). The first
    visitor that asks who they are gets Ada, the next Ben, then Cleo, and the
    answer stays the same for the life of that session. A created issue is
    published to the topic [issues] under the key [all]. *)
