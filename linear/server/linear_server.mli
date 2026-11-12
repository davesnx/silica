(** The mini Linear server: handlers, the [issues] topic and the page, over the
    mocked {!Linear_store}. The code that listens on a port is in [main.ml]. *)

module Server = Silica_adapter.Server

val create :
  ?count:int ->
  ?traces:string ->
  Silica_server.config ->
  assets:Silica_server.Assets.t ->
  read:(string -> string option) ->
  unit ->
  Server.t
(** A new store with [count] seed issues (see {!Linear_store.create}). The first
    visitor that asks who they are gets Ada, the next Ben, then Cleo, and the
    answer stays the same for the life of that session. A created issue is
    published to the topic [issues] under the key [all]. With [config.dev], the
    issues and comments are kept across a restart (see
    {!Silica_server.Make.keep}).

    With [config.dev] only, the endpoint {!Linear_trace.endpoint} ([/api/trace])
    takes a trace file that the development client posts, and each page has
    [<meta name="linear-trace">] so the client knows to record. The endpoint
    refuses a trace of more than 512 KiB or 2000 transitions, one that does not
    read as a trace file, and any trace once [traces] (default [linear/traces],
    made when missing) holds 200 trace files. It writes the trace again by
    {!Silica.Trace.to_jsonl} into a new file whose name it makes itself,
    [trace-<UTC time>-<n>.jsonl], and answers that name. Without [config.dev]
    the endpoint does not exist. *)
