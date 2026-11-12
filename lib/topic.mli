(** A named, keyed piece of server data with a state type. The server publishes
    the full state under a key; every subscriber of that name and key gets it
    with a sequence number. The server holds no machine for a topic. *)

type 's t

val declare : name:string -> state:'s Codec.t -> 's t
(** Declare a topic in shared code. [name] is the same on the server and in the
    browser. *)

val name : 's t -> string
(** The name of the topic. *)

val state : 's t -> 's Codec.t
(** The codec of the topic state. *)

val state_id : 's t -> 's Type.Id.t
(** A type id made by [declare], one for each topic. The checker uses it to find
    the sample states of a topic. *)

type 's update = { seq : int; status : 's Status.t }
(** What a subscription raises as an event. [seq] is the sequence number of the
    state in [status]; it is [0] when the status holds no new state: connecting,
    offline (the old data with [stale = true]) or a failure. A client drops a
    state whose [seq] is not higher than the last one it got on the socket. *)

val update_codec : 's Codec.t -> 's update Codec.t
(** [update_codec state] writes [{"seq": n, "status": ..}], the status by
    {!Status.codec}. A machine whose events hold updates uses it in its event
    codec (see {!Machine.register_codecs}). For a topic:
    [update_codec (Topic.state topic)]. *)
