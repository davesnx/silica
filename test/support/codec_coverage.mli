(** Checks that the codec module of an app registers every machine whose events
    are plain data. *)

val required : string -> (string * string option) list
(** The files of the directory whose [type event] derives JSON, with the id of
    the machine that the file makes, if it can be read. *)

val missing : string -> string list
(** A line for each file of {!required} whose machine has no registered codecs
    (see {!Silica.Machine.registered_codecs}). *)
