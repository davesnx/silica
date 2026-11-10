(** Hashed static files, from an esbuild metafile. *)

type t

val of_metafile : prefix:string -> string -> (t, string) result
(** [of_metafile ~prefix json] reads the metafile text [json]. Only outputs with
    an [entryPoint] are entries. The logical name of an entry is the basename of
    its entry point, with the extension of the output file. The public path is
    [prefix] followed by the basename of the output. Error when the JSON is not
    valid, when there is no [outputs] object, or when two entries have the same
    logical name. *)

val resolve : t -> string -> string option
(** [resolve t name] is the public path for the logical name [name]. *)

val all : t -> string list
(** The public paths of all entries. *)

val outputs : t -> string list
(** The public paths of every output, entries and chunks, in the order of the
    metafile. *)

val imports : t -> string -> string list
(** [imports t public] is the public paths of the files that the output with the
    public path [public] imports with a static [import] statement, directly or
    through another file, in the order they are found, without [public]. A
    dynamic [import()], such as a lazy route chunk, is not followed. *)

val is_hashed : string -> bool
(** True when the basename has the form [stem-hash.ext], where hash has 8 or
    more characters in [A-Z0-9]. *)

val content_type : string -> string
(** The content type for a file name, from its extension. Text types have
    [; charset=utf-8]. *)

val etag : string -> string
(** A strong ETag: the first 16 hex characters of the SHA-256 of the content, in
    double quotes. *)
