(** The types of {!Topic}, in a module below {!Host} and {!Action}. Use them
    through {!Topic}. *)

type 's t
(** A topic with state ['s]. *)

val declare : name:string -> state:'s Codec.t -> 's t
val name : 's t -> string
val state : 's t -> 's Codec.t
val state_id : 's t -> 's Type.Id.t

type 's update = { seq : int; status : 's Status.t }

val update_codec : 's Codec.t -> 's update Codec.t
