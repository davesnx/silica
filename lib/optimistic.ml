module type CHANGE = sig
  type value
  type change

  val apply : value -> change -> value
end

module type OPTIMISTIC = sig
  type value
  type change
  type t

  val make : value -> t
  val apply : id:int -> change -> t -> t
  val settle : ?confirmed:value -> id:int -> t -> t
  val drop : id:int -> t -> t
  val rebase : value -> t -> t
  val value : t -> value
  val confirmed : t -> value
  val pending : t -> int
  val is_pending : id:int -> t -> bool
end

module Make (Change : CHANGE) = struct
  type value = Change.value
  type change = Change.change
  type t = { confirmed : value; pending : (int * change) list }

  let make confirmed = { confirmed; pending = [] }

  let apply ~id change t =
    if List.mem_assoc id t.pending then
      {
        t with
        pending =
          List.map
            (fun (other, c) ->
              if other = id then
                (id, change)
              else
                (other, c)
            )
            t.pending;
      }
    else
      { t with pending = t.pending @ [ (id, change) ] }

  let settle ?confirmed ~id t =
    match List.assoc_opt id t.pending with
    | None ->
        t
    | Some change ->
        {
          confirmed =
            ( match confirmed with
            | Some value ->
                value
            | None ->
                Change.apply t.confirmed change
            );
          pending = List.remove_assoc id t.pending;
        }

  let drop ~id t = { t with pending = List.remove_assoc id t.pending }
  let rebase confirmed t = { t with confirmed }

  let value t =
    List.fold_left
      (fun value (_, change) -> Change.apply value change)
      t.confirmed t.pending

  let confirmed t = t.confirmed
  let pending t = List.length t.pending
  let is_pending ~id t = List.mem_assoc id t.pending
end
