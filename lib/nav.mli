(** One navigation, as a {!Source.block} sees it. *)

type t =
  | Push of Url.t  (** A link press or {!Cmd.Navigate}: a new history entry. *)
  | Replace of Url.t
      (** A replace link or {!Cmd.Replace}: the current entry changes. *)
  | Traverse of { delta : int; url : Url.t }
      (** Back ([delta < 0]) or forward ([delta > 0]) by [delta] history
          entries, to the entry with the URL [url]. *)
