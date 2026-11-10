(** The mocked data of mini Linear: issues in memory. Safe to share between
    threads. *)

open Linear_model

type t

val create : ?count:int -> unit -> t
(** The store with [count] seed issues (default 30), ENG-1 to ENG-[count].
    Beyond 30 the seed list repeats, with the number added to the title. *)

val issues : t -> Issue.t list
(** Every issue, by number, oldest first. *)

val create_issue : t -> now:int -> Draft.t -> (Issue.t, string) result
(** Check the draft (see {!Draft.check}) and add the issue: the next number, no
    assignee, no labels, [created_at] and [updated_at] set to [now], version 1.
    [Error] holds the message of the check. *)

val update_issue :
  t -> now:int -> Change.request -> (Change.answer, string) result
(** Apply the change when [base] is the current version: the version goes up by
    one and [updated_at] is [now] ([Saved]). When [base] is older, nothing
    changes and the answer is the current issue ([Conflict]): the last write
    wins only when it was made on the newest version. [Error] for an unknown
    issue or a change that {!Change.check} refuses. *)

val comments : t -> int -> Comment.t list
(** The comments of the issue with this number, oldest first. *)

val add_comment :
  t -> now:int -> author:User.t -> New_comment.t -> (Comment.t, string) result
(** Check the body (see {!Comment.check}) and add the comment to an existing
    issue. *)

val edit_comment :
  t -> author:User.t -> Comment_edit.t -> (Comment.t, string) result
(** Change the body of a comment of [author] and mark it edited. [Error] for an
    unknown comment, a comment of someone else or a bad body. *)

val next_user : unit -> unit -> User.t
(** A function that gives Ada, Ben and Cleo in turn, and then starts again. *)
