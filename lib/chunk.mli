(** A value whose code may arrive later: the page of a lazy route.

    On a native server the code is linked in, so a chunk is {!ready}. In the
    browser, {!deferred} wraps a loader such as a dynamic import (see
    [Silica_browser.import]): the bundler puts the code in its own file, and the
    file loads the first time a view shows the chunk with {!Node.val-chunk}, or
    earlier when a link preloads its route ({!Router.Make.val-routes}). The same
    shared view code uses either, because each platform library defines the
    module that holds the chunks. *)

type 'a t

val ready : 'a -> 'a t
(** A chunk whose value is here. *)

val deferred : ((('a, string) result -> unit) -> unit) -> 'a t
(** A chunk that a loader brings. The loader runs at the first {!load}, at most
    once at a time, and calls its argument once with the value or an error
    message. After an error, the next {!load} runs the loader again. *)

val get : 'a t -> 'a option
(** The value, when it is here. *)

val load : 'a t -> (unit -> unit) -> unit
(** Start the loader, or join the load in progress, and call the function once
    when the load ends, with a value or with an error. It is called at once when
    the value is here or the last load failed. *)

val failure : 'a t -> string option
(** The message of the last load, when it failed. *)

val forget_failure : 'a t -> unit
(** Make a failed chunk loadable again: {!failure} gives [None] and the next
    {!load} runs the loader. *)

type any = Any : 'a t -> any
