(** The router: a machine whose states are the routes.

    Define a [State] GADT whose constructors are the routes and whose index is
    the data of the route (its path parameters and its typed query), with [path]
    that gives the {!Route.t} of each:

    {[
      module State = struct
        type _ t = Issues : filters t | Issue : int t | Not_found : string t

        let path : type data. data t -> data Route.t = function
          | Issues ->
              Route.(s "issues" +? filters)
          | Issue ->
              Route.(s "issues" / int)
          | Not_found ->
              Route.rest
      end

      module Router = Router.Make (State)

      let routes =
        Router.routes ~not_found:Not_found [ Issues; Issue; Not_found ]
    ]}

    Any machine can navigate with [Router.navigate routes Issue 42]. The app
    root is [Router.make routes ~render], defined after the pages.

    The snapshot of the router machine is the current route with its data. The
    URL is that snapshot printed by [path], after the [base] of the routes. A
    navigation command, a press on a {!Make.link}, the browser back and forward
    buttons and a typed URL all reach the machine as {!event} [Url_changed]. The
    route is found by trying the listed routes in order, so the last one must
    match every path (use {!Route.rest}): a bad path goes to that route. A query
    value that does not parse falls back to its default, so it never makes a bad
    route. *)

type event =
  | Url_changed of Url.t
      (** Sent by a host when the URL changes: at a navigation command, a link
          press, and when the user moves in the history. *)
  | Preload of Url.t
      (** Sent by a host when a link may be pressed soon (see {!preload}). The
          router answers with a {!Cmd.Preload} for each call that the [preload]
          function of the routes gives for the route of the URL, and a
          {!Cmd.Load_chunk} for each chunk that the [chunks] function of
          {!Make.make} gives, and changes nothing else. *)

type preload =
  | Intent
      (** Preload when the pointer enters the link or the link gets focus. *)
  | Viewport  (** Preload when the link enters the viewport. *)
  | Never

module Make (State : sig
  type 'data t

  val path : 'data t -> 'data Route.t
  (** The route of a state. *)
end) : sig
  type snapshot = At : 'data State.t * 'data -> snapshot

  type targets =
    | []
    | ( :: ) : 'data State.t * targets -> targets
        (** A list of routes with different data, as in [[ Issues; Issue ]]. *)

  type routes

  val routes :
    ?base:string ->
    ?not_found:'data State.t ->
    ?reenter:targets ->
    ?preload:(snapshot -> Remote.request list) ->
    ?chunked:targets ->
    targets ->
    routes
  (** The listed routes, in the order they are tried. [base], such as ["/app"],
      is the path that every URL of the app starts with: {!parse} needs it and
      takes it off, {!print} puts it first. [not_found] names the route that
      {!is_not_found} and the server status refer to. A route in [reenter]
      starts its page again when only its parameters change; any other route
      keeps its page, which receives the new parameters as a props event
      ([goto]). [preload] gives the Remote calls that the page of a route makes
      when it starts, so that a link can fetch them early (see {!link}).
      [chunked] lists the lazy routes: those whose page is a {!Node.val-chunk}.
      A link to one of them preloads its code (the [chunks] of {!make}) as it
      preloads the calls. The code itself is named in {!make}, at the app root,
      because a page links to routes and so depends on [routes]. *)

  val parse : routes -> Url.t -> snapshot
  (** Raises [Failure] when the path does not start with the [base] of the
      routes, or when no listed route matches the path. *)

  val print : routes -> snapshot -> Url.t
  (** The URL of a snapshot, after the [base] of the routes. *)

  val is_not_found : routes -> snapshot -> bool
  (** [true] when the snapshot is in the [not_found] route of [routes]. *)

  val navigate : ?hash:string -> routes -> 'data State.t -> 'data -> 'e Cmd.t
  (** Push a history entry. With [hash], the URL has that hash, and the browser
      scrolls to the element with that id after the paint. The router ignores
      the hash. *)

  val replace : ?hash:string -> routes -> 'data State.t -> 'data -> 'e Cmd.t
  (** Replace the current history entry. Use it for a change of the query only.
      [hash] is as in {!navigate}. *)

  val link :
    ?hash:string ->
    ?preload:preload ->
    ?current:snapshot ->
    routes ->
    'data State.t ->
    'data ->
    'e Node.attr list ->
    'e Node.t list ->
    'e Node.t
  (** An [a] with [href] and [data-silica-link]. The browser host turns a press
      into a history push without a page load. [hash] is as in {!navigate}.

      [preload] (default [Intent]) says when the host fetches the calls that the
      [preload] function of the routes gives for the route of the link, and
      loads the code of the route when it is [chunked]: the link has
      [data-silica-preload="intent"] or ["viewport"]. A link with [Never], or
      whose route has no calls and is not [chunked], has no such attribute.

      [current] is the route now. The link has [aria-current="page"] when its
      path equals the path of [current], and [data-active] when its path is a
      prefix of the path of [current] (by whole segments), as for the link of a
      parent page. The query is ignored for both. *)

  val link_replace :
    ?hash:string ->
    ?preload:preload ->
    ?current:snapshot ->
    routes ->
    'data State.t ->
    'data ->
    'e Node.attr list ->
    'e Node.t list ->
    'e Node.t
  (** Like {!link}, but the press replaces the history entry. For a link that
      changes only the query. *)

  type decision =
    | Allow
    | Redirect of snapshot
        (** Go to this route instead, and replace the URL. *)
    | Cancel
        (** Stay: the router raises {!Cmd.Revert_navigation}, so the URL and the
            history position go back to those of the current route. *)

  val make :
    routes ->
    ?guard:(from:snapshot option -> target:snapshot -> decision) ->
    ?exit:(snapshot -> event Cmd.t list) ->
    ?title:(snapshot -> string) ->
    ?chunks:(snapshot -> Chunk.any list) ->
    render:(snapshot -> event Node.t) ->
    Url.t ->
    (snapshot, event) Component.t
  (** The router component for the app root, starting at the URL. [render] shows
      the page of the current route, as a component node. [guard] runs before a
      URL change to another URL takes effect, and at the start with
      [from = None]: use it for "go to login". At the start, [Redirect] starts
      in the other route and raises {!Cmd.Replace} with its URL (a server render
      answers it with a redirect), and [Cancel] starts in the route of the URL,
      as there is no route to stay in. For unsaved data inside a page, use
      {!Source.block}. [exit] runs when a route state is left, as the [exit] of
      its state. [title] gives the document title of a route: the router raises
      {!Cmd.Set_title} with it at the start and at each change of route or
      parameters, before the commands of the page, so a page machine that sets
      its own title wins. A change to the same printed URL does nothing.
      [chunks] gives the code that [render] shows with {!Node.val-chunk} for a
      route: the router loads it when a link to that route preloads (see
      {!val-routes}, [chunked]). *)
end
