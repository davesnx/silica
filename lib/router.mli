(** The router: a machine whose states are the routes of the app.

    The routes are a plain variant with the data of each route (its path
    parameters and its typed query). Each {!val-route} gives the {!Route.t} of
    one constructor, how to build the constructor from its data and how to read
    the data back:

    {[
      type route =
        | Issues of filters [@route Route.(s "issues" +? filters)]
        | Issue of int [@route Route.(s "issues" / int)]
        | Not_found of string [@route Route.rest]
      [@@deriving routes]

      let routes =
        Router.routes
          ~not_found:(function Not_found _ -> true | _ -> false)
          route_routes
    ]}

    The deriver [[@@deriving routes]] of [silica.ppx] makes [route_routes], one
    {!val-route} for each constructor, in order, from its [[@route]] path. By
    hand, the same table is
    [Router.route Route.(s "issues" / int) (fun n -> Issue n) (function Issue n
     -> Some n | _ -> None)] for each constructor; the check [route-round-trip]
    of [Check.app] finds a pair that does not agree.

    Any machine can navigate with [Router.navigate routes (Issue 42)]. The app
    root is [Router.make routes ~render], defined after the pages.

    The snapshot of the router machine is the current route. The URL is that
    route printed by its {!Route.t}, after the [base] of the routes. A
    navigation action, a press on a {!link}, the browser back and forward
    buttons and a typed URL all reach the machine as {!event} [Url_changed]. The
    route is found by trying the listed routes in order, so the last one must
    match every path (use {!Route.rest}): a bad path goes to that route. A query
    value that does not parse falls back to its default, so it never makes a bad
    route. *)

type event =
  | Url_changed of Url.t
      (** Sent by a host when the URL changes: at a navigation action, a link
          press, and when the user moves in the history. *)
  | Preload of Url.t
      (** Sent by a host when a link may be pressed soon (see {!preload}). The
          router answers with a {!Action.Preload} for each call that the
          [preload] function of the routes gives for the route of the URL, and a
          {!Action.Load_chunk} for each chunk that the [chunks] function of
          {!make} gives, and changes nothing else. *)

type preload =
  | Intent
      (** Preload when the pointer enters the link or the link gets focus. *)
  | Viewport  (** Preload when the link enters the viewport. *)
  | Never

type 'r route
(** One route of an app whose routes are ['r]. *)

val route : 'data Route.t -> ('data -> 'r) -> ('r -> 'data option) -> 'r route
(** [route path make data]: [make] builds the route from the data that [path]
    parses, and [data] gives the data back for a route of this kind, [None] for
    the other routes. [data (make d)] must be [Some d]. *)

type 'r routes

val routes :
  ?base:string ->
  ?not_found:('r -> bool) ->
  ?reenter:('r -> bool) ->
  ?preload:('r -> Remote.request list) ->
  ?chunked:('r -> bool) ->
  'r route list ->
  'r routes
(** The listed routes, in the order they are tried. [base], such as ["/app"], is
    the path that every URL of the app starts with: {!parse} needs it and takes
    it off, {!print} puts it first. [not_found] tells the route that
    {!is_not_found} and the server status refer to. A route in [reenter] starts
    its page again when only its parameters change; any other route keeps its
    page, which gets the new parameters as new props. [preload] gives the Remote
    calls that the page of a route makes when it starts, so that a link can
    fetch them early (see {!link}). [chunked] tells the lazy routes: those whose
    page is a {!Node.val-chunk}. A link to one of them preloads its code (the
    [chunks] of {!make}) as it preloads the calls. The code itself is named in
    {!make}, at the app root, because a page links to routes and so depends on
    [routes]. *)

val parse : 'r routes -> Url.t -> 'r
(** Raises [Failure] when the path does not start with the [base] of the routes,
    or when no listed route matches the path. *)

val print : 'r routes -> 'r -> Url.t
(** The URL of a route, after the [base] of the routes. Raises [Failure] when no
    listed route gives data for it. *)

val name : 'r routes -> 'r -> string
(** The pattern of the route, as {!Route.describe} gives it: the name of the
    state of the router machine. *)

val is_not_found : 'r routes -> 'r -> bool
(** The [not_found] function of the routes; [false] when there is none. *)

val navigate : ?hash:string -> 'r routes -> 'r -> 'e Action.t
(** Push a history entry. With [hash], the URL has that hash, and the browser
    scrolls to the element with that id after the paint. The router ignores the
    hash. *)

val replace : ?hash:string -> 'r routes -> 'r -> 'e Action.t
(** Replace the current history entry. Use it for a change of the query only.
    [hash] is as in {!navigate}. *)

val link :
  ?hash:string ->
  ?preload:preload ->
  ?current:'r ->
  'r routes ->
  'r ->
  'e Node.attr list ->
  'e Node.t list ->
  'e Node.t
(** An [a] with [href] and [data-silica-link]. The browser host turns a press
    into a history push without a page load. [hash] is as in {!navigate}.

    [preload] (default [Intent]) says when the host fetches the calls that the
    [preload] function of the routes gives for the route of the link, and loads
    the code of the route when it is [chunked]: the link has
    [data-silica-preload="intent"] or ["viewport"]. A link with [Never], or
    whose route has no calls and is not [chunked], has no such attribute.

    [current] is the route now. The link has [aria-current="page"] when its path
    equals the path of [current], and [data-active] when its path is a prefix of
    the path of [current] (by whole segments), as for the link of a parent page.
    A link to the root (the path of the routes, ["/"] with no base) is
    [data-active] only on the root itself, as every path starts with it. The
    query is ignored for both. *)

val link_replace :
  ?hash:string ->
  ?preload:preload ->
  ?current:'r ->
  'r routes ->
  'r ->
  'e Node.attr list ->
  'e Node.t list ->
  'e Node.t
(** Like {!link}, but the press replaces the history entry. For a link that
    changes only the query. *)

type 'r decision =
  | Allow
  | Redirect of 'r  (** Go to this route instead, and replace the URL. *)
  | Cancel
      (** Stay: the router raises {!Action.Revert_navigation}, so the URL and
          the history position go back to those of the current route. *)

val state_codec : 'r routes -> 'r Codec.t
(** A route as its printed URL, for {!Machine.register_codecs} on the machine of
    the router component. *)

val event_codec : event Codec.t
(** [["Url_changed", url]] or [["Preload", url]]. *)

type made = { made : 'r. 'r routes -> (unit, 'r, event) Machine.t -> unit }

val on_make : made ref
(** Development code that sets it sees each router machine that {!make} makes,
    with its routes. The hot reload of [silica dev] hands the route over with
    {!state_codec} (see {!Handoff.codecs}), so the router keeps its route, with
    no codec in the registry of {!Machine.register_codecs}. A release bundle
    keeps the default, which does nothing. *)

val make :
  'r routes ->
  ?guard:(from:'r option -> target:'r -> 'r decision) ->
  ?exit:('r -> event Action.t list) ->
  ?title:('r -> string) ->
  ?chunks:('r -> Chunk.any list) ->
  render:('r -> event Node.t) ->
  Url.t ->
  (unit, 'r, event) Component.t
(** The router component for the app root, starting at the URL. [render] shows
    the page of the current route, as a component node. [guard] runs before a
    URL change to another URL takes effect, and at the start with [from = None]:
    use it for "go to login". At the start, [Redirect] starts in the other route
    and raises {!Action.Replace} with its URL (a server render answers it with a
    redirect), and [Cancel] starts in the route of the URL, as there is no route
    to stay in. For unsaved data inside a page, use {!Host.block}. [exit] runs
    when a route is left, as the [exit] of its state, and before the page starts
    again when a [reenter] route gets new parameters. [title] gives the document
    title of a route: the router raises {!Action.Set_title} with it at the start
    and at each change of route or parameters, before the actions of the page,
    so a page machine that sets its own title wins. A change to the same printed
    URL does nothing. [chunks] gives the code that [render] shows with
    {!Node.val-chunk} for a route: the router loads it when a link to that route
    preloads (see {!val-routes}, [chunked]). The router names its events
    ["url changed"] and ["preload"]. *)
