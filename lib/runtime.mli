(** The host runtime: the queue of events, the instance tree, timers, Remote
    calls, Live subscriptions, the top-level error page and the replay of the
    answers of a server render. It reaches the platform only through
    {!Platform.t}, so the browser and the in-memory platform of the tests run
    this same code.

    Preload. The runtime carries out {!Action.Preload}: it sends the call at
    once and keeps a [200] answer for 30 seconds from its arrival, for one use.
    The next call of the same endpoint path with the same encoded input takes
    the kept answer with no request, or waits for the preload that is still in
    flight. A failed or non-[200] preload is not kept: a call that waited for it
    sends its own request. A preload of a call that is kept or in flight does
    nothing.

    Chunks. The runtime carries out {!Action.Load_chunk}. The first paint waits
    until the first view has no {!Node.val-chunk} that is loading, so a renderer
    that adopts the server screen adopts it with the code of its page; timers,
    calls and resources start at once. *)

val start :
  ?dev:bool ->
  ?on_error:(exn -> Node.error -> unit) ->
  ?answers:Answers.t ->
  ?live_config:Live_client.config ->
  ?on_route:(Router.event -> 'e) ->
  platform:Instance.message Platform.t ->
  create:
    (dev:bool ->
    on_error:(exn -> Node.error -> unit) option ->
    ('p, 's, 'e) Instance.t
    ) ->
  unit ->
  unit
(** Create the tree with [create], answer its calls from [answers] before the
    first paint, paint, and then run every event that the platform raises, one
    at a time: events raised while one runs are queued. [on_route] makes the
    event for the root component from each URL change and each link preload that
    the renderer reports.

    Transition log: [create] gives the tree its hook (see {!Instance.mount}).
    The runtime gives the root each URL change and preload with the cause
    {!Instance.constructor-Route} (see {!Instance.route}). After an error page,
    "Try again" calls [create] again, and the new tree gets the same hook.

    Errors. The runtime is a top-level boundary: an exception that no
    {!Node.val-boundary} catches paints an error page (with "Try again", which
    starts the root again with fresh state, and "Reload the page"), and goes to
    [on_error] and [platform.io.log]. A step that raises changes nothing: its
    state, view and actions are dropped with the tree. In [dev] mode the page
    shows the details, and an exception during the first paint is raised again
    after the error page shows. *)
