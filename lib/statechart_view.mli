(** An SVG drawing of a {!Statechart_layout.t}. Colors come from the silica-ui
    theme variables ([--ui-text], [--ui-muted], [--ui-border], [--ui-surface],
    [--ui-accent], [--ui-accent-text]), with the light theme as fallback, so the
    same node works in a themed page and as a standalone SVG file (render it
    with {!Render.to_string}).

    Class names for styles and tests: [chart] on the [svg]; [chart-node] on each
    state group, with [is-active], [is-final] and [is-initial], and the state
    name in [data-state]; [chart-edge] on each edge group, with [is-taken] and
    [is-back], and the states in [data-from] and [data-target]; [chart-start] on
    the start mark. *)

val view :
  ?current:string -> ?taken:string * string -> Statechart_layout.t -> 'e Node.t
(** [current] fills the node of that state with the accent color. [taken] is the
    [(from, target)] of an edge to draw in the accent color, such as the last
    step. *)
