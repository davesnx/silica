(** An SVG drawing of a {!Statechart_layout.t}. Colors come from the silica-ui
    theme variables ([--ui-text], [--ui-muted], [--ui-border], [--ui-surface],
    [--ui-accent], [--ui-accent-text], [--ui-danger]), with the light theme as
    fallback, so the same node works in a themed page and as a standalone SVG
    file (render it with {!Render.to_string}).

    Class names for styles and tests: [chart] on the [svg]; [chart-node] on each
    state group, with [is-active], [is-final], [is-initial] and [is-drift], and
    the state name in [data-state]; [chart-edge] on each edge group, with
    [is-taken], [is-back] and [is-drift], and the states in [data-from] and
    [data-target]; [chart-start] on the start mark; [chart-note] on the
    {!Statechart_layout.t.note}.

    The label of an edge between two states is drawn on a box of the surface
    color, so the line stops at the label and goes on after it. Drift nodes and
    edges ({!Statechart_layout.layout} with [observed]) are dashed, in the
    danger color. *)

val view :
  ?current:string -> ?taken:string * string -> Statechart_layout.t -> 'e Node.t
(** [current] fills the node of that state with the accent color. [taken] is the
    [(from, target)] of an edge to draw in the accent color, such as the last
    step. *)
