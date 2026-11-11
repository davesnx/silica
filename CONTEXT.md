# Silica

An OCaml view framework where all behaviour is described as statecharts, and
hosts (browser, server, model checker) interpret them.

## Data kinds

**Local**:
Data owned by the client. It may hold values that cannot be serialised.
_Avoid_: state (as a data kind), client state

**Remote**:
Data obtained from a typed endpoint on the server, one request per call.
_Avoid_: fetch, API data, server state

**Live**:
Data owned by the server and synced to the client as it changes: a topic
the client subscribes to, or the state of an island for one connection.
_Avoid_: realtime, socket state, LiveView

## Behaviour

**State**:
A constructor of the plain variant of a machine, with the data it carries, such
as `Typing` carrying the query. Each state has a **state name**, which the
**shape** of the machine gives; no two states have the same name.
Never a data kind.
_Avoid_: node, mode, control state

**Shape**:
The value that tells a machine about its state type or its event type: the
name of each value and, for states, the list of all names. `Machine.make` takes
the shape of the states as `~shape` and the shape of the events as `~events`;
the `[@@deriving state_shape]` and `[@@deriving event_shape]` derivers make
them.
_Avoid_: schema, state names (as the name of the value)

**Event name**:
The name of an event, from the event shape of its machine
(`Machine.event_name`). The checker names the edges of the spec with it.
_Avoid_: label (as the name of the value)

**Codec**:
An encoder and a decoder between a type and JSON (`Silica.Codec.t`). Endpoints
and topics have codecs for the wire. The codecs of the snapshots and events of
a machine are registered apart from it (`Machine.register_codecs`), by
development entry points only, so release bundles carry none.
_Avoid_: serializer

**Snapshot**:
The value a machine holds at one moment: a value of its state type, one state
and its data. A machine whose data belongs to every state holds a record with a
`phase` field, and the name of the snapshot is the name of the phase.
_Avoid_: state (as the current value), model, store

**Machine**:
A statechart: states, the events it accepts, and one update function that gives
the next step for each state and event. Made with `Machine.make`, or
`Machine.make_with_props` for a component with props.
_Avoid_: reducer, store, actor

**Update**:
The function of a machine from the props (if any), the snapshot and the event
to the next step.
_Avoid_: handler (a handler is in the view), reducer

**Next**:
What the update gives: `Go` a snapshot, `Go_with` a snapshot and actions,
`Stay` with actions, `Reenter` a snapshot, or `Ignore`. `Machine.map_next`
puts the next step of a part, such as a phase, back into the whole snapshot.
_Avoid_: outcome, result

**Transition**:
A step of a machine from one state to a state on an event. The Quint export
writes one Quint `action` for each.
_Avoid_: action

**Owns**:
The function of a machine that gives the timers, Remote calls, subscriptions
and held sources of each snapshot. A source runs while the snapshot owns it.
_Avoid_: invoke, effect

**Source key**:
The identity of a source in what a snapshot owns: the path and input of a call,
the topic and key of a subscription, the delay of a timer. After each step the
machine stops the sources whose key went away and starts the new ones; the
others keep running.
_Avoid_: source id (an id names a running source for the host)

**Props**:
The value that a parent gives to a child component. The update, `owns` and the
view get it; `on_props` reacts when it changes. Props are not an event and are
not kept in the snapshot.
_Avoid_: props event, attributes

**Ignored event**:
An event of a view handler, a timer, a call answer, a subscription or an action
result that the machine does not handle (its update gives `Ignore`). The
checker reports it as the violation `ignored-event`. `Machine.can` tells
whether a machine handles an event, so a view can disable the element.
_Avoid_: unhandled event, dead button

**Component**:
A machine together with the view of its states. Each rendered component runs
its own machine while it is rendered; it gets props from its parent and sends
output to it through function props. A rendered component is the same instance
as before when it has the same position, machine id and key.
_Avoid_: widget, region, child machine

**Spec**:
What a checker run found about a machine: the states it reached
(initial state first, then by name), the timers and sources of each, and the
edges it took, each with the name of its event. The Quint export, the text
form and the diagrams read it.
_Avoid_: schema, graph (as the name of the value)

**Action**:
A request from a machine for the host to do something outside the machine.
_Avoid_: command, effect, side effect

**Host**:
The thing that runs machines and carries out their actions: the browser, the
server, or the model checker.
_Avoid_: runtime, driver, platform

**Renderer**:
The part of a platform that paints views, turns platform input into events and
runs platform actions. The queue, the instance tree, timers, calls and Live
subscriptions stay in the host runtime, which is the same for every platform.
_Avoid_: driver, backend

**Platform**:
What the host runtime runs on, given as one value: a renderer, the current URL,
and the io (a clock, one HTTP request form, a Live socket, the CSRF token and a
log). The browser and the in-memory platform of the tests are two platforms of
the same runtime.
_Avoid_: driver, adapter (an adapter is a server term), environment

**Island**:
A place in a view where a Live component runs, owned by its own connection.
_Avoid_: portal, embed

**Topic**:
A named, keyed piece of server data with a state type, published by the
server and sent in full to every subscriber, with a sequence number.
_Avoid_: channel, room, stream

**Subscription**:
One socket that follows one key of a topic. A snapshot owns it through
`Source.live`: it starts when a snapshot starts to own it and stops when the
snapshot no longer owns it. The client reconnects by itself and tells the state when it is offline.
_Avoid_: listener, watcher

**Status**:
The value that shows where Remote or Live data is: idle, loading, ready or
failed, with the old data kept when there is some. Live data is stale while
the socket is down.
_Avoid_: loading flag, query state

**Optimistic**:
A pure value that holds the last confirmed data and the changes that the
server has not yet confirmed. Its value shows the changes on the data. A change
**settles** when the Remote answer or a newer Live state confirms it, and goes
back when the call fails. It is first-order data, so a snapshot can hold it.
_Avoid_: pending state, draft

**Boundary**:
A place in the view that catches an exception from the components under it,
shows a fallback, and can reset them.
_Avoid_: error page, try block

## Verification

**Answer**:
The end of a Remote call, as the machine gets it: `Ok` with the output,
`Rejected` with the application error of the handler, or `Failed` with the
error of the transport. `Remote.error_to_string` gives a sentence for the
error.
_Avoid_: response, result

**Sample**:
A value that the model checker delivers in place of the server: an answer of
an endpoint or a state of a topic. Failures of a call are added by themselves.
_Avoid_: mock, fixture, stub

**Unreached state**:
A name in the `states` of a machine that a checker run never reached. The
checker reports it, unless an allowance with the rule `unreached-state`
covers it.
_Avoid_: dead state

**Check**:
A configured run of the model checker: the component or app, its samples,
inputs, bounds, properties and allowances. `Check.run` gives its report.
_Avoid_: checker config

**Rule**:
A built-in test of the checker, with an id (such as `ignored-event`), a
message and a fix hint. `Check.rules` lists them.
_Avoid_: built-in invariant, lint

**Property**:
A test of the app that must hold in every reached state, made with
`Check.always`, with its own name and optional hint.
_Avoid_: invariant (as the name of the value), assertion

**Violation**:
A rule or property that fails, with its message, fix hint, shortest trace,
state and text view at the failing step. A report holds every violation, the
first of each rule, state, element and message.
_Avoid_: error, failure (as the name of the value)

**Fix hint**:
The sentence of a violation that tells an agent or a person how to fix it.
_Avoid_: suggestion, help text

**Allowance**:
A rule that a check turns off, with its reason, made with `Check.allow`. The
report lists each allowance and how often it applied.
_Avoid_: ignore list, suppression

**Cause**:
Where a step of a trace came from: an element and handler of the view, an
input, a timer, a call answer, a subscription update, a held source, an action
result, new props or the browser history.
_Avoid_: origin, via

**Spec drift**:
A change of the exported Quint or Graphviz files of a machine: the checker run
found other states or edges than before.
_Avoid_: dead edge, stale spec

**Role**:
What an element is to a screen reader, such as `button`, `link` or
`textbox`: its `role` attribute, or the role of its tag.
`Silica.Accessibility` computes it from the view.
_Avoid_: kind, type of element

**Accessible name**:
The name of an element for a screen reader, such as "Save" for a button: from
`aria-labelledby`, `aria-label`, a `label`, `alt`, the content or `title`.
Tests find elements by their role and accessible name.
_Avoid_: label (a `label` element is one source of the name), text

**Text view**:
A view as text, one line per element that has a role or a handler, with its
name, its states and its handlers (`Silica.View_text`). An agent or a reviewer
reads and compares it with no browser.
_Avoid_: snapshot (a snapshot is a state of a machine), DOM dump

**Transition log**:
One record (`Silica.Transition.t`) for each start, step, ignored event, props
change and end of each instance, with the state names before and after, the
event, the actions and the cause. A host gives it to a hook; with no hook
nothing is recorded.
_Avoid_: event log, history (the history is the list of URLs)

**Cause**:
What made a step: the element of a handler (by role, accessible name and
path), a timer, the answer of a call, a Live update, a held source, the result
of an action, new props, a child, a URL change or a message of the host.
_Avoid_: trigger, source (a source is a held resource)

**Instance path**:
The name of an instance in its tree: the machine id of the root, then for each
child the position of its component node in the view of its parent (or its
key) and its machine id, such as `app/0.2:router/#inbox:issue-list`.
_Avoid_: instance id (the instance number in timer ids)

**Trace**:
The transitions of one run as a file (`Silica.Trace`): JSON Lines, one header
line, then one record per line. The memory platform, the browser recorder, the
Live runner of the server and the checker write the same format.
_Avoid_: log file, recording

## Navigation

**Page**:
What the app shows for one route.
_Avoid_: screen, view (as a noun for a page)

**Route**:
A state of the router, a constructor of the plain route variant: it names a
page, carries the page's parameters, and converts to and from a URL with its
`Router.route`.
_Avoid_: path, link target

**Router**:
The machine whose states are the routes. Its view renders the page for the
current route.
_Avoid_: navigator

**Guard**:
The function of the router that decides, from the current route and the target
route, to allow a navigation, to go to another route instead, or to cancel it.
A cancel puts the history back where it was.
_Avoid_: middleware, interceptor

**Block**:
A held source of a page that has unsaved data. While it is held, no navigation
happens: a link press, a navigation action, back and forward reach the page as
an attempt, which it can resume; closing the tab asks first.
_Avoid_: prompt, confirm guard

**Preload**:
A Remote call that the router makes before a page needs it, when the user
points at or focuses a link to the page. The answer waits 30 seconds for the
same call of the page, and is used once.
_Avoid_: prefetch, loader

**Lazy route**:
A route whose page code is a chunk: the browser loads it the first time the
route shows, or when a link to it preloads. The first paint waits for the
chunks of the first view, so hydration adopts the server HTML with the page
code.
_Avoid_: async route, dynamic route

**Chunk**:
A value whose code may arrive later (`Silica.Chunk.t`): ready at once on the
native server, loaded by a dynamic import in the browser. A view shows it with
`Node.chunk`, with a view for the time it loads.
_Avoid_: bundle (the whole client file), module (the OCaml unit)

**Route title**:
The document title of a route, given by the `title` function of the router.
The router sets it at the start and at each change of route or data, before
the actions of the page, so a page can set its own.
_Avoid_: page title (for the function), head

**Route focus**:
The element that gets keyboard focus after a navigation to another page (an
element with `data-silica-route-focus`, else the root). The new title is read
out by a polite live region.
_Avoid_: autofocus

## Server

**Session**:
Temporary server data for one visitor, found by an opaque cookie id. It lives
8 hours when unused and 7 days at most. The store changes one session in one
step with `update`.
_Avoid_: user, login

**Session gone**:
The case where the server no longer knows the session of a Live socket, for
example after a restart. The server accepts the upgrade and closes the socket at
once with code 4005, because a browser cannot read the status of a refused
upgrade. The client does not reconnect: it reloads the page once, which gives it
a new session and token.
_Avoid_: auth error, 403 loop

**CSRF token**:
A value, tied to a session, that the page gives to the client. Every call and
every Live hello carries it, so a page of another site cannot act for the
visitor.
_Avoid_: nonce (a nonce belongs to one page response)

**Adapter**:
The code that connects the server package to one HTTP library. It only
translates: requests, whole WebSocket messages and files in, responses out.
_Avoid_: server (the server is the package), driver

**Asset manifest**:
The map from logical file names to the hashed files of the client build, read
from the esbuild metafile.
_Avoid_: bundle list

**Production build**:
The client build that dune makes by default: minified, split into an entry,
shared chunks and lazy route chunks, each file with a `.br` and a `.gz` copy.
`SILICA_MINIFY=false` gives the debug build.
_Avoid_: release build (the opam profile), dist

**Static export**:
The pages of a server written as files, with the settle answers of each page as
JSON files, so the site runs on a static host. A page that finds the
`silica-static` meta reads its Remote calls from those files; a call with no
file fails.
_Avoid_: prerender, SSG

**Budget**:
A limit on a measured number. Size budgets (brotli bytes per bundle) run in
`dune test`; time budgets run in `dune build @bench`.
_Avoid_: target, threshold

**Render skip**:
An instance whose snapshot, props and descendants did not change since its
last render is not rendered again: its last view is used. A view must be a
function of its snapshot.
_Avoid_: memo, shouldComponentUpdate

## Components (silica-ui)

**silica-ui**:
The package of headless components (`ui/`) built after Base UI: each one is a
component with its own machine, styled by the caller through classes and
`data-*` attributes.
_Avoid_: widget library, design system

**Popup**:
The part of a component that shows over the page while it is open: a dialog,
a popover, a menu, a list box or a tooltip. It is painted in a portal.
_Avoid_: overlay, modal (for all popups)

**Dismiss layer**:
A held source of an open popup that closes it on Escape or on a press outside
it, or when focus moves out of it. Layers stack: Escape and outside presses
reach the newest layer first, and a press inside a newer layer does not close an
older one. While a modal popup is open, the page behind it is `inert`.
_Avoid_: click-outside handler

**Anchor**:
A held source that keeps a popup next to another element (its anchor), on the
side and with the alignment of a placement, and moves it to the other side when
there is no room.
_Avoid_: positioner, tether

**Open request**:
The `open_` value that a parent gives in the props of a popup component. When
it changes, the component opens or closes. What the component does by itself is
an **open change**, which it reports to the parent with its reason.
_Avoid_: controlled prop

**Shortcut**:
A key binding of a page (`Source.keys`). It does not fire while the user types
in a text field (unless the key has Ctrl or Meta, or is Escape), and not when an
element handler already used the key.
_Avoid_: hotkey

**Token**:
A named CSS value of the design, such as the page background, the accent colour
or the corner radius (`--ui-surface`). silica-ui components use only tokens;
an app adds its own tokens to its themes.
_Avoid_: design variable, CSS variable (for the value of the design)

**Theme**:
A name, a colour scheme (light or dark) and a value for every token. The page
uses the theme of its **theme choice**: `System` (the light or the dark theme
of the app, as the system setting says, live) or one fixed theme. The choice is
the `data-theme` attribute of `<html>` and is kept in a cookie; a part of the
page can use another theme (a theme scope).
_Avoid_: skin, palette, dark mode (for the whole feature)

**Story**:
A named example of a component (`Silica_ui.Story.t`): a name, the source text
that a reader sees, the controls that change it, and a function that renders
it with the values of those controls. The source is the text of the compiled
file, embedded by a dune rule.
_Avoid_: demo, example (for a story), fixture

**Story control**:
A toggle or a choice of a story that changes what it renders. The values of
the controls are plain data that a parent machine keeps.
_Avoid_: knob, arg

**Component browser**:
The part of the Silica site (`/components`) with one page per silica-ui
component: its stories with their controls and source, and the first story in
each shipped theme side by side. It replaced the gallery app in v0.12; its
"Events" stories are the fixtures of the browser tests.
_Avoid_: storybook, gallery

## Apps

**Mini Linear**:
The reference app of the repository (`linear/`): a small issue tracker with a
list, a detail page, a create modal and live updates, over mocked data. It is
the test of Silica as a whole, not a product.
_Avoid_: demo, example (the example app is another, smaller app)

**Issue view**:
The filters, grouping and ordering of the mini Linear list. It is the query part
of the `Issues` route, so the URL holds it.
_Avoid_: saved view, display settings

**Conflict**:
A change sent with an older issue `version` than the server has. The last write
wins; the loser keeps the draft and sees the new value.
_Avoid_: merge, rebase
