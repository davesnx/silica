# silica

silica is a view framework for OCaml where all behaviour lives in statecharts. A component is a machine and a view.

silica is also a model checker: it explores every state that a component can reach and verifies its properties, with no browser and before a user finds the bug.

- State machines handle all the possible states, so they can be verified without running, replayed, debugged and run everywhere.
- The silica code runs on the server as native OCaml and in the browser as JavaScript, thanks to [Melange](https://melange.re).
- Data can live on the client (local state), on the server (remote data) or live (a WebSocket).
- It comes with a typed router: the routes are the states of a machine, and one value reads and writes each URL. It has guards, preload, unsaved-changes blocks and lazy routes.
- All browser actions and states are represented as state machines as well.
- Machines can compose.

## What this makes possible

Every state of the app is encoded, so silica can be more than a view framework:

- **A contract for AI agents**: an agent writes the machine, the model checker judges it, and you review the statechart diff, not the code.
- **Design conformance**: check that the code follows the statechart of a design or a spec.
- **Client and server monitoring**: a transition that the spec does not allow is an alert, and a funnel is a path in the graph.
- **Whole-app diagrams**: the statechart of every component and route, drawn from the code, so it is never out of date.
- **Tests from the graph**: every path is a test, and every reachable state is a story and a screenshot.

silica is experimental.

## License

MIT © [David Sancho](https://github.com/davesnx)
