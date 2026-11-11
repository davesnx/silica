Make a new app.

  $ silica new counter-app
  created .gitignore
  created .ocamlformat
  created AGENTS.md
  created app/melange/dune
  created app/native/dune
  created app/shared/api.ml
  created app/shared/counter.mlx
  created check/dune
  created check/main.ml
  created check/spec/counter.dot
  created check/spec/counter.qnt
  created check/spec/counter.spec.txt
  created check/spec/counter.views.txt
  created client/client_main.ml
  created client/dune
  created client/package.json
  created dune-project
  created server/dune
  created server/main.ml
  created test/counter_page.ml
  created test/counter_steps.ml
  created test/dune
  created test/expect.ml
  
  Next:
    cd counter-app
    npm --prefix client install
    dune build
    dune exec server/main.exe

The directory name is the project name.

  $ grep counter-app counter-app/dune-project counter-app/client/package.json
  counter-app/dune-project:(name counter-app)
  counter-app/dune-project: (name counter-app)
  counter-app/client/package.json:  "name": "counter-app-client",

A second run changes nothing.

  $ silica new counter-app
  counter-app already holds the template; nothing changed.
  
  Next:
    cd counter-app
    npm --prefix client install
    dune build
    dune exec server/main.exe

A changed file stops the command, and it writes nothing.

  $ echo "(* mine *)" >> counter-app/server/main.ml
  $ rm counter-app/AGENTS.md
  $ silica new counter-app
  silica new: these files differ from the template:
    server/main.ml
  Nothing was written. Move these files away, or choose another directory.
  [1]
  $ test -e counter-app/AGENTS.md
  [1]
  $ silica new counter-app > /dev/null 2>&1
  [1]

Restore the file; the next run writes the missing one.

  $ silica new restored > /dev/null
  $ cp restored/server/main.ml counter-app/server/main.ml
  $ silica new counter-app | head -1
  created AGENTS.md

A name that is not a valid project name stops the command.

  $ silica new 1app
  silica new: "1app" is not a valid project name. Use letters, digits, '_' and '-', and start with a letter.
  [1]

The app builds its server and its client bundle, and its tests pass.

  $ ln -s "$DUNE_SOURCEROOT/example/client/node_modules" counter-app/client/node_modules
  $ cd counter-app
  $ dune build server/main.exe client/dist 2>&1
  $ ls _build/default/client/dist | sed 's/-[A-Z0-9]*\./-HASH./'
  client_main-HASH.js
  client_main-HASH.js.map
  $ dune test 2>&1
  $ silica check 2>&1
  counter: 16 states, 45 transitions
  passed (exit 0)
  $ silica check --json | grep -E '"(exit_code|outcome)"'
    "exit_code": 0,
    "outcome": "passed",

A change of behaviour fails the tests: the golden spec names the changed edge.

  $ sed 's/edge counting -> saving on save/edge counting -> saved on save/' check/spec/counter.spec.txt > changed.txt
  $ mv changed.txt check/spec/counter.spec.txt
  $ dune test 2>&1 | grep -E '^ *(golden-drift|removed|added)'
  golden-drift: The golden file spec/counter.spec.txt differs from what the check found.
    removed edge counting -> saved on save
    added edge counting -> saving on save
  $ dune exec ./check/main.exe -- golden check/spec --write > /dev/null
  $ dune test 2>&1
  $ dune build @fmt 2>&1
