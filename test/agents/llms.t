The generator of llms.txt and llms-full.txt reads the .mld pages of a docs
directory. llms.txt takes its summary from the first paragraph of index.mld
and its sections and descriptions from the section {1:pages ...} of index.mld.

  $ ../../site/gen/llms.exe index https://example.org llms_docs
  # Sample
  
  > Sample is a **small** fixture for the converter.
  
  Each page below is Markdown. All pages in one file: https://example.org/llms-full.txt
  
  ## Start
  
  - [Markup](https://example.org/docs/markup.md): every construct that the docs use.
  
  ## Elsewhere
  
  - [A skill](https://example.com/skill): a link that is not a page.

Each page becomes Markdown: odoc markup, references to pages, sections and
values, code blocks, lists and tables.

  $ ../../site/gen/llms.exe page https://example.org llms_docs markup
  # Markup
  
  A paragraph with `code`, **bold**, *italic*, *emphasis*, and a [link](https://example.com). Plain text: snake_case, \_lead, 2 \* 3, a \<tag>, ``x`y`` and `` `z `` in code.
  
  ## A labelled heading
  
  ### Same
  
  ### Same
  
  References: `Check`, `Check.run`, `Check.report`, `Silica.Machine.Sample`, the rules (`Check.rules`), [Sample](https://example.org/docs), [A labelled heading](https://example.org/docs/markup#labelled), [Same](https://example.org/docs/markup#same-2), [here](https://example.org/docs/markup#labelled).
  
  - one
  - two with `Check.run`
  - nested
  
  1. first
  2. second
  
  1. a paragraph
  
     ```ocaml
     let x = 1
     ```
  
  2. done
  
  | Rule | Fix |
  |---|---|
  | `ignored-event` | handle it |
  | `dead-end` | **add** a way out |
  
  ````ocaml
  let quoted = "```"
  ````
  
  ```dune
  (rule
   (alias runtest))
  ```
  
  ```
  verbatim {text}
    kept
  ```
  
  \1. A paragraph that starts with a number.

llms-full.txt holds the index page, then each listed page in the listed order.

  $ ../../site/gen/llms.exe full https://example.org llms_docs | grep '^# '
  # Sample
  # Markup

A page that the index does not list stops the build.

  $ cp -rL llms_docs unlisted
  $ printf '{0 Orphan}\n\nNo one links here.\n' > unlisted/orphan.mld
  $ ../../site/gen/llms.exe index https://example.org unlisted
  llms: index.mld: the pages section lists orphan.mld 0 times, not once
  [1]

A link to an unknown page or section stops the build.

  $ mkdir broken && cp llms_docs/index.mld broken/
  $ printf '{0 Markup}\n\nSee {!page-nowhere}.\n' > broken/markup.mld
  $ ../../site/gen/llms.exe full https://example.org broken
  llms: markup.mld: a link to an unknown page "nowhere"
  [1]
  $ printf '{0 Markup}\n\nSee {!section-nowhere}.\n' > broken/markup.mld
  $ ../../site/gen/llms.exe full https://example.org broken
  llms: markup.mld: a link to an unknown anchor markup#nowhere
  [1]

Markup with no Markdown form stops the build.

  $ printf '{0 Markup}\n\nSee {^ up}.\n' > broken/markup.mld
  $ ../../site/gen/llms.exe full https://example.org broken
  llms: markup.mld: unsupported inline markup at line 3
  [1]

The API page of an interface leaves out what odoc leaves out: the items
between two stop comments.

  $ printf '(** A sample. *)\n\nval shown : int\n(** Shown. *)\n\n(**/**)\n\nval hidden : int\n(** Hidden. *)\n\n(**/**)\n\nval again : int\n' > sample.mli
  $ ../../site/gen/llms.exe api "API: sample" - sample.mli
  # API: sample
  
  ## Sample
  
  A sample.
  
  ```ocaml
  val shown : int
  ```
  
  Shown.
  
  ```ocaml
  val again : int
  ```
