(** A view as text that a person or a program can read and compare, with no
    browser. The checker, [Silica_memory] and the golden files print views with
    this one function.

    One line per element that has a role or a handler, indented by two spaces
    for each such element above it. A line is:
    - the role (see {!Accessibility.val-role}), or the tag when the element has
      a handler and no role;
    - the accessible name in quotes, when it is not empty;
    - the states that are set, in this order: [level=2], [value="..."],
      [checked] or [checked=false], [selected], [expanded] or [expanded=false],
      [disabled], [current=page], [href="..."], [modal]. The value of an
      [<input type="password">] prints as [value="••••••••"] whatever its length
      (see {!Secret}); an empty value prints as [value=""];
    - the handlers in brackets, such as [[press, key_down]].

    Other elements are left out and their children move up. The text that is not
    in such a line prints as [text "..."]: the text of the elements left out
    side by side is joined into one line, with a space where an element starts
    or ends and white space collapsed. As in a browser, a block element ends
    that line: the text before it, in it and after it print on separate lines.
    The block elements are those that a browser shows as a block by default:
    [p], [div], [li], [section], [main], [form], [fieldset], the headings, lists
    and table parts, [br], [hr] and the others of the HTML rendering rules.
    [label], [span], [a], [strong] and the other inline elements do not end the
    line. Text inside an element whose role takes its name from its content (see
    {!Accessibility.name_from_content}) is its name and does not print again.

    A component node prints as [component <id>], an island as
    [island <channel>], a chunk that loads as [loading]. Hidden elements (see
    {!Accessibility.hidden}) and their children are left out. The children of a
    portal print at the place of the portal. In a quoted string, a double quote
    and a backslash are escaped with a backslash. Each line ends with a newline.
*)

val to_string : 'e Node.t -> string

val visible : 'e Node.t -> string
(** The text of the view that the accessibility tree has: every text node, side
    by side with no separator, as the [textContent] of the page, without the
    text of hidden elements (see {!Accessibility.hidden}: also [inert] and
    [aria-hidden] ones, which a browser draws), of component nodes and of
    loading chunks. *)

val shows : 'e Node.t -> string -> bool
(** [shows view text]: [text] is a part of {!visible} [view]. *)
