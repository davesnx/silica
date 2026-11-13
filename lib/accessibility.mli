(** The role, the accessible name and the states of the elements of a view,
    computed from the view as data. The checker, {!View_text} and the queries of
    [Silica_memory] use this one definition, so "the button named Save" means
    the same element in each tool.

    The computation follows WAI-ARIA 1.2, HTML-AAM and the Accessible Name and
    Description Computation 1.2, for the subset of HTML that Silica views use:

    {b Role.} An explicit [role] attribute wins: its first token that is a
    {!type-role} is used, and [none] or [presentation] gives no role. Otherwise
    the tag gives the role: [button]; [a] and [area] with [href] ([Link]); [h1]
    to [h6] ([Heading]); [ul], [ol], [menu] ([List]); [li] ([Listitem]); [input]
    by its [type] ([button], [submit], [reset], [image]: [Button]; [checkbox];
    [radio]; [range]: [Slider]; [number]: [Spinbutton]; [search]: [Searchbox];
    [text], [email], [tel], [url] or no type: [Textbox]; [Combobox] for a text
    or search field with [list]; other types: no role); [textarea] ([Textbox]);
    [select] ([Combobox], or [Listbox] with [multiple] or [size] above 1);
    [option]; [optgroup], [fieldset], [details] ([Group]); [img] ([Img], no role
    with [alt=""]); [nav], [main], [aside], [form], [search], [dialog]; [header]
    and [footer] ([Banner], [Contentinfo]) when no [article], [aside], [main],
    [nav] or [section] is above them; [section] ([Region]) when it has a name;
    [table], [tr], [td] ([Cell]), [th] ([Columnheader], or [Rowheader] with
    [scope="row"]); [hr] ([Separator]); [progress] ([Progressbar]); [output]
    ([Status]). Elements that ARIA maps to [generic] ([div], [span], [a] with no
    [href], and the rest) have no role here; [role="generic"] gives [Generic].

    {b Name.} The first of these that is not empty, with white space collapsed
    and trimmed:
    + [aria-labelledby]: the names of the elements with those ids, joined by a
      space. A referenced element counts even when it is hidden, and its own
      [aria-labelledby] is not followed.
    + [aria-label].
    + For a field ([input] that is not a button, [textarea], [select]): the text
      of each [label] whose [for] is its id, then of the [label] around it. For
      an [input] button: [value], or ["Submit"] and ["Reset"] for those types.
      For [img], [area] and an [input] image: [alt]. For [fieldset]: its
      [legend] child. For [table]: its [caption] child.
    + The content, for the roles that take their name from it (see
      {!name_from_content}), and for every element reached while the content of
      another element is read: the text and the names of the children, in order,
      with no space added between them. Hidden children give nothing. A field
      inside the content gives nothing (ARIA would use its value).
    + [title].
    + [placeholder], for [input] and [textarea].

    {b Hidden.} An element is hidden when it or an element above it has
    [hidden], [inert] or [aria-hidden="true"], or when it is an [input] of type
    [hidden]. CSS ([display: none]) and closed [details] are not seen.

    {b Tree.} Component nodes have no elements (a view of a parent shows its
    children as {!constructor-Component}); a chunk shows its value, or
    {!Loading}; a boundary shows its body. The children of a portal are elements
    with nothing above them, as in the page, where they are painted at the end
    of the body. *)

type role =
  | Alert
  | Alertdialog
  | Banner
  | Button
  | Cell
  | Checkbox
  | Columnheader
  | Combobox
  | Complementary
  | Contentinfo
  | Dialog
  | Form
  | Generic
  | Grid
  | Gridcell
  | Group
  | Heading
  | Img
  | Link
  | List
  | Listbox
  | Listitem
  | Main
  | Menu
  | Menubar
  | Menuitem
  | Menuitemcheckbox
  | Menuitemradio
  | Navigation
  | Option
  | Progressbar
  | Radio
  | Radiogroup
  | Region
  | Row
  | Rowheader
  | Search
  | Searchbox
  | Separator
  | Slider
  | Spinbutton
  | Status
  | Switch
  | Tab
  | Table
  | Tablist
  | Tabpanel
  | Textbox
  | Toolbar
  | Tooltip
  | Tree
  | Treeitem

val role_of_string : string -> role option
(** The role with this ARIA name, such as ["button"]. *)

val role_to_string : role -> string
(** The ARIA name of the role, such as ["button"]. *)

val name_from_content : role -> bool
(** The role takes its name from its content when nothing else names it, such as
    [Button], [Link], [Heading], [Option] and [Tab]. *)

type 'e element
(** An element of a view, with the elements above it and the document it is in.
*)

type 'e item =
  | Element of 'e element
  | Text of string  (** A text node that is not empty. *)
  | Component of string  (** A component node, with the id of its machine. *)
  | Loading  (** A chunk whose value is not here yet. *)

val items : 'e Node.t -> 'e item list
(** The top of the view, in view order. The children of a portal are in place of
    the portal. *)

val children : 'e element -> 'e item list
(** The children of the element, in view order, as {!items} gives them. *)

val elements : 'e Node.t -> 'e element list
(** Every element of the view in document order: first the elements outside
    portals, then the elements of each portal. A portal inside a portal comes
    before the portal around it. *)

val tag : 'e element -> string
(** The tag name. *)

val ancestors : 'e element -> 'e element list
(** The elements above it in the page, nearest first. A child of a portal has
    none. *)

val attributes : 'e element -> 'e Node.attr list
(** The attributes, as the view gives them. *)

val attribute : 'e element -> string -> string option
(** The value of an attribute; [Some ""] for a [true] boolean attribute. *)

val role : 'e element -> role option
(** The explicit [role], else the role of the tag. *)

val name : 'e element -> string
(** The accessible name. *)

val hidden : 'e element -> bool
(** The element is not in the accessibility tree: it or an element above it has
    [hidden], [inert] or [aria-hidden="true"], or it is an [<input>] of type
    [hidden]. As in a browser, an [inert] element is drawn but a screen reader
    does not reach it, so its text is not in the text view, in
    {!View_text.visible} or in [Check.view_shows]. *)

val path : 'e element -> string
(** Where the element is: the tags from the top down, joined by [" > "], each
    with its position among the children of the same tag when there are several,
    such as ["main > form > button[2]"]. The children of a portal start with
    ["portal > "]. *)

(** {1 States} *)

val level : 'e element -> int option
(** [aria-level], else the level of an [h1] to [h6]. *)

val checked : 'e element -> bool option
(** [aria-checked] ["true"] or ["false"], else [checked] on an [input] checkbox
    or radio. [None] when the element cannot be checked, and for
    [aria-checked="mixed"]. *)

val selected : 'e element -> bool option
(** [aria-selected] ["true"] or ["false"], else [selected] on an [option]. *)

val expanded : 'e element -> bool option
(** [aria-expanded] ["true"] or ["false"]. *)

val disabled : 'e element -> bool
(** [aria-disabled="true"], or [disabled] on a [button], [input], [select],
    [textarea], [option], [optgroup] or [fieldset]. *)

val current : 'e element -> string option
(** [aria-current], when it is not ["false"]. *)

val value : 'e element -> string option
(** [aria-valuetext], else [aria-valuenow], else the [value] of an [input],
    [textarea] or [select]. The value of a password field is {!Secret.mask} (an
    empty value stays [""]), so a typed secret does not show in a printed view.
*)

val modal : 'e element -> bool
(** [aria-modal="true"]. *)

val behind_modal : 'e element -> 'e element option
(** The open modal that the element is behind, if any. An open modal is an
    element with [aria-modal="true"] that is not {!hidden} and has no other open
    modal inside it (a modal opened from inside another one is in front of it,
    also through a portal). While the view shows an open modal, an element is
    behind it unless it is inside an open modal (also through a portal) or above
    one in the page; the elements around a portal are not above the portal, as
    its children are painted at the end of the body. This is the page that the
    browser renderer makes [inert] while a {!Host.focus_trap} runs (the
    [dialog-focus] rule of the checker asks for that trap on each modal): the
    checker and [Silica_memory] give no input to an element behind a modal. It
    stays in the text view, as it stays drawn. *)
