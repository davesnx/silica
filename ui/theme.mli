(** Themes: named sets of design tokens, a choice between them, and the machine
    that applies the choice to the page.

    A theme gives a value to each token of {!type-tokens}. {!stylesheet} turns a
    {!type-set} of themes into CSS: one [[data-theme="name"]] block of CSS
    custom properties ([--ui-surface], ...) and [color-scheme] per theme, a
    [[data-theme="system"]] block that follows [prefers-color-scheme], and the
    Tailwind [@theme inline] map that makes utilities such as [bg-ui-surface]
    read those properties. An app writes the stylesheet to a file in a dune rule
    and imports it in its Tailwind input.

    The theme component ({!switcher} or {!provider}) holds the {!type-choice}.
    At the start and at each change it sets [data-theme] on the [<html>] element
    ({!Silica.Action.Set_document_attribute}); a server render writes it into
    the first HTML. A change made in the page is stored in the cookie
    {!cookie_name}, which the server and the browser read before the first props
    ({!choice_of_cookie}), so the first HTML has the right theme. A switch
    changes one attribute: no other component renders again.

    {[
      let set = Theme.set ~light:Theme.light ~dark:Theme.dark Theme.shipped

      (* server page and browser entry *)
      let choice =
        Theme.choice_of_cookie set cookie
          (* in a view *)
          Theme.switcher
          ~on_change:(fun current -> Theme_changed current)
          (Theme.props ~id:"theme" set choice)
    ]} *)

type scheme =
  | Light
  | Dark  (** The [color-scheme] of a theme: form controls and scroll bars. *)

type tokens = {
  surface : string;  (** The page background. *)
  raised : string;  (** The background of popups, dialogs and cards. *)
  text : string;  (** Text on [surface], [raised] and [highlight]. *)
  muted : string;  (** Secondary text on [surface] and [raised]. *)
  border : string;
  highlight : string;  (** The background of a highlighted or hovered item. *)
  accent : string;  (** The background of a main button, a checked control. *)
  accent_text : string;  (** Text on [accent]. *)
  danger : string;  (** Error text on [surface]. *)
  backdrop : string;  (** The layer behind a modal dialog. *)
  focus : string;  (** The focus ring. *)
  radius : string;  (** The corner radius of controls. *)
  shadow : string;  (** The shadow of popups. *)
  font : string;  (** The font family of the page. *)
  spacing : string;
      (** The unit of the spacing scale: Tailwind [p-4] is 4 times it. *)
}
(** The tokens of silica-ui. Each one is a CSS value, such as ["#ffffff"] or
    ["0.5rem"]. A theme must give all of them, so a new token is a compile error
    in each theme that does not have it. Tailwind names: [ui-<token>] for
    colours ([bg-ui-surface], [text-ui-accent-text]), [rounded-ui], [shadow-ui],
    [font-ui]; [spacing] is the Tailwind [--spacing]. *)

type t
(** A theme. *)

val make :
  name:string ->
  ?label:string ->
  scheme:scheme ->
  ?extra:(string * string) list ->
  tokens ->
  t
(** [name] is the value of [data-theme] and of the cookie: lower case ASCII
    letters, digits and [-], starting with a letter, not ["system"]. [label] is
    the text in a switcher (default: [name]). [extra] are the tokens of the app,
    as pairs of a name (same rules as [name]) and a value; the CSS property is
    [--<name>]. A value is not empty and may not hold a semicolon, a brace, an
    angle bracket, a backslash, a double quote or a line break; it holds an even
    count of single quotes.

    Raises [Invalid_argument] for a bad name or value. *)

val name : t -> string
val label : t -> string
val scheme : t -> scheme
val tokens : t -> tokens

val extra : t -> (string * string) list
(** The app tokens, as given to {!make}. *)

(** {1 Shipped themes} *)

val light : t
(** ["light"]: white surfaces, indigo accent. *)

val dark : t
(** ["dark"]: gray surfaces, indigo accent. *)

val high_contrast : t
(** ["high-contrast"]: black surfaces, white text and borders, yellow accent. *)

val ocean : t
(** ["ocean"]: a brand theme with a dark scheme, navy surfaces and a cyan
    accent. *)

val shipped : t list
(** [light], [dark], [high_contrast] and [ocean]. *)

(** {1 Sets} *)

type set
(** The themes of an app, with the pair that {!System} uses. *)

val set : light:t -> dark:t -> t list -> set
(** [set ~light ~dark themes]. Under {!System} the page uses [light] when the
    system setting is light and [dark] when it is dark.

    Raises [Invalid_argument] when [themes] is empty, two themes have the same
    name, [light] or [dark] is not in [themes], [light] does not have the scheme
    [Light] or [dark] the scheme [Dark], or two themes have different [extra]
    token names. The message names the theme and the token. *)

val themes : set -> t list
(** In the order given to {!val-set}. *)

val system_light : set -> t
val system_dark : set -> t

(** {1 Choice} *)

type choice =
  | System  (** Follow the system setting ([prefers-color-scheme]), live. *)
  | Fixed of t

val choice_name : choice -> string
(** ["system"], or the name of the theme. *)

val choice_of_name : set -> string -> choice option
(** The choice with that name in the set. *)

val cookie_name : string
(** ["silica-theme"]. *)

val choice_of_cookie : set -> string option -> choice
(** The choice stored in the cookie {!cookie_name}, given its value (from
    [Silica_server.cookie] or [Silica_browser.cookie]). No cookie, or a name
    that is not in the set (a theme that the app removed), gives [System]. *)

(** {1 CSS} *)

val stylesheet : set -> string
(** The CSS of the set: the Tailwind [@theme inline] map of {!type-tokens}, one
    [[data-theme="name"]] block per theme with its [color-scheme] and a
    [--ui-<token>] or [--<extra>] property per token, and the
    [[data-theme="system"]] blocks of {!System}. The output depends only on the
    set. *)

val scope : ?key:string -> t -> 'e Silica.Node.t list -> 'e Silica.Node.t
(** A [<div data-theme="name">] around the nodes: they use the theme, whatever
    the theme of the page. silica-ui CSS gives every element with [data-theme]
    the [surface] background, the [text] colour and the [font]. A popup that a
    component in the nodes opens (a {!Silica.Node.portal} with no target) uses
    the theme too: the browser renderer gives its container in the body the same
    [data-theme]. *)

(** {1 Machine} *)

type props = { id : string; label : string; set : set; choice : choice }
(** [id] is the id of the switcher (unique in the page). [label] is its
    accessible name. [choice] is the choice at the start; when it changes, the
    component takes it as a change made in the page. *)

val props : id:string -> ?label:string -> set -> choice -> props
(** [label] defaults to ["Theme"]. *)

type current = {
  choice : choice;
  theme : t;  (** The theme in use: under [System], the light or dark one. *)
}

type event =
  | Choose of string  (** The name of a choice, from the switcher. *)
  | System_dark of bool  (** The system setting is dark. *)

type state =
  | Fixed_theme of t  (** A fixed theme. Its state name is ["fixed"]. *)
  | System_scheme of bool option
      (** The system setting decides: [Some dark] once the media source answers,
          [None] before. Its state name is ["system"]. *)

val state_codec : set -> state Silica.Codec.t
(** [["Fixed_theme", name]] or [["System_scheme", dark]], with [dark] a boolean
    or [null], for {!Silica.Machine.register_codecs} on {!val-machine}. A theme
    name that the set does not have does not decode. *)

val machine : (props, state, event) Silica.Machine.t
(** In [System_scheme] it holds [Source.media "(prefers-color-scheme: dark)"].
    Each change of choice sets [data-theme]; a change made in the page (a
    [Choose], or props with another choice) also sets the cookie for one year.
*)

val current : props -> state -> current option
(** [None] under [System_scheme] until the system setting is known (always in a
    server render). *)

val switcher_component : (props, state, event) Silica.Component.t
(** The machine with a {!Select} of ["System"] and the label of each theme. *)

val provider_component : (props, state, event) Silica.Component.t
(** The machine with an empty view. *)

val switcher :
  ?key:string -> ?on_change:(current -> 'e) -> props -> 'e Silica.Node.t
(** The theme component with its switcher. [on_change] hears the current theme
    after each [Choose] and each change of the system setting under [System],
    including the first one. Render one theme component per page. *)

val provider :
  ?key:string -> ?on_change:(current -> 'e) -> props -> 'e Silica.Node.t
(** The theme component with no view, for an app that draws its own switcher:
    the app keeps the choice and gives it in the props. *)
