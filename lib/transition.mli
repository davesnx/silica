(** The transition log: one record for each start, step and end of each
    instance.

    A host gives an {!Instance.observer} to {!Instance.mount} (in the [create]
    function of {!Runtime.start}), to the Live runner of the server, or to
    [Silica_browser.mount ~observe]. {!observer} makes one: it turns each step
    into a {!t} with the names of the states and of the event, the data of the
    event, the props and the snapshot by the codecs of the machine (see
    {!Machine.register_codecs}), the actions as text and the cause.
    [Trace.Recorder] keeps the records and writes them as a trace file.

    A record holds names always, and data only when the machine has registered
    the codec for it. A release bundle that registers no codec gives records
    with names only. *)

(** {1 Records} *)

type change = Instance.change =
  | Started  (** The instance started: [from] is [""]. *)
  | Resumed
      (** The instance started again at a snapshot it reached before, such as a
          Live island after a server restart: [from] is [""], [target] is the
          state of that snapshot, [actions] are only the starts of what runs
          during it (see {!Machine.resume_with_props}), and the cause is
          [Start]. *)
  | Unhandled  (** The machine ignored the event: nothing changed. *)
  | Kept  (** The step kept the state name. *)
  | Entered  (** The step entered a state, with the same name or another. *)
  | Ended  (** The instance ended: [target] is the state it ended in. *)

val change_to_string : change -> string
(** ["started"], ["resumed"], ["unhandled"], ["kept"], ["entered"] or ["ended"],
    as {!codec} writes it. *)

val change_of_string : string -> (change, string) result
(** Reads {!change_to_string}. *)

type cause =
  | Start  (** The start of an instance. *)
  | Element of {
      handler : string;
          (** ["press"], ["double_press"], ["input"], ["submit"], ["focus"],
              ["blur"], ["key_down"], ["key_up"], ["pointer_enter"] or
              ["pointer_leave"]. *)
      role : string option;
          (** The role of the element, by {!Accessibility}. *)
      name : string;  (** The accessible name of the element. *)
      element : string;
          (** Where the element is, as {!Accessibility.path} gives it. *)
      value : string option;
          (** The text of an [input] ({!Secret.mask} for a password field), or
              the name of the key of a [key_down] or [key_up] (see
              {!Key.to_string}; {!Secret.mask} for a key that types a character
              into a password field). *)
    }  (** A handler of an element of the view. *)
  | Timer of { id : string; ms : int }  (** A timer that the state owned. *)
  | Answer of { id : string; path : string; answer : Silica_json.Json.t option }
      (** The answer of a call to the endpoint at [path], by
          {!Endpoint.answer_codec}. [None] when data is not recorded. *)
  | Live of { id : string; topic : string; update : Silica_json.Json.t option }
      (** A state or a change of connection of a Live subscription, by
          {!Topic.update_codec}. [None] when data is not recorded. *)
  | Held of { id : string; name : string }
      (** A held resource, by its {!Action.held_name}. *)
  | Action_result of { action : string }
      (** The result of an action, such as {!Action.Measure}. *)
  | Props  (** The parent gave new props. *)
  | Child of { instance : string }
      (** The [output] or the [on_final] of the child at this instance path. *)
  | Url of string  (** The URL changed: the root got [Router.Url_changed]. *)
  | Preload of string  (** The root got [Router.Preload] for this URL. *)
  | Message
      (** The host gave the root an event (see {!Instance.val-message}). *)
  | Ended  (** The instance ended. *)

type t = {
  seq : int;  (** From 1, one higher for each record of a hook. *)
  time_ms : float option;
      (** The clock of the observer when the record was made, in milliseconds.
      *)
  instance : string;
      (** The instance path: the root is its machine id; a child adds
          ["/<slot>:<machine id>"] to the path of its parent, where the slot is
          the positions of the component node in the view of the parent, joined
          by ["."], and a key is [#key]. [%], [/], [:], [#] and [.] in a key are
          percent encoded. Example: [app/0.2:router/#inbox:issue-list]. *)
  number : int;
      (** The number of the instance in its tree, as in resource ids. *)
  entry : int;
      (** The number of the entry of the state after the step, as in timer and
          resource ids. *)
  machine : string;  (** {!Machine.id}. *)
  from : string;
      (** The state name before; [""] for [Started] and [Resumed]. *)
  target : string;  (** The state name after. *)
  change : change;
  event : string;
      (** {!Machine.event_name} of the event; [""] when there is no event
          ([Started], [Resumed], [Ended], [Props]) or the machine gives no
          [event_name]. {!label} names the event in both cases. *)
  data : Silica_json.Json.t option;
      (** The event, by the event codec of the machine. *)
  props : Silica_json.Json.t option;
      (** The props, by the props codec of the machine, for [Started], [Resumed]
          and for a [Props] cause. *)
  snapshot : Silica_json.Json.t option;
      (** The snapshot after, by the state codec of the machine. *)
  actions : string list;
      (** The actions given to the host, in order, as {!describe} writes them.
      *)
  cause : cause;
}

val codec : t Codec.t
(** One JSON object. Optional fields that are [None] and empty [actions] are
    left out. *)

val describe : 'm Action.host -> string
(** One action as short text, such as
    ["start timer 1.2/save/saved/after 2000ms 2000ms"],
    ["start call 1.1/save/saving//api/save null /api/save"] (the id, then the
    path), ["stop 1.1/save/saving//api/save null"], ["focus title"] or
    ["navigate /issues"]. The text of a [Copy] and the value of a cookie are
    left out. *)

val cause_label : cause -> string
(** The label of an event that its machine does not name: the handler, the key
    of a key handler, the role and the quoted accessible name of the element, as
    in [press button "Save"] or [key_down Escape textbox "Search"];
    [after 2000ms] for a timer; [ok /api/save], [rejected /api/save] or
    [failed /api/save] for the answer of a call ([answer /api/save] when the
    answer is not recorded); [live <topic>]; the name of a held resource, such
    as [dismiss]; [result <action>]; [props]; [child <instance>]; [url <url>];
    [preload <url>]; [message]. The checker labels the events of its steps and
    specs the same way. *)

val label : t -> string
(** The label of the event of the record: its [event], or {!cause_label} of its
    cause when the machine does not name it; [""] for [Started], [Resumed] and
    [Ended]. *)

val strip : t -> t
(** The record with no user data: [data], [props], [snapshot], the [answer] of
    an [Answer], the [update] of a [Live] and the [value] of an [input] are
    [None]. *)

val names_only : t list -> t list
(** The records of one trace with names only, as a recorder without data keeps
    them: each record is {!strip}ped, and also:
    - Each id of a timer, call, subscription or held resource, in causes and in
      actions, becomes ["<number>.<entry>/<machine id>/#<n>"], where [n] counts
      the distinct ids of the list from 1. The same id gets the same [n], so a
      start, its answer and its stop still match, but the id no longer holds the
      state, the path, the encoded input of a call or the key of a subscription
      (see {!Host.key}).
    - The accessible [name] of the element of a handler is [""]: it can be text
      that a user typed, such as the title of an issue in a link.
    - A URL keeps its path only, without query and fragment: in [Url] and
      [Preload] causes and in [navigate] and [replace] actions.
    - [set_title] actions lose their title, [start live] actions their key, and
      [set_document_attribute] actions their value.

    What stays: the sequence numbers, times, instance paths (with the keys that
    the app gave component nodes), machine ids, state and event names, changes,
    handler names, roles and element paths, timer delays, endpoint paths, topic
    and resource names, and the other actions as {!describe} writes them. *)

(** {1 Observers} *)

val learn : Secret.t -> Instance.observed -> unit
(** [learn secrets observed]: when the step types a text into a password field,
    [secrets] learns it, as {!observer} does. A host that encodes snapshots
    itself, such as the inspector, scrubs them with the same table (see
    {!Secret.scrub}). *)

type pending
(** A record whose element may not be known yet. The role and name of the
    element of a handler are computed from the view, which costs a walk of it,
    so they wait until the record is read. At the input the observer only finds
    the element and its path; the record keeps that element and the tag and
    attributes of the elements above it, which are immutable data, so the answer
    is the same later. When the name depends on other parts of the page
    ([aria-labelledby] in the element, or a field with an [id] or inside a
    [label]), the record keeps the whole view. *)

val resolve : pending -> t
(** The record, with its element. The first call does the work. *)

val peek : pending -> t
(** The record as it is now, with no work: the element of a handler may still
    have no role, an empty name and an empty path. *)

val ready : t -> pending
(** A record that is known. *)

val observer :
  ?clock:(unit -> float) ->
  ?mask:bool ->
  ?secrets:Secret.t ->
  (pending -> unit) ->
  Instance.observer
(** [observer ?clock ?secrets record] numbers the steps from 1, reads the
    [clock] (no time when it is not given) and gives each record to [record].

    Text typed into a password field (see {!Secret}) is masked: the [value] of
    the [input] cause is {!Secret.mask} (an empty text stays [""]), and the
    observer learns the text into [secrets] (a new table when it is not given),
    under the instance path and the element path of the field. In [data],
    [props] and [snapshot], each string that is the last text typed into a
    password field is {!Secret.mask}. A key that types a character into a
    password field (see {!Secret.is_typed_key}) is {!Secret.mask} in the
    [value], and the record has no [data]. A trace then cannot type that text or
    that key again: a replay of it stops at that step.

    With [~mask:false] (default [true]) the records keep that text in clear, for
    a test that replays its own run in the same process (see
    [Silica_memory.trace_in_clear]). Never write such records to a file. The
    work happens only in the observer: a host with no observer does none, and an
    app that never calls this function carries none of its code. *)
