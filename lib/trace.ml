module Json = Silica_json.Json

type origin =
  | Checker of { rule : string; start : int; choices : int list }
  | Runtime of { platform : string }

type header = {
  root : string;
  url : string option;
  build : string;
  origin : origin;
  dropped : int;
  loop_from : int option;
}

type t = { header : header; transitions : Transition.t list }

let string value = Json.declassify (`String value)
let int value = Json.declassify (`Int value)
let object_of fields = Json.declassify (`Assoc fields)
let optional name encode = function
  | None ->
      []
  | Some value ->
      [ (name, encode value) ]

let encode_header header =
  object_of
    ([ ("root", string header.root) ]
    @ optional "url" string header.url
    @ [
        ("build", string header.build);
        ( "origin",
          match header.origin with
          | Checker { rule; start; choices } ->
              object_of
                [
                  ("type", string "checker");
                  ("rule", string rule);
                  ("start", int start);
                  ("choices", Json.declassify (`List (List.map int choices)));
                ]
          | Runtime { platform } ->
              object_of
                [ ("type", string "runtime"); ("platform", string platform) ]
        );
        ("dropped", int header.dropped);
      ]
    @ optional "loop_from" int header.loop_from
    )

let ( let* ) = Result.bind

let fields json =
  match Json.classify json with
  | `Assoc fields ->
      Ok fields
  | _ ->
      Error "expected a JSON object"

let field fields name decode =
  match List.assoc_opt name fields with
  | Some value -> (
      match decode value with
      | Ok decoded ->
          Ok (Some decoded)
      | Error message ->
          Error ("field \"" ^ name ^ "\": " ^ message)
    )
  | None ->
      Ok None

let required fields name decode =
  let* value = field fields name decode in
  match value with
  | Some value ->
      Ok value
  | None ->
      Error ("missing field \"" ^ name ^ "\"")

let as_string json =
  match Json.classify json with
  | `String value ->
      Ok value
  | _ ->
      Error "expected a string"

let as_int json =
  match Json.classify json with
  | `Int value ->
      Ok value
  | _ ->
      Error "expected an integer"

let as_ints json =
  match Json.classify json with
  | `List items ->
      List.fold_right
        (fun item acc ->
          let* rest = acc in
          let* value = as_int item in
          Ok (value :: rest)
        )
        items (Ok [])
  | _ ->
      Error "expected a list of integers"

let decode_origin json =
  let* fields = fields json in
  let* kind = required fields "type" as_string in
  match kind with
  | "checker" ->
      let* rule = required fields "rule" as_string in
      let* start = required fields "start" as_int in
      let* choices = required fields "choices" as_ints in
      Ok (Checker { rule; start; choices })
  | "runtime" ->
      let* platform = required fields "platform" as_string in
      Ok (Runtime { platform })
  | other ->
      Error ("unknown origin \"" ^ other ^ "\"")

let decode_header json =
  let* fields = fields json in
  let* root = required fields "root" as_string in
  let* url = field fields "url" as_string in
  let* build = required fields "build" as_string in
  let* origin = required fields "origin" decode_origin in
  let* dropped = required fields "dropped" as_int in
  let* loop_from = field fields "loop_from" as_int in
  Ok { root; url; build; origin; dropped; loop_from }

let to_jsonl trace =
  String.concat ""
    (List.map
       (fun json -> Json.to_string json ^ "\n")
       (encode_header trace.header
       :: List.map Transition.codec.encode trace.transitions
       )
    )

let of_jsonl text =
  let lines =
    List.filter_map
      (fun (number, line) ->
        if String.trim line = "" then
          None
        else
          Some (number, line)
      )
      (List.mapi
         (fun index line -> (index + 1, line))
         (String.split_on_char '\n' text)
      )
  in
  let parse (number, line) decode =
    match Result.bind (Json.of_string line) decode with
    | Ok value ->
        Ok value
    | Error message ->
        Error ("line " ^ string_of_int number ^ ": " ^ message)
  in
  match lines with
  | [] ->
      Error "line 1: no header"
  | first :: rest ->
      let* header = parse first decode_header in
      let* transitions =
        List.fold_left
          (fun found line ->
            let* found = found in
            let* transition = parse line Transition.codec.decode in
            Ok (transition :: found)
          )
          (Ok []) rest
      in
      Ok { header; transitions = List.rev transitions }

let change_text : Transition.change -> string = function
  | Started ->
      "started"
  | Resumed ->
      "resumed"
  | Unhandled ->
      "unhandled"
  | Kept ->
      "kept"
  | Entered ->
      "entered"
  | Ended ->
      "ended"

let cause_text : Transition.cause -> string = function
  | Start ->
      "start"
  | Element { handler; role; name; element; value } -> (
      handler ^ " "
      ^ (match role with Some role -> role ^ " " | None -> "")
      ^ "\"" ^ name ^ "\" (" ^ element ^ ")"
      ^ match value with Some value -> " " ^ String.escaped value | None -> ""
    )
  | Timer { id; ms } ->
      "timer " ^ string_of_int ms ^ "ms " ^ id
  | Answer { path; answer; _ } -> (
      "answer " ^ path
      ^
      match answer with
      | Some answer ->
          " " ^ Json.to_string answer
      | None ->
          ""
    )
  | Live { topic; _ } ->
      "live " ^ topic
  | Held { name; _ } ->
      "held " ^ name
  | Action_result { action } ->
      "result of " ^ action
  | Props ->
      "props"
  | Child { instance } ->
      "child " ^ instance
  | Url url ->
      "url " ^ url
  | Preload url ->
      "preload " ^ url
  | Message ->
      "message"
  | Ended ->
      "end"

let origin_text = function
  | Checker { rule; _ } ->
      "checker " ^ rule
  | Runtime { platform } ->
      platform

let line ?start (record : Transition.t) =
  let time =
    match (start, record.time_ms) with
    | Some start, Some time ->
        " +" ^ string_of_int (int_of_float (time -. start)) ^ "ms"
    | _ ->
        ""
  in
  let states =
    match record.change with
    | Started | Resumed ->
        record.target
    | Unhandled | Kept | Entered | Ended ->
        record.from ^ " -> " ^ record.target
  in
  string_of_int record.seq ^ time ^ " " ^ record.instance ^ " " ^ states ^ " ("
  ^ change_text record.change ^ ")"
  ^ (match Transition.label record with "" -> "" | label -> " on " ^ label)
  ^ " <- " ^ cause_text record.cause
  ^
  match record.actions with
  | [] ->
      ""
  | actions ->
      " => " ^ String.concat "; " actions

let to_text trace =
  let header = trace.header in
  let start =
    match trace.transitions with
    | { time_ms = Some time; _ } :: _ ->
        Some time
    | _ ->
        None
  in
  let line = line ?start in
  String.concat ""
    (List.map
       (fun text -> text ^ "\n")
       (("trace " ^ header.root
        ^ (match header.url with Some url -> " " ^ url | None -> "")
        ^ " build " ^ header.build ^ " from " ^ origin_text header.origin
        ^ ( if header.dropped > 0 then
              " dropped " ^ string_of_int header.dropped
            else
              ""
          )
        ^
        match header.loop_from with
        | Some seq ->
            " loop from " ^ string_of_int seq
        | None ->
            ""
        )
       :: List.map line trace.transitions
       )
    )

let path_only url =
  let cut = String.length url in
  let cut = Option.value (String.index_opt url '?') ~default:cut |> min cut in
  let cut = Option.value (String.index_opt url '#') ~default:cut |> min cut in
  String.sub url 0 cut

let names_only trace =
  {
    header = { trace.header with url = Option.map path_only trace.header.url };
    transitions = Transition.names_only trace.transitions;
  }

module Recorder = struct
  type trace = t

  type t = {
    capacity : int;
    data : bool;
    platform : string;
    build : string;
    kept : Transition.pending Queue.t;
    mutable dropped : int;
    mutable root : string option;
    mutable observer : Instance.observer option;
  }

  let keep recorder pending =
    let (transition : Transition.t) = Transition.peek pending in
    if recorder.root = None && not (String.contains transition.instance '/')
    then
      recorder.root <- Some transition.machine;
    Queue.add pending recorder.kept;
    if Queue.length recorder.kept > recorder.capacity then (
      ignore (Queue.pop recorder.kept);
      recorder.dropped <- recorder.dropped + 1
    )

  let record recorder transition = keep recorder (Transition.ready transition)

  let create ?(capacity = 1000) ?(data = false) ?mask ?clock
      ?(platform = "browser") ~build () =
    if capacity < 1 then
      invalid_arg "Silica.Trace.Recorder.create: capacity below 1";
    let recorder =
      {
        capacity;
        data;
        platform;
        build;
        kept = Queue.create ();
        dropped = 0;
        root = None;
        observer = None;
      }
    in
    recorder.observer <- Some (Transition.observer ?clock ?mask (keep recorder));
    recorder

  let observer recorder =
    match recorder.observer with
    | Some observer ->
        observer
    | None ->
        invalid_arg "Silica.Trace.Recorder.observer"

  let transitions recorder =
    let resolved =
      List.of_seq (Seq.map Transition.resolve (Queue.to_seq recorder.kept))
    in
    if recorder.data then
      resolved
    else
      Transition.names_only resolved

  let trace recorder ~url : trace =
    {
      header =
        {
          root = Option.value recorder.root ~default:"";
          url =
            ( if recorder.data then
                url
              else
                Option.map path_only url
            );
          build = recorder.build;
          origin = Runtime { platform = recorder.platform };
          dropped = recorder.dropped;
          loop_from = None;
        };
      transitions = transitions recorder;
    }

  let clear recorder =
    Queue.clear recorder.kept;
    recorder.dropped <- 0
end
