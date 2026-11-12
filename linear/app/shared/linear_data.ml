open Silica
open Jsonkit.Primitives
open Linear_model

type request = { number : int; base : int; change : Change.t } [@@deriving json]

let replace (issue : Issue.t) issues =
  List.map
    (fun (i : Issue.t) ->
      if i.number = issue.number && issue.version > i.version then
        issue
      else
        i
    )
    issues

module Edits = struct
  include Optimistic.Make (struct
    type value = Issue.t list
    type change = request

    let apply issues (r : request) =
      List.map
        (fun (i : Issue.t) ->
          if i.number = r.number then
            { (Change.apply i r.change) with version = i.version + 1 }
          else
            i
        )
        issues
  end)
end

module Toast = struct
  type priority = Silica_ui.Toast.priority = Low | High [@@deriving json]

  type t = Silica_ui.Toast.toast = {
    key : string;
    title : string;
    description : string option;
    priority : priority;
  }
  [@@deriving json]
end

type t = {
  edits : (Issues.t, request) Silica_jsonkit.optimistic;
  stale : bool;
  requests : (int * request) list;
  next_id : int;
}
[@@deriving json]

type outcome = Saved | Conflict of Issue.t | Failed of string

let loaded issues =
  { edits = Edits.make issues; stale = false; requests = []; next_id = 1 }

let issues t = Edits.value t.edits
let pending t = Edits.pending t.edits
let requests t = t.requests

let find number t =
  List.find_opt (fun (i : Issue.t) -> i.number = number) (issues t)

let start ?base number change t =
  match find number t with
  | None ->
      t
  | Some issue ->
      let id = t.next_id in
      let request =
        { number; base = Option.value base ~default:issue.version; change }
      in
      {
        t with
        edits = Edits.apply ~id request t.edits;
        requests = t.requests @ [ (id, request) ];
        next_id = id + 1;
      }

let apply (update : Issues.t Topic.update) t =
  match update.status with
  | Status.Ready { stale; _ } when update.seq = 0 ->
      { t with stale }
  | Status.Ready { data; stale } ->
      let landed (id, (r : request)) edits =
        match List.find_opt (fun (i : Issue.t) -> i.number = r.number) data with
        | Some issue when issue.version > r.base ->
            Edits.settle ~confirmed:data ~id edits
        | Some _ | None ->
            edits
      in
      {
        t with
        edits = List.fold_right landed t.requests (Edits.rebase data t.edits);
        stale;
      }
  | Status.Idle | Status.Loading _ ->
      t
  | Status.Failed _ ->
      { t with stale = true }

let finish id (answer : (Change.answer, string) Remote.answer) t =
  match List.assoc_opt id t.requests with
  | None ->
      (t, None)
  | Some request -> (
      let t = { t with requests = List.remove_assoc id t.requests } in
      let confirmed = Edits.confirmed t.edits in
      match answer with
      | Ok (Change.Saved issue) ->
          ( {
              t with
              edits =
                Edits.settle ~confirmed:(replace issue confirmed) ~id t.edits;
            },
            Some (request, Saved)
          )
      | Ok (Change.Conflict issue) ->
          ( {
              t with
              edits =
                Edits.drop ~id (Edits.rebase (replace issue confirmed) t.edits);
            },
            Some (request, Conflict issue)
          )
      | Rejected message ->
          ( { t with edits = Edits.drop ~id t.edits },
            Some (request, Failed message)
          )
      | Failed error ->
          ( { t with edits = Edits.drop ~id t.edits },
            Some (request, Failed (Remote.error_to_string error))
          )
    )

let toast id (request : request) outcome =
  let key = "toast-" ^ string_of_int id in
  match outcome with
  | Saved ->
      None
  | Conflict (current : Issue.t) ->
      Some
        (Silica_ui.Toast.toast ~key ~priority:High
           ~description:
             (Issue.identifier current
            ^ " changed before it was saved. The page shows the newest value."
             )
           ("Your " ^ Change.field request.change ^ " change lost")
        )
  | Failed message ->
      Some
        (Silica_ui.Toast.toast ~key ~priority:High
           ~description:(message ^ ". The old value is back.")
           ("Could not save the "
           ^ Change.field request.change
           ^ " of ENG-"
           ^ string_of_int request.number
           )
        )

let calls t on_answer =
  List.map
    (fun (id, (r : request)) ->
      Remote.call update_endpoint
        { Change.number = r.number; base = r.base; change = r.change }
        (fun answer -> on_answer id answer
      )
    )
    t.requests

let date_of_epoch seconds =
  let days = seconds / 86400 in
  let z = days + 719468 in
  let era = z / 146097 in
  let day_of_era = z - (era * 146097) in
  let year_of_era =
    (day_of_era - (day_of_era / 1460) + (day_of_era / 36524)
   - (day_of_era / 146096)
    )
    / 365
  in
  let day_of_year =
    day_of_era - ((365 * year_of_era) + (year_of_era / 4) - (year_of_era / 100))
  in
  let shifted_month = ((5 * day_of_year) + 2) / 153 in
  let day = day_of_year - (((153 * shifted_month) + 2) / 5) + 1 in
  let month =
    if shifted_month < 10 then
      shifted_month + 3
    else
      shifted_month - 9
  in
  let year =
    year_of_era + (era * 400)
    +
    if month <= 2 then
      1
    else
      0
  in
  Printf.sprintf "%04d-%02d-%02d" year month day
