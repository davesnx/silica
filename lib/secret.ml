module Json = Silica_json.Json

let mask = "••••••••"
let hide = function "" -> "" | _ -> mask

let is_password ~tag ~kind =
  tag = "input" && Option.map String.lowercase_ascii kind = Some "password"

let is_typed_key text =
  let key = (Key.of_string text).key in
  key <> ""
  && not
       (String.length key > 1
       && String.for_all
            (fun character ->
              ('a' <= character && character <= 'z')
              || ('A' <= character && character <= 'Z')
              || ('0' <= character && character <= '9')
            )
            key
       )

type t = (string, string) Hashtbl.t

let create () = Hashtbl.create 4
let learn secrets ~field text = Hashtbl.replace secrets field text

let rec scrub secrets json =
  if Hashtbl.length secrets = 0 then
    json
  else
    match Json.classify json with
    | `String text
      when text <> ""
           && Hashtbl.fold
                (fun _ secret found -> found || secret = text)
                secrets false ->
        Json.declassify (`String mask)
    | `List items ->
        Json.declassify (`List (List.map (scrub secrets) items))
    | `Assoc fields ->
        Json.declassify
          (`Assoc
             (List.map (fun (name, value) -> (name, scrub secrets value)) fields)
          )
    | `Null | `Bool _ | `Int _ | `Float _ | `String _ ->
        json
