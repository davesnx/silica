open Silica
open Silica_test_support

let observe sim =
  let text =
    String.concat "\n"
      [
        "page: " ^ Sim.page sim;
        "running: " ^ String.concat ", " (Sim.running sim);
        "sources: "
        ^ String.concat ", "
            (List.map (fun (id, name) -> id ^ " " ^ name) (Sim.sources sim));
        "log: " ^ String.concat ", " (Sim.log sim);
      ]
  in
  Sim.clear_log sim;
  text

type shared = { run : 's 'e. ('s, 'e) Sim.t -> unit }

let both name shared = (name, shared.run, shared.run)

let same_steps original port steps =
  Alcotest.(check string) "start" (observe original) (observe port);
  List.iter
    (fun (name, on_original, on_port) ->
      on_original original;
      on_port port;
      Alcotest.(check string) name (observe original) (observe port)
    )
    steps

type found = {
  states : int;
  transitions : int;
  edges : Spec.label list;
  spec : Spec.t;
}

let found pp_event = function
  | Check.Ok { states; transitions; edges; spec; _ } ->
      { states; transitions; edges; spec }
  | report ->
      Alcotest.failf "%a" (fun f r -> Check.pp_report ~pp_event f r) report

let edge_strings = List.map (fun (from, target) -> from ^ " -> " ^ target)

let label_strings =
  List.map (fun (label : Spec.label) ->
      Printf.sprintf "%s -[%s]-> %s%s" label.from label.event label.target
        ( if label.kept then
            " (kept)"
          else
            ""
        )
  )

let state_strings (spec : Spec.t) =
  List.map
    (fun (state : Spec.state) ->
      Printf.sprintf "%s final=%b timers=[%s] sources=[%s] held=[%s]" state.name
        state.final
        (String.concat "; " (List.map string_of_int state.timers))
        (String.concat "; " state.sources)
        (String.concat "; " state.held)
    )
    spec.states

let same_check ~name ~declared original port =
  let strings = Alcotest.(list string) in
  Alcotest.(check int) "states" original.states port.states;
  Alcotest.(check int) "transitions" original.transitions port.transitions;
  Alcotest.check strings "taken edges"
    (label_strings original.edges)
    (label_strings port.edges);
  Alcotest.check strings "explored states"
    (state_strings original.spec)
    (state_strings port.spec);
  Alcotest.check strings "explored edges"
    (edge_strings original.spec.edges)
    (edge_strings port.spec.edges);
  Printf.printf
    "%s: %d states, %d transitions\n\
    \  declared edges (old Machine.spec): %s\n\
    \  explored edges (Check report spec): %s\n\
    \  lint of the explored spec: [%s]\n"
    name port.states port.transitions
    (String.concat ", " (edge_strings declared.Spec.edges))
    (String.concat ", " (edge_strings port.spec.edges))
    (String.concat "; " (List.map Spec.issue_to_string (Spec.lint port.spec)))

let same_render original port =
  Alcotest.(check string)
    "server render"
    (Render.to_string original)
    (Render.to_string port)

module Landing = struct
  module Original = Landing_save
  module Port = Spike_landing_save

  let original_label : Original.event -> string = function
    | Save ->
        "save"
    | Answer (Ok _) ->
        "answer ok"
    | Answer (Error _) ->
        "answer error"
    | Reset ->
        "reset"

  let port_label : Port.event -> string = function
    | Save ->
        "save"
    | Answer (Ok _) ->
        "answer ok"
    | Answer (Error _) ->
        "answer error"
    | Reset ->
        "reset"

  let pp label f event = Format.pp_print_string f (label event)

  let port_check () =
    Check.component ~label:port_label
      ~samples:[ Check.answers Port.save [ "12:00" ] ]
      Port.component

  let check_both () =
    let original =
      Check.component ~label:original_label
        ~samples:[ Check.answers Original.save [ "12:00" ] ]
        Original.component
      |> found (pp original_label)
    in
    let port = port_check () |> found (pp port_label) in
    same_check ~name:"save"
      ~declared:(Machine.spec Original.machine)
      original port

  let same_html () =
    let answer time =
      ( "answer " ^ time,
        (fun sim -> Sim.answer sim Original.save time),
        fun sim -> Sim.answer sim Port.save time
      )
    in
    same_steps
      (Sim.mount Original.component)
      (Sim.mount Port.component)
      [
        both "press save" { run = (fun sim -> Sim.press sim "Save the draft") };
        answer "12:00";
        both "the timer fires"
          {
            run =
              (fun sim ->
                List.iter (Sim.fire sim)
                  (List.filter
                     (String.ends_with ~suffix:"save/saved/after0")
                     (Sim.running sim)
                  )
              );
          };
        both "press save again"
          { run = (fun sim -> Sim.press sim "Save the draft") };
        ( "the call fails",
          (fun sim -> Sim.answer_error sim Original.save (Remote.Http 500)),
          fun sim -> Sim.answer_error sim Port.save (Remote.Http 500)
        );
        both "press try again" { run = (fun sim -> Sim.press sim "Try again") };
        answer "12:01";
      ]

  let render () =
    same_render
      (Component.node Original.component)
      (Component.node Port.component)

  let spec_without_running () =
    let spec = Machine.spec Port.machine in
    Alcotest.(check (list string))
      "only the initial state"
      [ "idle final=false timers=[] sources=[] held=[]" ]
      (state_strings spec);
    Alcotest.(check (list string)) "no edges" [] (edge_strings spec.edges)
end

module Dialogs = struct
  module Original = Ui_samples.Note_dialog
  module Port = Spike_dialog.Make (Ui_samples.Note)

  let original_props ?open_ ?dismissible ?alert () =
    Original.Props
      (Original.props ~id:"note" ~title:"Edit note" ~trigger:"Edit"
         ~close:"Close" ?open_ ?dismissible ?alert
         { Ui_samples.Note.text = "hello" }
      )

  let port_props ?open_ ?dismissible ?alert () =
    Port.Props
      (Port.props ~id:"note" ~title:"Edit note" ~trigger:"Edit" ~close:"Close"
         ?open_ ?dismissible ?alert
         { Ui_samples.Note.text = "hello" }
      )

  let port_label : Port.event -> string = function
    | Props { open_ = None; _ } ->
        "props"
    | Props { open_ = Some true; _ } ->
        "props open"
    | Props { open_ = Some false; _ } ->
        "props closed"
    | Open_request reason ->
        "open " ^ Silica_ui.Popup.reason_name reason
    | Close_request reason ->
        "close " ^ Silica_ui.Popup.reason_name reason
    | Content (Ui_samples.Note.Edit _) ->
        "edit"
    | Content Ui_samples.Note.Save ->
        "save"

  let every_props props =
    [
      props ?open_:None ?dismissible:None ?alert:None ();
      props ?open_:(Some true) ?dismissible:None ?alert:None ();
      props ?open_:(Some false) ?dismissible:None ?alert:None ();
      props ?open_:(Some true) ?dismissible:(Some false) ?alert:None ();
      props ?open_:(Some true) ?dismissible:None ?alert:(Some true) ();
    ]

  let port_check () =
    Check.component ~label:port_label ~props:(every_props port_props)
      ~inputs:(function
        | Port.Open _ ->
            [ Port.Content (Ui_samples.Note.Edit "changed") ]
        | _ ->
            []
        )
      Port.component

  let check_both () =
    let original =
      Ui_checks.dialog ()
      |> found (fun f event ->
          Format.pp_print_string f (Ui_checks.dialog_label event)
      )
    in
    let port =
      port_check ()
      |> found (fun f event -> Format.pp_print_string f (port_label event))
    in
    same_check ~name:"note-dialog"
      ~declared:(Machine.spec Original.machine)
      original port

  let same_html () =
    let props name ?open_ ?dismissible () =
      ( name,
        (fun sim -> Sim.send sim (original_props ?open_ ?dismissible ())),
        fun sim -> Sim.send sim (port_props ?open_ ?dismissible ())
      )
    in
    let press id =
      both ("press " ^ id) { run = (fun sim -> Sim.press_id sim ~id) }
    in
    let escape = both "escape" { run = (fun sim -> ignore (Sim.escape sim)) } in
    same_steps
      (Sim.mount ~props:(original_props ()) Original.component)
      (Sim.mount ~props:(port_props ()) Port.component)
      [
        press "note-trigger";
        escape;
        press "note-trigger";
        both "outside press" { run = Sim.outside_press };
        press "note-trigger";
        both "type" { run = (fun sim -> Sim.input sim ~label:"Note" "changed") };
        both "submit" { run = Sim.submit };
        props "props open" ~open_:true ();
        props "props closed" ~open_:false ();
        press "note-trigger";
        press "note-close";
        props "props open, not dismissible" ~open_:true ~dismissible:false ();
        escape;
        props "props closed again" ~open_:false ();
      ]

  let render () =
    same_render
      (Component.node ~props:(original_props ~open_:true ()) Original.component)
      (Component.node ~props:(port_props ~open_:true ()) Port.component)
end

module Issue_list = struct
  module Original = Linear_list
  module Port = Spike_linear_list
  open Linear_model

  let filtered =
    {
      Linear_view.default with
      filter = { Linear_view.Filter.none with statuses = [ Issue_status.Done ] };
    }

  let port_check () =
    let open Port in
    let invariants =
      [
        {
          Check.name = "never shows loading and rows";
          holds =
            (fun _ view ->
              not
                (Linear_checks.shows view "Loading issues"
                && Linear_checks.count_rows view > 0
                )
            );
        };
        {
          Check.name = "a ready list shows one row for each issue";
          holds =
            (fun state view ->
              match state with
              | Ready data ->
                  Linear_checks.count_rows view
                  = List.length
                      (List.concat_map
                         (fun (group, members) ->
                           if List.mem group data.collapsed then
                             []
                           else
                             members
                         )
                         (Linear_view.groups data.view
                            (Linear_data.issues data.data)
                         )
                      )
              | Loading _ | Failed _ ->
                  true
            );
        };
        {
          Check.name = "the offline note shows exactly when the data is stale";
          holds =
            (fun state view ->
              match state with
              | Ready data ->
                  Linear_checks.shows view "Offline" = data.data.stale
              | Loading _ | Failed _ ->
                  not (Linear_checks.shows view "Offline")
            );
        };
      ]
    in
    Check.component ~max_states:50_000
      ~samples:[ Linear_checks.issues; Linear_checks.topic ]
      ~props:[ Props Linear_view.default; Props filtered ]
      ~within:(function
        | Ready data ->
            data.data.next_id <= 2 && List.length data.toasts <= 1
        | Loading _ | Failed _ ->
            true
        )
      ~inputs:(function
        | Ready data ->
            [
              Filter_open false;
              Filter_changed filtered.filter;
              Grouping_picked "assignee";
              Ordering_picked "priority";
              Menu_open false;
              Command "create";
              Command "open-1";
              Command "status-done";
            ]
            @ List.concat_map
                (fun (id, (request : Linear_data.request)) ->
                  [
                    Saved (id, Error (Remote.Network "down"));
                    Saved
                      ( id,
                        Ok
                          (Change.Conflict
                             {
                               (Linear_checks.issue request.number) with
                               version = request.base + 1;
                             }
                          )
                      );
                  ]
                )
                (Linear_data.requests data.data)
            @ List.map
                (fun (toast : Silica_ui.Toast.toast) -> Toast_closed toast.key)
                data.toasts
        | Loading _ | Failed _ ->
            []
        )
      ~label:Linear_checks.list_label ~invariants Port.component

  let check_both () =
    let pp = Linear_checks.pp_list_event in
    same_check ~name:"issue-list"
      ~declared:(Machine.spec Original.machine)
      (found pp (Linear_checks.list ()))
      (found pp (port_check ()))

  let issues = Linear_checks.three

  let ready seq data : Issues.t Topic.update =
    { seq; status = Status.Ready { data; stale = false } }

  let offline data : Issues.t Topic.update =
    { seq = 0; status = Status.Ready { data; stale = true } }

  let same_html () =
    let publish name update =
      both name
        { run = (fun sim -> Sim.publish sim issues_topic ~key:"all" update) }
    in
    let key name =
      both ("key " ^ name)
        { run = (fun sim -> ignore (Sim.press_key sim (Key.make name))) }
    in
    let send name event =
      (name, (fun sim -> Sim.send sim event), fun sim -> Sim.send sim event)
    in
    let answer =
      both "answer" { run = (fun sim -> Sim.answer sim issues_endpoint issues) }
    in
    same_steps
      (Sim.mount Original.component)
      (Sim.mount Port.component)
      [
        answer;
        publish "publish" (ready 1 issues);
        key "j";
        key "j";
        key "k";
        key "f";
        send "group" (Port.Toggle_group Linear_view.All);
        publish "offline" (offline issues);
        both "press reconnect" { run = (fun sim -> Sim.press sim "Reconnect") };
        send "props filtered" (Port.Props filtered);
        send "menu open" (Port.Menu_open true);
        send "command" (Port.Command "create");
      ];
    same_steps
      (Sim.mount Original.component)
      (Sim.mount Port.component)
      [
        both "the call fails"
          {
            run =
              (fun sim -> Sim.answer_error sim issues_endpoint (Remote.Http 500));
          };
        send "props filtered" (Port.Props filtered);
        both "press try again" { run = (fun sim -> Sim.press sim "Try again") };
        answer;
      ]

  let render () =
    same_render
      (Component.node Original.component)
      (Component.node Port.component)
end
