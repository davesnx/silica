open Cmdliner
open Silica

let default_socket =
  Filename.concat "_build" (Filename.concat "silica-dev" "inspect.sock")

type session = {
  input : In_channel.t;
  output : Out_channel.t;
  mutable next : int;
  targets : Inspect.target list;
}

exception Stop of int

let fail code message =
  prerr_endline ("silica inspect: " ^ message);
  raise (Stop code)

let send session message =
  Out_channel.output_string session.output (Inspect.to_string message ^ "\n");
  Out_channel.flush session.output

let rec receive input =
  match In_channel.input_line input with
  | None ->
      None
  | Some line when String.trim line = "" ->
      receive input
  | Some line -> (
      match Inspect.of_string line with
      | Ok message ->
          Some message
      | Error message ->
          fail 1 ("the hub sent a message that does not read: " ^ message)
    )

let connect socket_path =
  if not (Sys.file_exists socket_path) then
    fail 3
      (socket_path
     ^ " does not exist: no silica dev runs here. Start silica dev in this \
        directory, or give --socket."
      );
  let socket =
    try
      let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Unix.connect socket (Unix.ADDR_UNIX socket_path);
      socket
    with Unix.Unix_error (error, _, _) ->
      fail 3
        (socket_path ^ ": " ^ Unix.error_message error
       ^ ". Is silica dev still running?"
        )
  in
  let input = Unix.in_channel_of_descr socket
  and output = Unix.out_channel_of_descr socket in
  Out_channel.output_string output
    (Inspect.to_string (Hello_client "silica-inspect") ^ "\n");
  Out_channel.flush output;
  match receive input with
  | Some (Target_list { targets; _ }) ->
      { input; output; next = 0; targets }
  | Some (Failure { message; _ }) ->
      fail 1 message
  | Some _ | None ->
      fail 1 "the hub did not answer the hello"

let request session request =
  session.next <- session.next + 1;
  send session (Request { id = Some session.next; request });
  session.next

let answer session id =
  match receive session.input with
  | None ->
      fail 1 "the hub closed the connection"
  | Some (Failure { id = Some got; message }) when got = id ->
      fail 1 message
  | Some message ->
      message

let run body =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  try
    body ();
    0
  with Stop code -> code

let print_json message = print_endline (Inspect.to_string message)

let kind_name : Inspect.kind -> string = function
  | Browser ->
      "browser"
  | Server ->
      "server"

let targets socket json =
  run (fun () ->
      let session = connect socket in
      if json then
        print_json (Target_list { id = None; targets = session.targets })
      else if session.targets = [] then
        print_endline "no target is connected; open the page of the app"
      else
        List.iter
          (fun (target : Inspect.target) ->
            print_endline
              (String.concat " "
                 ([ target.target; kind_name target.kind ]
                 @ Option.to_list target.url
                 )
              )
          )
          session.targets
  )

let trees session target =
  let id = request session (Get_tree target) in
  let barrier = request session Targets in
  let rec collect found =
    match receive session.input with
    | None ->
        fail 1 "the hub closed the connection"
    | Some (Event { id = Some got; target; event = Tree instances })
      when got = id ->
        collect ((target, instances) :: found)
    | Some (Failure { id = Some got; message }) when got = id ->
        fail 1 message
    | Some (Target_list { id = Some got; _ }) when got = barrier ->
        List.rev found
    | Some _ ->
        collect found
  in
  collect []

let resource_text now ({ resource; since } : Inspect.resource) =
  let age =
    match since with
    | Some since ->
        Printf.sprintf " for %.1fs" ((now -. since) /. 1000.)
    | None ->
        ""
  in
  match resource with
  | Machine.Timer { id; ms } ->
      Printf.sprintf "timer %s %dms%s" id ms age
  | Call { id; path } ->
      Printf.sprintf "call %s %s%s" id path age
  | Subscription { id; topic } ->
      Printf.sprintf "subscription %s %s%s" id topic age
  | Held { id; name } ->
      Printf.sprintf "held %s %s%s" id name age

let filter_of machines path events changes : Inspect.filter =
  { machines; path; events; changes }

let tree socket target machines path json =
  run (fun () ->
      let session = connect socket in
      let filter = filter_of machines path [] [] in
      let now = Unix.gettimeofday () *. 1000. in
      List.iter
        (fun (target, instances) ->
          let instances =
            List.filter
              (fun instance -> Inspect.passes filter (Mount instance))
              instances
          in
          if json then
            print_json (Event { id = None; target; event = Tree instances })
          else (
            print_endline ("[" ^ target ^ "]");
            List.iter
              (fun (instance : Inspect.instance) ->
                print_endline
                  ("  " ^ instance.path ^ " " ^ instance.machine ^ " "
                 ^ instance.state
                  );
                List.iter
                  (fun resource ->
                    print_endline ("    owns " ^ resource_text now resource)
                  )
                  instance.owns;
                Option.iter
                  (fun data ->
                    print_endline ("    data " ^ Silica_json.Json.to_string data)
                  )
                  instance.data;
                Option.iter
                  (fun props ->
                    print_endline
                      ("    props " ^ Silica_json.Json.to_string props)
                  )
                  instance.props
              )
              instances
          )
        )
        (trees session target)
  )

let tail socket target machines path events changes json count =
  run (fun () ->
      let session = connect socket in
      session.next <- session.next + 1;
      let id = session.next in
      send session
        (Subscribe
           {
             id = Some id;
             target;
             filter = filter_of machines path events changes;
           }
        );
      let rec wait () =
        match answer session id with
        | Inspect.Subscribed { id = Some got } when got = id ->
            ()
        | _ ->
            wait ()
      in
      wait ();
      let printed = ref 0 in
      let rec loop () =
        if Option.fold count ~none:true ~some:(fun count -> !printed < count)
        then
          match receive session.input with
          | None ->
              ()
          | Some message -> (
              match Inspect.to_text message with
              | Some text ->
                  if json then
                    print_json message
                  else
                    print_endline text;
                  incr printed;
                  loop ()
              | None ->
                  loop ()
            )
      in
      loop ()
  )

let choose session target =
  match target with
  | Some target ->
      target
  | None -> (
      let browsers =
        List.filter
          (fun (target : Inspect.target) -> target.kind = Browser)
          session.targets
      in
      match (session.targets, browsers) with
      | [ only ], _ | _, [ only ] ->
          only.target
      | [], _ ->
          fail 1 "no target is connected; open the page of the app"
      | all, _ ->
          fail 1
            ("several targets are connected; give --target with one of: "
            ^ String.concat ", "
                (List.map (fun (target : Inspect.target) -> target.target) all)
            )
    )

let exported session target =
  let id = request session (Export (Some target)) in
  let rec find () =
    match answer session id with
    | Inspect.Trace_file { id = Some got; trace; _ } when got = id ->
        trace
    | _ ->
        find ()
  in
  find ()

let export socket target output =
  run (fun () ->
      let session = connect socket in
      let trace = exported session (choose session target) in
      match output with
      | Some file ->
          Out_channel.with_open_bin file (fun channel ->
              Out_channel.output_string channel trace
          )
      | None ->
          print_string trace
  )

let replay file machine json =
  match In_channel.with_open_bin file In_channel.input_all with
  | exception Sys_error message ->
      prerr_endline ("silica inspect replay: " ^ message);
      3
  | text -> (
      match (machine, Trace.of_jsonl text) with
      | _, Error message ->
          prerr_endline ("silica inspect replay: " ^ file ^ ": " ^ message);
          3
      | Some id, Ok _ | None, Ok { header = { root = id; _ }; _ } ->
          Sys.command
            (Filename.quote_command Sys.executable_name
               ([ "replay"; id; file ]
               @
               if json then
                 [ "--json" ]
               else
                 []
               )
            )
    )

let spec_suffixes = [ ".spec.json"; ".spec.txt" ]

let read_specs directory machine =
  let names =
    match Sys.readdir directory with
    | names ->
        Array.to_list names
    | exception Sys_error _ ->
        []
  in
  let bases =
    List.filter_map
      (fun name ->
        List.find_map
          (fun suffix ->
            if
              name = machine ^ suffix
              || String.ends_with ~suffix:("." ^ machine ^ suffix) name
            then
              Some (Filename.chop_suffix name suffix)
            else
              None
          )
          spec_suffixes
      )
      names
    |> List.sort_uniq String.compare
  in
  let read base suffix decode =
    let name = base ^ suffix in
    match
      In_channel.with_open_bin
        (Filename.concat directory name)
        In_channel.input_all
    with
    | text -> (
        match decode text with
        | Ok spec ->
            Some (name, spec)
        | Error message ->
            fail 3 (Filename.concat directory name ^ ": " ^ message)
      )
    | exception Sys_error _ ->
        None
  in
  List.filter_map
    (fun base ->
      match
        read base ".spec.json" (fun text ->
            Result.bind (Silica_json.Json.of_string text) Spec.of_json
        )
      with
      | Some found ->
          Some found
      | None ->
          read base ".spec.txt" Spec.of_text
    )
    bases

let labelled machine transitions =
  List.filter_map
    (fun (record : Transition.t) ->
      let label kept =
        Some
          ( {
              Spec.from = record.from;
              target = record.target;
              event = Transition.label record;
              kept;
            },
            record
          )
      in
      match record.change with
      | _ when record.machine <> machine ->
          None
      | Entered ->
          label false
      | Kept ->
          label (record.from = record.target)
      | Started | Resumed | Unhandled | Ended ->
          None
    )
    transitions
  |> List.sort_uniq (fun (left, _) (right, _) -> compare left right)

let allowed specs record =
  Spec.conforms specs
    {
      header =
        {
          root = record.Transition.machine;
          url = None;
          build = "";
          origin = Runtime { platform = "" };
          dropped = 0;
          loop_from = None;
        };
      transitions = [ record ];
    }
  = []

let label_text (label : Spec.label) =
  ( if label.kept then
      "stay " ^ label.from
    else
      "edge " ^ label.from ^ " -> " ^ label.target
  )
  ^
  if label.event = "" then
    ""
  else
    " on " ^ label.event

let not_checked machine transitions labels =
  let records =
    List.filter
      (fun (record : Transition.t) -> record.machine = machine)
      transitions
  in
  let names =
    List.concat_map
      (fun (record : Transition.t) ->
        List.filter (fun name -> name <> "") [ record.from; record.target ]
      )
      records
    |> List.sort_uniq compare
  in
  {
    Spec.id = machine;
    init =
      ( match
          List.find_opt
            (fun (record : Transition.t) -> record.change = Started)
            records
        with
      | Some record ->
          record.target
      | None ->
          ""
      );
    states =
      List.map
        (fun name ->
          { Spec.name; final = false; timers = []; network = []; held = [] }
        )
        names;
    edges = labels;
  }

let seen_steps session machine =
  let id = request session (Seen machine) in
  let rec find () =
    match answer session id with
    | Inspect.Seen_steps { id = Some got; steps; _ } when got = id ->
        steps
    | _ ->
        find ()
  in
  find ()

let graph socket target machine specs =
  run (fun () ->
      let found =
        Option.fold ~none:[]
          ~some:(fun directory -> read_specs directory machine)
          specs
      in
      let live =
        if Sys.file_exists socket then
          let session = connect socket in
          let current =
            List.concat_map
              (fun (target, instances) ->
                List.filter_map
                  (fun (instance : Inspect.instance) ->
                    if instance.machine = machine then
                      Some
                        ("current " ^ instance.state ^ " at " ^ instance.path
                       ^ " [" ^ target ^ "]"
                        )
                    else
                      None
                  )
                  instances
              )
              (trees session target)
          in
          Some (current, seen_steps session machine)
        else
          None
      in
      let current, transitions = Option.value live ~default:([], []) in
      let labels = labelled machine transitions in
      match found with
      | _ :: _ ->
          List.iter
            (fun (name, spec) ->
              print_endline ("spec " ^ name);
              print_string (Spec.to_text spec)
            )
            found;
          List.iter print_endline current;
          let specs = List.map snd found in
          List.iter
            (fun (label, record) ->
              if allowed specs record then
                print_endline ("seen " ^ label_text label)
              else
                print_endline ("drift " ^ label_text label)
            )
            labels;
          if live = None then
            print_endline
              "no silica dev runs here: the current state is not shown"
      | [] when live = None ->
          fail 3
            ("no spec of " ^ machine
            ^ ( match specs with
              | Some directory ->
                  " in " ^ directory
              | None ->
                  " (give --specs DIR)"
              )
            ^ ", and no silica dev runs here"
            )
      | [] ->
          print_endline ("not checked " ^ machine);
          print_string
            (Spec.to_text
               (not_checked machine transitions (List.map fst labels))
            );
          List.iter print_endline current
  )

let socket =
  Arg.(
    value & opt string default_socket
    & info [ "socket" ] ~docv:"PATH"
        ~doc:"The Unix socket of the hub of $(b,silica dev)."
  )

let json_flag doc = Arg.(value & flag & info [ "json" ] ~doc)

let target =
  Arg.(
    value
    & opt (some string) None
    & info [ "target" ] ~docv:"TARGET"
        ~doc:
          "A target, such as $(b,tab-3f2a) or $(b,server); see $(b,silica \
           inspect targets). With none: every target."
  )

let machines =
  Arg.(
    value & opt_all string []
    & info [ "machine" ] ~docv:"ID"
        ~doc:"Only this machine id. Repeat it for several."
  )

let path =
  Arg.(
    value
    & opt (some string) None
    & info [ "path" ] ~docv:"PATH"
        ~doc:"Only the instance at this path and the instances under it."
  )

let events =
  Arg.(
    value & opt_all string []
    & info [ "event" ] ~docv:"NAME" ~doc:"Only steps on this event name."
  )

let change_conv =
  Arg.conv
    ( (fun text ->
        Result.map_error
          (fun message -> `Msg message)
          (Transition.change_of_string text)
      ),
      fun formatter change ->
        Format.pp_print_string formatter (Transition.change_to_string change)
    )

let changes =
  Arg.(
    value & opt_all change_conv []
    & info [ "change" ] ~docv:"CHANGE"
        ~doc:
          "Only steps with this change: $(b,started), $(b,resumed), \
           $(b,unhandled), $(b,kept), $(b,entered) or $(b,ended)."
  )

let exit_status =
  [
    `S Manpage.s_exit_status;
    `P
      "0: done. 1: the hub answered with an error. 3: no $(b,silica dev) runs \
       here, or bad arguments.";
  ]

let targets_command =
  Cmd.v
    (Cmd.info "targets"
       ~doc:"List the pages and the server that silica dev inspects."
       ~man:exit_status ~exits:[]
    )
    Term.(const targets $ socket $ json_flag "Print the targets message.")

let tree_command =
  Cmd.v
    (Cmd.info "tree"
       ~doc:"Print the running instances, their states, data and resources."
       ~man:
         ([
            `S Manpage.s_description;
            `P
              "One line for each instance: its path, its machine and its \
               state; then the resources it owns, and its snapshot and props \
               when the machine registered its codecs.";
          ]
         @ exit_status
         )
       ~exits:[]
    )
    Term.(
      const tree $ socket $ target $ machines $ path
      $ json_flag "Print one tree message for each target."
    )

let count =
  Arg.(
    value
    & opt (some int) None
    & info [ "count" ] ~docv:"N" ~doc:"Stop after $(docv) lines."
  )

let tail_command =
  Cmd.v
    (Cmd.info "tail" ~doc:"Print each transition of the app as it happens."
       ~man:
         ([
            `S Manpage.s_description;
            `P
              "Prints one line for each step of each machine of each target, \
               from now on, until silica dev stops: the target, the sequence \
               number, the instance path, the states, the change, the event, \
               the cause and the actions. Also: a target that connects or \
               disconnects, and a hot reload.";
            `P
              "With $(b,--json), each line is the protocol message: a \
               $(b,step) holds the transition record, with its data when the \
               machine registered its codecs, and $(b,owns), the resources of \
               the instance after the step.";
          ]
         @ exit_status
         )
       ~exits:[]
    )
    Term.(
      const tail $ socket $ target $ machines $ path $ events $ changes
      $ json_flag "Print each message as one JSON line."
      $ count
    )

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output" ] ~docv:"FILE" ~doc:"Write the trace to $(docv)."
  )

let export_command =
  Cmd.v
    (Cmd.info "export" ~doc:"Write the steps of a target as a trace file."
       ~man:
         ([
            `S Manpage.s_description;
            `P
              "Writes the last 5000 steps that the hub kept for the target as \
               a trace file (JSON Lines), for $(b,silica inspect replay), \
               $(b,silica replay) and $(b,silica conform). With no \
               $(b,--target), the one page that is connected.";
          ]
         @ exit_status
         )
       ~exits:[]
    )
    Term.(const export $ socket $ target $ output)

let replay_command =
  let file =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"TRACE_FILE" ~doc:"A file of $(b,silica inspect export)."
    )
  in
  let machine =
    Arg.(
      value
      & opt (some string) None
      & info [ "machine" ] ~docv:"ID"
          ~doc:"The machine to replay; by default the root of the trace."
    )
  in
  Cmd.v
    (Cmd.info "replay"
       ~doc:"Replay an exported trace in the check of its machine."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Runs $(b,silica replay) $(i,ID) $(i,TRACE_FILE): the steps of \
              the trace go through the check of the machine, and each step, \
              its cause, the rules that fail and the last view are printed. \
              $(i,ID) is the root machine of the trace, which selects the \
              entry whose root it is, such as a joint check. The steps need \
              their event data: register the codecs of the machine.";
         ]
       ~exits:[]
    )
    Term.(const replay $ file $ machine $ json_flag "Print the replay as JSON.")

let graph_command =
  let machine =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"MACHINE" ~doc:"The machine id."
    )
  in
  let specs =
    Arg.(
      value
      & opt (some string) None
      & info [ "specs" ] ~docv:"DIR"
          ~doc:
            "A directory with $(i,MACHINE).spec.json or $(i,MACHINE).spec.txt, \
             and $(i,ENTRY).$(i,MACHINE).spec.json or .spec.txt of each joint \
             check, as $(b,silica check) writes them."
    )
  in
  Cmd.v
    (Cmd.info "graph"
       ~doc:"Print the statechart of a machine with what the app did."
       ~man:
         ([
            `S Manpage.s_description;
            `P
              "Prints each spec of the machine as a $(b,spec) line with the \
               file name and the text form (see $(b,Spec.to_text)), then \
               $(b,current) lines with the state of each running instance, \
               $(b,seen) lines for the edges that the app took and a spec has, \
               and $(b,drift) lines for the edges it took that no spec has. \
               The edges are those of every page since silica dev started, \
               also before a reload. With no spec, the graph is the one that \
               the app took, under the line $(b,not checked).";
          ]
         @ exit_status
         )
       ~exits:[]
    )
    Term.(const graph $ socket $ target $ machine $ specs)

let command =
  Cmd.group
    (Cmd.info "inspect"
       ~doc:"Read the machines of the app that silica dev runs."
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Connects to $(b,silica dev) through its Unix socket \
              $(b,_build/silica-dev/inspect.sock) and reads what the machines \
              of each page and of the server's Live islands do. Run it from \
              the root of the app while $(b,silica dev) runs.";
         ]
    )
    [
      targets_command;
      tree_command;
      tail_command;
      export_command;
      replay_command;
      graph_command;
    ]
