open Silica
module Json = Silica_json.Json
module Memory = Silica_memory

let save_endpoint =
  Remote.declare ~path:"/api/save" ~input:Codec.int ~output:Codec.bool
    ~rejected:Codec.string

type counter_event = Add of int | Save | Saved of (bool, string) Remote.answer
type counter = Counting of int | Saving of int | Saved_at of int

let counter_machine =
  Machine.make_with_props ~id:"counter" ~init:(Counting 0)
    ~name:(function
      | Counting _ -> "counting" | Saving _ -> "saving" | Saved_at _ -> "saved"
      )
    ~during:(fun _ -> function
      | Saving count ->
          [ Remote.call save_endpoint count (fun answer -> Saved answer) ]
      | Counting _ | Saved_at _ ->
          []
      )
    (fun _ state event ->
      match (state, event) with
      | Counting count, Add more ->
          Go (Counting (count + more))
      | Counting count, Save ->
          Go (Saving count)
      | Saving count, Saved (Ok true) ->
          Go (Saved_at count)
      | _ ->
          Ignore
    )

let counter =
  Component.make_with_props counter_machine (fun label state ->
      Node.text
        (Printf.sprintf "%s: %s" label
           ( match state with
           | Counting count ->
               string_of_int count
           | Saving _ ->
               "saving"
           | Saved_at count ->
               Printf.sprintf "saved %d" count
           )
        )
  )

type panel = One | Two

let panel_machine =
  Machine.make ~id:"panel" ~init:One
    ~name:(function One -> "one" | Two -> "two")
    (fun state () -> match state with One -> Go Two | Two -> Ignore)

let panel =
  Component.make panel_machine (fun state ->
      Node.list
        (Component.node_with_props ~key:"left" ~props:"left" counter
        ::
        ( match state with
        | One ->
            []
        | Two ->
            [ Component.node_with_props ~key:"right" ~props:"right" counter ]
        )
        )
  )

let add =
  Mcp.make ~name:"counter.add" ~description:"Add to the count"
    Mcp.Input.(field "amount" ~description:"How much to add" int)

let save = Mcp.make ~name:"counter.save" ~description:"Save" Mcp.Input.none
let split =
  Mcp.make ~name:"panel.split" ~description:"Show two counters" Mcp.Input.none

let publications =
  [
    Mcp.tools panel_machine [ Mcp.on split Fun.id ];
    Mcp.tools counter_machine
      [ Mcp.on add (fun amount -> Add amount); Mcp.on save (fun () -> Save) ];
  ]

let saved = ref []

let answer request =
  saved := Memory.body request :: !saved;
  (200, "true")

let server () =
  saved := [];
  Silica_mcp.make ~name:"test" ~version:"1.0.0" ~instructions:"Count." ~answer
    publications (Memory.mount panel)

let json text =
  match Json.of_string text with
  | Ok json ->
      json
  | Error reason ->
      Alcotest.failf "bad JSON in the test %s: %s" text reason

let meta =
  {|"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}|}

let answer_to server text =
  match Silica_mcp.handle server (json text) with
  | Some answer ->
      Json.to_string answer
  | None ->
      Alcotest.failf "no answer to %s" text

let request server ?(params = "") meth =
  answer_to server
    (Printf.sprintf {|{"jsonrpc":"2.0","id":1,"method":%S,"params":{%s%s}}|}
       meth params
       ( if params = "" then
           meta
         else
           "," ^ meta
       )
    )

let call server name arguments =
  request server "tools/call"
    ~params:(Printf.sprintf {|"name":%S,"arguments":%s|} name arguments)

let server_info =
  {|"_meta":{"io.modelcontextprotocol/serverInfo":{"name":"test","version":"1.0.0"}}|}

let result fields =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":1,"result":{"resultType":"complete",%s,%s}}|} fields
    server_info

let ok structured =
  result
    (Printf.sprintf
       {|"content":[{"type":"text","text":%s}],"structuredContent":%s,"isError":false|}
       (Json.to_string (Json.declassify (`String structured)))
       structured
    )

let refused sentence =
  result
    (Printf.sprintf {|"content":[{"type":"text","text":%S}],"isError":true|}
       sentence
    )

let error code message =
  Printf.sprintf {|{"jsonrpc":"2.0","id":1,"error":{"code":%d,"message":%S}}|}
    code message

let check = Alcotest.(check string)

let discover () =
  check "server/discover"
    (result
       {|"supportedVersions":["2026-07-28"],"capabilities":{"tools":{}},"instructions":"Count.","ttlMs":300000,"cacheScope":"public"|}
    )
    (request (server ()) "server/discover")

let version () =
  let server = server () in
  check "an old version"
    {|{"jsonrpc":"2.0","id":1,"error":{"code":-32022,"message":"Unsupported protocol version","data":{"supported":["2026-07-28"],"requested":"2025-11-25"}}}|}
    (answer_to server
       {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2025-11-25","io.modelcontextprotocol/clientCapabilities":{}}}}|}
    );
  check "no _meta"
    (error (-32602)
       "The request has no _meta io.modelcontextprotocol/protocolVersion."
    )
    (answer_to server {|{"jsonrpc":"2.0","id":1,"method":"tools/list"}|});
  check "no capabilities"
    (error (-32602)
       "The request has no _meta io.modelcontextprotocol/clientCapabilities."
    )
    (answer_to server
       {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}|}
    );
  check "initialize"
    (error (-32601)
       "Method not found: initialize. This server speaks MCP 2026-07-28 only; \
        send server/discover."
    )
    (request server "initialize");
  Alcotest.(check bool)
    "a notification gets no answer" true
    (Option.is_none
       (Silica_mcp.handle server
          (json
             {|{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":1}}|}
          )
       )
    )

let tools_json =
  {|"tools":[{"name":"read","description":"The current URL, the running instances of the published machines with their states, the timers that wait (delays in ms), and the text of the page.","inputSchema":{"type":"object","additionalProperties":false},"annotations":{"readOnlyHint":true,"openWorldHint":false}},{"name":"panel.split","description":"Show two counters","inputSchema":{"type":"object","properties":{"instance":{"type":"string","description":"The instance path of the running instance of panel that gets the event. Leave it out when one instance runs."}},"additionalProperties":false}},{"name":"counter.add","description":"Add to the count","inputSchema":{"type":"object","properties":{"amount":{"type":"integer","minimum":-4611686018427387904,"maximum":4611686018427387903,"description":"How much to add"},"instance":{"type":"string","description":"The instance path of the running instance of counter that gets the event. Leave it out when one instance runs."}},"required":["amount"],"additionalProperties":false}},{"name":"counter.save","description":"Save","inputSchema":{"type":"object","properties":{"instance":{"type":"string","description":"The instance path of the running instance of counter that gets the event. Leave it out when one instance runs."}},"additionalProperties":false}}]|}

let list () =
  let server = server () in
  let listed =
    result (tools_json ^ {|,"ttlMs":300000,"cacheScope":"public"|})
  in
  check "tools/list" listed (request server "tools/list");
  ignore (call server "panel.split" "{}");
  check "the same list after a call" listed (request server "tools/list");
  check "a cursor"
    (error (-32602)
       "The tool list has one page, so there is no cursor to follow."
    )
    (request server "tools/list" ~params:{|"cursor":"next"|})

let instances one two =
  Printf.sprintf
    {|"url":"/","instances":[{"machine":"panel","instance":"panel","state":%S}%s]|}
    one two

let call_ok () =
  let server = server () in
  check "counter.add"
    (ok
       (Printf.sprintf
          {|{"instance":"panel/#left:counter","state":"counting",%s}|}
          (instances "one"
             {|,{"machine":"counter","instance":"panel/#left:counter","state":"counting"}|}
          )
       )
    )
    (call server "counter.add" {|{"amount":2}|});
  check "counter.save settles the call of the app"
    (ok
       (Printf.sprintf {|{"instance":"panel/#left:counter","state":"saved",%s}|}
          (instances "one"
             {|,{"machine":"counter","instance":"panel/#left:counter","state":"saved"}|}
          )
       )
    )
    (call server "counter.save" "{}");
  Alcotest.(check (list string)) "the call had the count" [ "2" ] !saved;
  check "read"
    (ok
       (Printf.sprintf {|{"timers":[],"page":"text \"left: saved 2\"\n",%s}|}
          (instances "one"
             {|,{"machine":"counter","instance":"panel/#left:counter","state":"saved"}|}
          )
       )
    )
    (call server "read" "{}")

let call_refused () =
  let server = server () in
  ignore (call server "counter.save" "{}");
  check "a state that ignores the event"
    (refused
       "counter at panel/#left:counter is in the state saved, which ignores \
        counter.add."
    )
    (call server "counter.add" {|{"amount":1}|})

let decode_error () =
  let server = server () in
  check "a string for an int"
    (refused
       "The arguments of counter.add do not decode: amount: expected an \
        integer, got a string."
    )
    (call server "counter.add" {|{"amount":"two"}|});
  check "an unknown field"
    (refused "The arguments of counter.add do not decode: unknown field colour.")
    (call server "counter.add" {|{"amount":1,"colour":"red"}|});
  check "read takes no arguments"
    (refused "The arguments of read do not decode: unknown field page.")
    (call server "read" {|{"page":1}|});
  check "arguments that are not an object"
    (error (-32602) "The arguments of tools/call must be an object.")
    (call server "counter.add" "[1]")

let unknown_tool () =
  check "an unknown tool"
    (error (-32602) "Unknown tool: counter.reset")
    (call (server ()) "counter.reset" "{}")

let several () =
  let server = server () in
  ignore (call server "panel.split" "{}");
  check "no instance among two"
    (refused
       "Several instances of counter run: panel/#left:counter, \
        panel/#right:counter. Give the argument instance, one of them."
    )
    (call server "counter.add" {|{"amount":1}|});
  check "an instance that does not run"
    (refused
       "No instance of counter runs at panel/#middle:counter. The running \
        ones: panel/#left:counter, panel/#right:counter."
    )
    (call server "counter.add"
       {|{"amount":1,"instance":"panel/#middle:counter"}|}
    );
  check "the instance on the right"
    (ok
       (Printf.sprintf
          {|{"instance":"panel/#right:counter","state":"counting",%s}|}
          (instances "two"
             {|,{"machine":"counter","instance":"panel/#left:counter","state":"counting"},{"machine":"counter","instance":"panel/#right:counter","state":"counting"}|}
          )
       )
    )
    (call server "counter.add"
       {|{"amount":5,"instance":"panel/#right:counter"}|}
    );
  check "split again is ignored"
    (refused "panel at panel is in the state two, which ignores panel.split.")
    (call server "panel.split" "{}")

let names () =
  let app = Memory.mount panel in
  match
    Silica_mcp.make ~name:"test" ~version:"1.0.0" ~answer
      [
        Mcp.tools panel_machine
          [
            Mcp.on
              (Mcp.make ~name:"read" ~description:"Read" Mcp.Input.none)
              Fun.id;
          ];
      ]
      app
  with
  | _ ->
      Alcotest.fail "a second tool named read"
  | exception Invalid_argument message ->
      check "the reason" "Silica_mcp.make: two tools are named read" message

let serve () =
  let input = Filename.temp_file "silica_mcp" ".in" in
  let output = Filename.temp_file "silica_mcp" ".out" in
  Out_channel.with_open_text input (fun channel ->
      Printf.fprintf channel "%s\n\nnot json\n%s\n"
        (Printf.sprintf
           {|{"jsonrpc":"2.0","id":7,"method":"server/discover","params":{%s}}|}
           meta
        )
        {|{"jsonrpc":"2.0","method":"notifications/cancelled"}|}
  );
  In_channel.with_open_text input (fun input ->
      Out_channel.with_open_text output (fun output ->
          Silica_mcp.serve ~input ~output (server ())
      )
  );
  let lines = In_channel.with_open_text output In_channel.input_lines in
  Sys.remove input;
  Sys.remove output;
  match lines with
  | [ discovered; parse_error ] ->
      Alcotest.(check bool)
        "the answer to server/discover" true
        (String.starts_with ~prefix:{|{"jsonrpc":"2.0","id":7,"result":|}
           discovered
        );
      Alcotest.(check bool)
        "a parse error" true
        (String.starts_with
           ~prefix:{|{"jsonrpc":"2.0","id":null,"error":{"code":-32700,|}
           parse_error
        )
  | lines ->
      Alcotest.failf "%d lines: %s" (List.length lines)
        (String.concat "\n" lines)

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica_mcp"
    [
      ( "protocol",
        [
          case "server/discover" discover;
          case "the protocol version of each request" version;
          case "serve reads lines until the end" serve;
        ]
      );
      ( "tools",
        [
          case "tools/list is fixed" list;
          case "a call sends the event and settles the app" call_ok;
          case "a state that ignores the event refuses the call" call_refused;
          case "arguments that do not decode" decode_error;
          case "an unknown tool is a JSON-RPC error" unknown_tool;
          case "the argument instance among several instances" several;
          case "two tools with one name" names;
        ]
      );
    ]
