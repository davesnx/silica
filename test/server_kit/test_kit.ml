open Silica_server_kit

let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])
     )
    )

let unhex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let str = Alcotest.string
let check_hex name expected actual =
  Alcotest.check str name expected (hex actual)

let nist56 = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
let million_a = String.make 1_000_000 'a'

let test_sha1 () =
  check_hex "empty" "da39a3ee5e6b4b0d3255bfef95601890afd80709" (Sha1.digest "");
  check_hex "abc" "a9993e364706816aba3e25717850c26c9cd0d89d" (Sha1.digest "abc");
  check_hex "nist56" "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
    (Sha1.digest nist56);
  check_hex "million a" "34aa973cd4c4daa4f61eeb2bdbad27316534016f"
    (Sha1.digest million_a);
  Alcotest.(check int) "length" 20 (String.length (Sha1.digest "x"))

let test_sha256 () =
  Alcotest.check str "empty"
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (Sha256.hex "");
  Alcotest.check str "abc"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (Sha256.hex "abc");
  Alcotest.check str "nist56"
    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    (Sha256.hex nist56);
  Alcotest.check str "million a"
    "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
    (Sha256.hex million_a);
  Alcotest.(check int) "length" 32 (String.length (Sha256.digest "x"));
  check_hex "digest matches hex" (Sha256.hex "abc") (Sha256.digest "abc")

let rep byte n = String.make n (Char.chr byte)

let hmac_cases =
  [
    ( "case 1",
      rep 0x0b 20,
      "Hi There",
      "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
    );
    ( "case 2",
      "Jefe",
      "what do ya want for nothing?",
      "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
    );
    ( "case 3",
      rep 0xaa 20,
      rep 0xdd 50,
      "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"
    );
    ( "case 6",
      rep 0xaa 131,
      "Test Using Larger Than Block-Size Key - Hash Key First",
      "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
    );
    ( "case 7",
      rep 0xaa 131,
      "This is a test using a larger than block-size key and a larger than \
       block-size data. The key needs to be hashed before being used by the \
       HMAC algorithm.",
      "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"
    );
  ]

let test_hmac () =
  List.iter
    (fun (name, key, data, expected) ->
      check_hex name expected (Hmac.sha256 ~key data)
    )
    hmac_cases

let test_base64 () =
  let cases =
    [
      ("", "");
      ("f", "Zg==");
      ("fo", "Zm8=");
      ("foo", "Zm9v");
      ("foob", "Zm9vYg==");
      ("fooba", "Zm9vYmE=");
      ("foobar", "Zm9vYmFy");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.check str ("encode " ^ input) expected (Base64.encode input)
    )
    cases;
  Alcotest.check str "standard alphabet" "+/+/" (Base64.encode "\xfb\xff\xbf");
  Alcotest.check str "url alphabet" "-_-_" (Base64.encode_url "\xfb\xff\xbf");
  Alcotest.check str "url no padding" "Zm9vYg" (Base64.encode_url "foob");
  Alcotest.check str "url empty" "" (Base64.encode_url "")

let is_lower_hex s =
  String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

let test_random () =
  let bytes n = Result.get_ok (Random_source.bytes n) in
  let hex_id n = Result.get_ok (Random_source.hex_id ~bytes:n) in
  Alcotest.(check int) "bytes length" 16 (String.length (bytes 16));
  Alcotest.(check int) "zero bytes" 0 (String.length (bytes 0));
  Alcotest.(check bool) "bytes differ" true (bytes 16 <> bytes 16);
  let id = hex_id 16 in
  Alcotest.(check int) "hex length" 32 (String.length id);
  Alcotest.(check bool) "lowercase hex" true (is_lower_hex id);
  Alcotest.(check bool) "ids differ" true (hex_id 16 <> hex_id 16)

let test_random_source_missing () =
  Alcotest.(check (result string string))
    "no source"
    (Error
       "Random_source: /nonexistent/urandom: /nonexistent/urandom: No such \
        file or directory"
    )
    (Random_source.bytes ~source:"/nonexistent/urandom" 16)

let message_pp ppf (m : Ws.message) =
  let s =
    match m with
    | Text t ->
        "Text " ^ t
    | Binary b ->
        "Binary " ^ hex b
    | Close { code; reason } ->
        Printf.sprintf "Close %s %s"
          (match code with Some c -> string_of_int c | None -> "none")
          reason
    | Ping p ->
        "Ping " ^ hex p
    | Pong p ->
        "Pong " ^ hex p
  in
  Format.pp_print_string ppf s

let message = Alcotest.testable message_pp ( = )

let error_pp ppf (e : Ws.error) =
  match e with
  | Protocol m ->
      Format.fprintf ppf "Protocol %s" m
  | Too_big ->
      Format.pp_print_string ppf "Too_big"

let error = Alcotest.testable error_pp ( = )
let result = Alcotest.(result (option message) error)

let reader ?(log = ref []) data =
  let pos = ref 0 in
  fun n ->
    log := n :: !log;
    if !pos + n > String.length data then
      None
    else begin
      let s = String.sub data !pos n in
      pos := !pos + n;
      Some s
    end

let read_all ?(max = 1_000_000) data = Ws.read_message ~max ~read:(reader data)

let mask = "\x37\xfa\x21\x3d"

let test_accept_key () =
  Alcotest.check str "rfc example" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (Ws.accept_key "dGhlIHNhbXBsZSBub25jZQ==")

let test_ws_rfc_examples () =
  Alcotest.check result "masked hello" (Ok (Some (Ws.Text "Hello")))
    (read_all (unhex "818537fa213d7f9f4d5158"));
  Alcotest.check str "unmasked hello" "\x81\x05Hello" (Ws.encode (Text "Hello"));
  List.iter
    (fun length ->
      let text = String.make length 'a' in
      Alcotest.check str
        ("header and text are the frame, length " ^ string_of_int length)
        (Ws.encode (Text text))
        (Ws.text_header text ^ text)
    )
    [ 0; 125; 126; 65535; 65536; 300_000 ];
  Alcotest.check str "masked encode"
    (unhex "818537fa213d7f9f4d5158")
    (Ws.encode_masked ~mask (Text "Hello"))

let test_ws_lengths () =
  List.iter
    (fun n ->
      let payload = String.init n (fun i -> Char.chr (i land 0xff)) in
      let frame = Ws.encode_masked ~mask (Binary payload) in
      let header =
        if n <= 125 then
          2 + 4
        else if n <= 65535 then
          4 + 4
        else
          10 + 4
      in
      Alcotest.(check int)
        (Printf.sprintf "frame size %d" n)
        (header + n) (String.length frame);
      Alcotest.check result
        (Printf.sprintf "read %d" n)
        (Ok (Some (Ws.Binary payload))) (read_all frame);
      Alcotest.(check int)
        (Printf.sprintf "server frame size %d" n)
        (header - 4 + n)
        (String.length (Ws.encode (Binary payload)))
    )
    [ 0; 125; 126; 65535; 65536 ]

let test_ws_truncated () =
  let frame = Ws.encode_masked ~mask (Text "Hello") in
  Alcotest.check result "cut payload" (Error (Protocol "truncated"))
    (read_all (String.sub frame 0 (String.length frame - 1)));
  Alcotest.check result "cut header" (Error (Protocol "truncated"))
    (read_all (String.sub frame 0 1));
  Alcotest.check result "cut mask" (Error (Protocol "truncated"))
    (read_all (String.sub frame 0 4));
  Alcotest.check result "clean end" (Ok None) (read_all "")

let test_ws_too_big () =
  let frame = Ws.encode_masked ~mask (Binary (String.make 200 'x')) in
  let log = ref [] in
  let r = Ws.read_message ~max:100 ~read:(reader ~log frame) in
  Alcotest.check result "too big" (Error Too_big) r;
  Alcotest.(check bool)
    "payload not requested" true
    (List.for_all (fun n -> n < 200) !log);
  let huge = "\x82\xff\x00\x00\x00\x00\x7f\xff\xff\xff" ^ mask in
  let log = ref [] in
  Alcotest.check result "huge length" (Error Too_big)
    (Ws.read_message ~max:100 ~read:(reader ~log huge));
  Alcotest.(check bool)
    "nothing large requested" true
    (List.for_all (fun n -> n <= 8) !log);
  let exact = Ws.encode_masked ~mask (Binary (String.make 100 'x')) in
  Alcotest.check result "at max"
    (Ok (Some (Ws.Binary (String.make 100 'x'))))
    (Ws.read_message ~max:100 ~read:(reader exact))

let is_protocol = function Error (Ws.Protocol _) -> true | _ -> false

let test_ws_protocol_errors () =
  let check name data =
    Alcotest.(check bool) name true (is_protocol (read_all data))
  in
  Alcotest.check result "unmasked"
    (Error (Protocol "client frame is not masked"))
    (read_all "\x81\x05Hello....");
  check "reserved bit 1" ("\xc1\x80" ^ mask);
  check "reserved bit 2" ("\xa1\x80" ^ mask);
  check "reserved bit 3" ("\x91\x80" ^ mask);
  check "unknown opcode 3" ("\x83\x80" ^ mask);
  check "unknown opcode 11" ("\x8b\x80" ^ mask);
  check "continuation" ("\x80\x80" ^ mask);
  check "fin zero" ("\x01\x80" ^ mask);
  check "control too long" ("\x89\xfe\x00\x7e" ^ mask ^ String.make 126 'x');
  check "control without fin" ("\x09\x80" ^ mask);
  check "64 bit top bit" ("\x82\xff\x80\x00\x00\x00\x00\x00\x00\x00" ^ mask);
  Alcotest.check result "fragmented message"
    (Error (Protocol "fragmented messages are not supported"))
    (read_all ("\x01\x80" ^ mask))

let test_ws_control () =
  let close = Ws.Close { code = Some 1000; reason = "bye" } in
  Alcotest.check str "close encode" "\x88\x05\x03\xe8bye" (Ws.encode close);
  Alcotest.check result "close read" (Ok (Some close))
    (read_all (Ws.encode_masked ~mask close));
  let empty = Ws.Close { code = None; reason = "" } in
  Alcotest.check str "close empty encode" "\x88\x00" (Ws.encode empty);
  Alcotest.check result "close empty read" (Ok (Some empty))
    (read_all (Ws.encode_masked ~mask empty));
  Alcotest.check str "ping encode" "\x89\x02hi" (Ws.encode (Ping "hi"));
  Alcotest.check str "pong encode" "\x8a\x02hi" (Ws.encode (Pong "hi"));
  Alcotest.check str "text encode" "\x81\x02hi" (Ws.encode (Text "hi"));
  Alcotest.check str "binary encode" "\x82\x02hi" (Ws.encode (Binary "hi"));
  Alcotest.check result "ping read" (Ok (Some (Ws.Ping "payload")))
    (read_all (Ws.encode_masked ~mask (Ping "payload")))

let test_ws_round_trip () =
  let messages =
    [
      Ws.Text "caf\xc3\xa9";
      Binary "\x00\x01\xff";
      Close { code = Some 1001; reason = "going away" };
      Ping "";
      Pong "p";
    ]
  in
  let frames =
    String.concat ""
      (List.map (Ws.encode_masked ~mask:"\x01\x02\x03\x04") messages)
  in
  let read = reader frames in
  List.iter
    (fun m ->
      Alcotest.check result "round trip" (Ok (Some m))
        (Ws.read_message ~max:1000 ~read)
    )
    messages;
  Alcotest.check result "end" (Ok None) (Ws.read_message ~max:1000 ~read)

let metafile =
  {|{"inputs":{},"outputs":{
 "dist/client_main-4F2A9C1B.js":{"entryPoint":"out/client_main.mjs","bytes":10},
 "dist/client_main-9C0AA1B2.css":{"entryPoint":"out/client_main.mjs","bytes":5},
 "dist/chunk-ABCDEF12.js":{"bytes":3}}}|}

let get_ok = function Ok v -> v | Error e -> Alcotest.fail e

let test_assets () =
  let t = get_ok (Assets.of_metafile ~prefix:"/assets/" metafile) in
  Alcotest.(check (option string))
    "js" (Some "/assets/client_main-4F2A9C1B.js")
    (Assets.resolve t "client_main.js");
  Alcotest.(check (option string))
    "css" (Some "/assets/client_main-9C0AA1B2.css")
    (Assets.resolve t "client_main.css");
  Alcotest.(check (option string))
    "chunk is not an entry" None
    (Assets.resolve t "chunk.js");
  Alcotest.(check (option string)) "unknown" None (Assets.resolve t "x.js");
  Alcotest.(check (list string))
    "all"
    [ "/assets/client_main-4F2A9C1B.js"; "/assets/client_main-9C0AA1B2.css" ]
    (List.sort compare (Assets.all t))

let split_metafile =
  {|{"inputs":{},"outputs":{
 "dist/main-AAAAAAAA.js":{"entryPoint":"out/main.mjs","imports":[
   {"path":"dist/chunk-BBBBBBBB.js","kind":"import-statement"},
   {"path":"dist/page-CCCCCCCC.js","kind":"dynamic-import"}]},
 "dist/chunk-BBBBBBBB.js":{"imports":[
   {"path":"dist/base-DDDDDDDD.js","kind":"import-statement"}]},
 "dist/page-CCCCCCCC.js":{"imports":[
   {"path":"dist/chunk-BBBBBBBB.js","kind":"import-statement"}]},
 "dist/base-DDDDDDDD.js":{"imports":[
   {"path":"dist/chunk-BBBBBBBB.js","kind":"import-statement"}]}}}|}

let test_assets_split () =
  let t = get_ok (Assets.of_metafile ~prefix:"/s/" split_metafile) in
  Alcotest.(check (list string))
    "static imports, through a chunk, no dynamic import, no loop"
    [ "/s/chunk-BBBBBBBB.js"; "/s/base-DDDDDDDD.js" ]
    (Assets.imports t "/s/main-AAAAAAAA.js");
  Alcotest.(check (list string))
    "outputs"
    [
      "/s/main-AAAAAAAA.js";
      "/s/chunk-BBBBBBBB.js";
      "/s/page-CCCCCCCC.js";
      "/s/base-DDDDDDDD.js";
    ]
    (Assets.outputs t);
  Alcotest.(check (list string)) "unknown" [] (Assets.imports t "/s/x.js")

let test_assets_errors () =
  let is_error = function Error _ -> true | Ok _ -> false in
  Alcotest.(check bool)
    "invalid json" true
    (is_error (Assets.of_metafile ~prefix:"/" "{not json"));
  Alcotest.(check bool)
    "no outputs" true
    (is_error (Assets.of_metafile ~prefix:"/" {|{"inputs":{}}|}));
  Alcotest.(check bool)
    "outputs not object" true
    (is_error (Assets.of_metafile ~prefix:"/" {|{"outputs":[]}|}));
  Alcotest.(check bool)
    "duplicate logical name" true
    (is_error
       (Assets.of_metafile ~prefix:"/"
          {|{"outputs":{"a/x-AAAAAAAA.js":{"entryPoint":"p/x.ts"},"b/x-BBBBBBBB.js":{"entryPoint":"q/x.ts"}}}|}
       )
    );
  Alcotest.(check bool)
    "empty outputs ok" true
    (not (is_error (Assets.of_metafile ~prefix:"/" {|{"outputs":{}}|})))

let test_is_hashed () =
  let check name expected input =
    Alcotest.(check bool) name expected (Assets.is_hashed input)
  in
  check "hashed js" true "client_main-4F2A9C1B.js";
  check "with directory" true "dist/client_main-4F2A9C1B.js";
  check "eight chars" true "a-ABCDEFGH.css";
  check "longer hash" true "a-ABCDEFGH12.css";
  check "four chars" false "a-4F2A.js";
  check "seven chars" false "a-ABCDEFG.js";
  check "lowercase" false "a-abcdefgh.js";
  check "no hash" false "client_main.js";
  check "no extension" false "a-ABCDEFGH";
  check "directory hashed only" false "dist-ABCDEFGH/plain.js";
  check "no stem" false "-ABCDEFGH.js"

let test_content_type () =
  let check ext expected =
    Alcotest.check str ext expected (Assets.content_type ("dist/file." ^ ext))
  in
  check "js" "text/javascript; charset=utf-8";
  check "mjs" "text/javascript; charset=utf-8";
  check "css" "text/css; charset=utf-8";
  check "html" "text/html; charset=utf-8";
  check "json" "application/json; charset=utf-8";
  check "map" "application/json; charset=utf-8";
  check "txt" "text/plain; charset=utf-8";
  check "svg" "image/svg+xml";
  check "png" "image/png";
  check "jpg" "image/jpeg";
  check "jpeg" "image/jpeg";
  check "webp" "image/webp";
  check "woff2" "font/woff2";
  check "ico" "image/x-icon";
  check "bin" "application/octet-stream";
  Alcotest.check str "no extension" "application/octet-stream"
    (Assets.content_type "Makefile")

let test_etag () =
  Alcotest.check str "abc" "\"ba7816bf8f01cfea\"" (Assets.etag "abc");
  Alcotest.(check bool) "differs" true (Assets.etag "a" <> Assets.etag "b")

let session ?(created = 0.) ?(last_seen = 0.) ?(data = []) id =
  { Session_store.id; created; last_seen; data }

let session_option =
  Alcotest.testable
    (fun ppf s ->
      Format.pp_print_string ppf
        ( match s with
        | None ->
            "None"
        | Some (s : Session_store.session) ->
            s.id
        )
    )
    ( = )

let test_store_basic () =
  let store = Session_store.create () in
  Alcotest.check session_option "absent" None
    (Session_store.get store ~now:0. "a");
  let s = session ~data:[ ("k", "v") ] "a" in
  Session_store.set store s;
  Alcotest.check session_option "present" (Some s)
    (Session_store.get store ~now:1. "a");
  Alcotest.(check int) "count" 1 (Session_store.count store);
  Session_store.remove store "a";
  Alcotest.check session_option "removed" None
    (Session_store.get store ~now:1. "a");
  Alcotest.(check int) "count after" 0 (Session_store.count store)

let test_store_idle () =
  let store = Session_store.create ~idle:100. ~absolute:1000. () in
  Session_store.set store (session ~last_seen:10. "a");
  Alcotest.check session_option "at boundary"
    (Some (session ~last_seen:10. "a"))
    (Session_store.get store ~now:110. "a");
  Alcotest.check session_option "just over" None
    (Session_store.get store ~now:110.001 "a");
  Alcotest.(check int) "removed" 0 (Session_store.count store)

let test_store_absolute () =
  let store = Session_store.create ~idle:100. ~absolute:1000. () in
  Session_store.set store (session ~created:0. ~last_seen:1000. "a");
  Alcotest.check session_option "at boundary"
    (Some (session ~created:0. ~last_seen:1000. "a"))
    (Session_store.get store ~now:1000. "a");
  Alcotest.check session_option "fresh but too old" None
    (Session_store.get store ~now:1000.5 "a")

let test_store_defaults () =
  let store = Session_store.create () in
  Session_store.set store (session ~created:0. ~last_seen:0. "a");
  Alcotest.(check bool)
    "alive at 8 hours" true
    (Session_store.get store ~now:28_800. "a" <> None);
  Alcotest.(check bool)
    "idle over 8 hours" true
    (Session_store.get store ~now:28_800.5 "a" = None);
  Session_store.set store (session ~created:0. ~last_seen:604_800. "b");
  Alcotest.(check bool)
    "alive at 7 days" true
    (Session_store.get store ~now:604_800. "b" <> None);
  Alcotest.(check bool)
    "absolute over 7 days" true
    (Session_store.get store ~now:604_800.5 "b" = None)

let test_store_expire () =
  let store = Session_store.create ~idle:10. ~absolute:100. () in
  Session_store.set store (session ~last_seen:0. "old");
  Session_store.set store (session ~created:90. ~last_seen:95. "fresh");
  Session_store.set store (session ~created:0. ~last_seen:100. "ancient");
  Alcotest.(check int) "expired" 2 (Session_store.expire store ~now:101.);
  Alcotest.(check int) "remaining" 1 (Session_store.count store);
  Alcotest.(check int) "again" 0 (Session_store.expire store ~now:101.)

let test_store_cap () =
  let store = Session_store.create ~max_sessions:2 () in
  Session_store.set store (session ~last_seen:5. "a");
  Session_store.set store (session ~last_seen:1. "b");
  Session_store.set store (session ~last_seen:9. "c");
  Alcotest.(check int) "count" 2 (Session_store.count store);
  Alcotest.check session_option "oldest evicted" None
    (Session_store.get store ~now:10. "b");
  Alcotest.(check bool)
    "others kept" true
    (Session_store.get store ~now:10. "a" <> None
    && Session_store.get store ~now:10. "c" <> None
    );
  let updated = session ~last_seen:6. ~data:[ ("n", "2") ] "a" in
  Session_store.set store updated;
  Alcotest.(check int) "replace keeps count" 2 (Session_store.count store);
  Alcotest.check session_option "replaced" (Some updated)
    (Session_store.get store ~now:10. "a");
  Alcotest.(check bool)
    "replace did not evict" true
    (Session_store.get store ~now:10. "c" <> None)

let test_store_ops () =
  let store = Session_store.create ~idle:10. () in
  let ops = Session_store.ops store in
  ops.set (session ~last_seen:0. "a");
  Alcotest.(check int) "shared state" 1 (Session_store.count store);
  Alcotest.(check bool) "get" true (ops.get ~now:5. "a" <> None);
  Alcotest.(check int) "expire" 1 (ops.expire ~now:50.);
  ops.set (session "b");
  ops.remove "b";
  Alcotest.(check int) "remove" 0 (Session_store.count store)

let test_store_threads () =
  let store = Session_store.create () in
  let worker k () =
    for i = 0 to 499 do
      Session_store.set store (session (Printf.sprintf "%d-%d" k i))
    done
  in
  let domains = List.init 4 (fun k -> Domain.spawn (worker k)) in
  List.iter Domain.join domains;
  Alcotest.(check int) "all stored" 2000 (Session_store.count store)

let counter (s : Session_store.session) =
  match List.assoc_opt "n" s.data with Some n -> int_of_string n | None -> 0

let bump (s : Session_store.session) =
  { s with data = [ ("n", string_of_int (counter s + 1)) ] }

let test_store_update () =
  let store = Session_store.create ~idle:10. () in
  Alcotest.(check bool)
    "absent: none" true
    (Session_store.update store ~now:0. "a" bump = None);
  Alcotest.(check int) "absent: nothing stored" 0 (Session_store.count store);
  Session_store.set store (session ~last_seen:0. "a");
  ( match Session_store.update store ~now:1. "a" bump with
  | Some s ->
      Alcotest.(check int) "the new value" 1 (counter s)
  | None ->
      Alcotest.fail "expected the updated session"
  );
  Alcotest.(check (option int))
    "stored" (Some 1)
    (Option.map counter (Session_store.get store ~now:1. "a"));
  ( match
      Session_store.update store ~now:1. "a" (fun s ->
          { (bump s) with id = "b" }
      )
    with
  | Some s ->
      Alcotest.(check string) "the id is kept" "a" s.id
  | None ->
      Alcotest.fail "expected the updated session"
  );
  Alcotest.(check bool)
    "no session b" true
    (Session_store.get store ~now:1. "b" = None);
  Alcotest.(check bool)
    "expired: none" true
    (Session_store.update store ~now:100. "a" bump = None);
  let ops = Session_store.ops (Session_store.create ()) in
  ops.set (session "c");
  Alcotest.(check (option int))
    "through ops" (Some 1)
    (Option.map counter (ops.update ~now:0. "c" bump))

let test_store_update_is_atomic () =
  let store = Session_store.create () in
  Session_store.set store (session "shared");
  let worker () =
    for _ = 1 to 500 do
      ignore (Session_store.update store ~now:0. "shared" bump)
    done
  in
  let domains = List.init 4 (fun _ -> Domain.spawn worker) in
  List.iter Domain.join domains;
  Alcotest.(check (option int))
    "no update is lost" (Some 2000)
    (Option.map counter (Session_store.get store ~now:0. "shared"))

let test_csrf () =
  let secret = "s3cret" in
  let token = Csrf.token ~secret ~session:"sess-1" in
  Alcotest.check str "definition"
    (Base64.encode_url (Hmac.sha256 ~key:secret "silica-csrf:sess-1"))
    token;
  Alcotest.(check bool) "valid" true (Csrf.valid ~secret ~session:"sess-1" token);
  Alcotest.(check bool)
    "other session" false
    (Csrf.valid ~secret ~session:"sess-2" token);
  Alcotest.(check bool)
    "other secret" false
    (Csrf.valid ~secret:"other" ~session:"sess-1" token);
  Alcotest.(check bool)
    "truncated" false
    (Csrf.valid ~secret ~session:"sess-1"
       (String.sub token 0 (String.length token - 1))
    );
  Alcotest.(check bool)
    "extended" false
    (Csrf.valid ~secret ~session:"sess-1" (token ^ "A"));
  Alcotest.(check bool) "empty" false (Csrf.valid ~secret ~session:"sess-1" "")

let () =
  Alcotest.run "server_kit"
    [
      ( "hash",
        [
          Alcotest.test_case "sha1" `Quick test_sha1;
          Alcotest.test_case "sha256" `Quick test_sha256;
          Alcotest.test_case "hmac" `Quick test_hmac;
        ]
      );
      ("base64", [ Alcotest.test_case "vectors" `Quick test_base64 ]);
      ( "random",
        [
          Alcotest.test_case "bytes and ids" `Quick test_random;
          Alcotest.test_case "a source that does not open" `Quick
            test_random_source_missing;
        ]
      );
      ( "ws",
        [
          Alcotest.test_case "accept key" `Quick test_accept_key;
          Alcotest.test_case "rfc examples" `Quick test_ws_rfc_examples;
          Alcotest.test_case "lengths" `Quick test_ws_lengths;
          Alcotest.test_case "truncated" `Quick test_ws_truncated;
          Alcotest.test_case "too big" `Quick test_ws_too_big;
          Alcotest.test_case "protocol errors" `Quick test_ws_protocol_errors;
          Alcotest.test_case "control frames" `Quick test_ws_control;
          Alcotest.test_case "round trip" `Quick test_ws_round_trip;
        ]
      );
      ( "assets",
        [
          Alcotest.test_case "resolve" `Quick test_assets;
          Alcotest.test_case "imports and outputs of a split bundle" `Quick
            test_assets_split;
          Alcotest.test_case "errors" `Quick test_assets_errors;
          Alcotest.test_case "is_hashed" `Quick test_is_hashed;
          Alcotest.test_case "content_type" `Quick test_content_type;
          Alcotest.test_case "etag" `Quick test_etag;
        ]
      );
      ( "session_store",
        [
          Alcotest.test_case "basic" `Quick test_store_basic;
          Alcotest.test_case "idle" `Quick test_store_idle;
          Alcotest.test_case "absolute" `Quick test_store_absolute;
          Alcotest.test_case "defaults" `Quick test_store_defaults;
          Alcotest.test_case "expire" `Quick test_store_expire;
          Alcotest.test_case "cap" `Quick test_store_cap;
          Alcotest.test_case "ops" `Quick test_store_ops;
          Alcotest.test_case "domains" `Quick test_store_threads;
          Alcotest.test_case "update" `Quick test_store_update;
          Alcotest.test_case "update is atomic across domains" `Quick
            test_store_update_is_atomic;
        ]
      );
      ("csrf", [ Alcotest.test_case "tokens" `Quick test_csrf ]);
    ]
