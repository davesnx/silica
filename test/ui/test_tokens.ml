let palette =
  [
    "white";
    "black";
    "slate";
    "gray";
    "zinc";
    "neutral";
    "stone";
    "red";
    "orange";
    "amber";
    "yellow";
    "lime";
    "green";
    "emerald";
    "teal";
    "cyan";
    "sky";
    "blue";
    "indigo";
    "violet";
    "purple";
    "fuchsia";
    "pink";
    "rose";
  ]

let colour_utilities =
  [
    "bg";
    "text";
    "border";
    "ring";
    "outline";
    "fill";
    "stroke";
    "divide";
    "from";
    "via";
    "to";
    "shadow";
    "accent";
    "caret";
    "decoration";
    "placeholder";
  ]

let fixed_shadows =
  [ "shadow"; "shadow-sm"; "shadow-md"; "shadow-lg"; "shadow-xl"; "shadow-2xl" ]

let utility word =
  match String.rindex_opt word ':' with
  | Some i ->
      String.sub word (i + 1) (String.length word - i - 1)
  | None ->
      word

let palette_colour word =
  let u = utility word in
  List.exists
    (fun prefix ->
      List.exists
        (fun colour ->
          let base = prefix ^ "-" ^ colour in
          u = base
          || String.starts_with ~prefix:(base ^ "-") u
          || String.starts_with ~prefix:(base ^ "/") u
        )
        palette
    )
    colour_utilities

let words text =
  String.split_on_char '"' text
  |> List.concat_map (String.split_on_char ' ')
  |> List.concat_map (String.split_on_char '\n')
  |> List.map String.trim
  |> List.filter (( <> ) "")

let read path = In_channel.with_open_bin path In_channel.input_all

let sources dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f ->
      Filename.check_suffix f ".ml" || Filename.check_suffix f ".mlx"
  )
  |> List.map (Filename.concat dir)

let arbitrary_colour word =
  List.exists
    (fun opening ->
      let n = String.length opening and m = String.length word in
      let rec at i =
        i + n <= m && (String.sub word i n = opening || at (i + 1))
      in
      at 0
    )
    [ "-[#"; "-[rgb"; "-[hsl"; "-[oklch"; "-[color:" ]

let css_colour line =
  let line = String.lowercase_ascii line in
  let has sub =
    let n = String.length sub and m = String.length line in
    let rec at i = i + n <= m && (String.sub line i n = sub || at (i + 1)) in
    at 0
  in
  has "rgb(" || has "hsl(" || has "oklch("
  || Str.string_match (Str.regexp ".*#[0-9a-f][0-9a-f][0-9a-f]") line 0

let check_css path () =
  let found =
    In_channel.with_open_bin path In_channel.input_lines
    |> List.filter css_colour
    |> List.map (fun line -> path ^ ": " ^ line)
  in
  Alcotest.(check (list string)) "no colour outside the tokens" [] found

let bad ~strict path =
  words (read path)
  |> List.filter (fun w ->
      String.starts_with ~prefix:"dark:" w
      || arbitrary_colour w || palette_colour w
      || (strict && List.mem (utility w) fixed_shadows)
  )
  |> List.map (fun w -> path ^ ": " ^ w)

let check_files ~strict files () =
  let found = List.concat_map (bad ~strict) files in
  Alcotest.(check (list string)) "no colour outside the tokens" [] found

let check_dir ~strict dir = check_files ~strict (sources dir)

let finds_a_palette_class () =
  Alcotest.(check bool) "bg-white" true (palette_colour "bg-white");
  Alcotest.(check bool)
    "hover:text-gray-500" true
    (palette_colour "hover:text-gray-500");
  Alcotest.(check bool) "bg-black/40" true (palette_colour "bg-black/40");
  Alcotest.(check bool) "bg-ui-surface" false (palette_colour "bg-ui-surface");
  Alcotest.(check bool) "text-sm" false (palette_colour "text-sm");
  Alcotest.(check bool) "bg-[#fff]" true (arbitrary_colour "bg-[#fff]");
  Alcotest.(check bool)
    "text-[rgb(0_0_0)]" true
    (arbitrary_colour "text-[rgb(0_0_0)]");
  Alcotest.(check bool) "text-[13px]" false (arbitrary_colour "text-[13px]");
  Alcotest.(check bool) "css hex" true (css_colour "  color: #fff;");
  Alcotest.(check bool) "css var" false (css_colour "  color: var(--ui-text);")

let () =
  Alcotest.run "silica-ui tokens"
    [
      ( "tokens only",
        [
          Alcotest.test_case "the detector" `Quick finds_a_palette_class;
          Alcotest.test_case "silica-ui default styles" `Quick
            (check_files ~strict:true
               [ "../../ui/style.ml"; "../../ui/silica-ui.css" ]
            );
          Alcotest.test_case "silica-ui" `Quick
            (check_dir ~strict:false "../../ui");
          Alcotest.test_case "site" `Quick
            (check_dir ~strict:true "../../site/app/shared");
          Alcotest.test_case "site stories" `Quick
            (check_dir ~strict:true "../../site/stories");
          Alcotest.test_case "site CSS" `Quick
            (check_css "../../site/client/input.css");
          Alcotest.test_case "mini Linear" `Quick
            (check_dir ~strict:true "../../linear/app/shared");
        ]
      );
    ]
