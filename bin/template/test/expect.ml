let equal ~show label expected actual =
  if expected <> actual then (
    Printf.eprintf "%s: expected %s, got %s\n" label (show expected)
      (show actual);
    exit 1
  )

let lines text =
  match List.rev (String.split_on_char '\n' text) with
  | "" :: rest ->
      List.rev rest
  | all ->
      List.rev all

let line_diff expected actual =
  let before = Array.of_list (lines expected) in
  let after = Array.of_list (lines actual) in
  let rows = Array.length before and columns = Array.length after in
  let common = Array.make_matrix (rows + 1) (columns + 1) 0 in
  for row = rows - 1 downto 0 do
    for column = columns - 1 downto 0 do
      common.(row).(column) <-
        ( if before.(row) = after.(column) then
            common.(row + 1).(column + 1) + 1
          else
            max common.(row + 1).(column) common.(row).(column + 1)
        )
    done
  done;
  let rec walk row column acc =
    if row < rows && column < columns && before.(row) = after.(column) then
      walk (row + 1) (column + 1) (("  " ^ before.(row)) :: acc)
    else if
      row < rows
      && (column = columns
         || common.(row + 1).(column) >= common.(row).(column + 1)
         )
    then
      walk (row + 1) column (("- " ^ before.(row)) :: acc)
    else if column < columns then
      walk row (column + 1) (("+ " ^ after.(column)) :: acc)
    else
      List.rev acc
  in
  walk 0 0 []

let string label expected actual =
  if String.contains expected '\n' || String.contains actual '\n' then (
    if expected <> actual then (
      Printf.eprintf "%s: the text differs (- expected, + got):\n%s\n" label
        (String.concat "\n" (line_diff expected actual));
      exit 1
    )
  ) else
    equal ~show:(Printf.sprintf "%S") label expected actual
