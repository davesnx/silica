let equal ~show label expected actual =
  if expected <> actual then (
    Printf.eprintf "%s: expected %s, got %s\n" label (show expected)
      (show actual);
    exit 1
  )

let string label expected actual =
  equal ~show:(Printf.sprintf "%S") label expected actual
