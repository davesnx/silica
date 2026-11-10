type t = {
  key : string;
  ctrl : bool;
  meta : bool;
  shift : bool;
  alt : bool;
  repeat : bool;
}

let make ?(ctrl = false) ?(meta = false) ?(shift = false) ?(alt = false)
    ?(repeat = false) key =
  { key; ctrl; meta; shift; alt; repeat }

let escape = make "Escape"
let enter = make "Enter"
let tab = make "Tab"
let arrow_up = make "ArrowUp"
let arrow_down = make "ArrowDown"
let equal (a : t) (b : t) = a = b

let to_string t =
  String.concat "+"
    (List.filter_map Fun.id
       [
         ( if t.ctrl then
             Some "Ctrl"
           else
             None
         );
         ( if t.meta then
             Some "Meta"
           else
             None
         );
         ( if t.alt then
             Some "Alt"
           else
             None
         );
         ( if t.shift then
             Some "Shift"
           else
             None
         );
         Some t.key;
       ]
    )
