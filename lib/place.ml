type side = Top | Bottom | Left | Right
type align = Start | Center | End
type t = { side : side; align : align; offset : float; padding : float }

let default = { side = Bottom; align = Start; offset = 4.; padding = 8. }

type result = {
  x : float;
  y : float;
  side : side;
  align : align;
  available_width : float;
  available_height : float;
}

let opposite = function
  | Top ->
      Bottom
  | Bottom ->
      Top
  | Left ->
      Right
  | Right ->
      Left

let compute (t : t) ~(anchor : Rect.t) ~(popup : Rect.t) ~(viewport : Rect.t) =
  let low_x = viewport.x +. t.padding
  and high_x = viewport.x +. viewport.width -. t.padding
  and low_y = viewport.y +. t.padding
  and high_y = viewport.y +. viewport.height -. t.padding in
  let room = function
    | Bottom ->
        high_y -. (anchor.y +. anchor.height +. t.offset)
    | Top ->
        anchor.y -. t.offset -. low_y
    | Right ->
        high_x -. (anchor.x +. anchor.width +. t.offset)
    | Left ->
        anchor.x -. t.offset -. low_x
  in
  let size = function
    | Top | Bottom ->
        popup.height
    | Left | Right ->
        popup.width
  in
  let side =
    let other = opposite t.side in
    if room t.side < size t.side && room other > room t.side then
      other
    else
      t.side
  in
  let along start length popup_length low high =
    let wanted =
      match t.align with
      | Start ->
          start
      | Center ->
          start +. ((length -. popup_length) /. 2.)
      | End ->
          start +. length -. popup_length
    in
    Float.max low (Float.min wanted (high -. popup_length))
  in
  let x, y =
    match side with
    | Bottom ->
        ( along anchor.x anchor.width popup.width low_x high_x,
          anchor.y +. anchor.height +. t.offset
        )
    | Top ->
        ( along anchor.x anchor.width popup.width low_x high_x,
          anchor.y -. t.offset -. popup.height
        )
    | Right ->
        ( anchor.x +. anchor.width +. t.offset,
          along anchor.y anchor.height popup.height low_y high_y
        )
    | Left ->
        ( anchor.x -. t.offset -. popup.width,
          along anchor.y anchor.height popup.height low_y high_y
        )
  in
  let across =
    match side with
    | Top | Bottom ->
        high_x -. low_x
    | Left | Right ->
        high_y -. low_y
  in
  let main = room side in
  let available_width, available_height =
    match side with
    | Top | Bottom ->
        (across, main)
    | Left | Right ->
        (main, across)
  in
  { x; y; side; align = t.align; available_width; available_height }

let side_name = function
  | Top ->
      "top"
  | Bottom ->
      "bottom"
  | Left ->
      "left"
  | Right ->
      "right"

let align_name = function Start -> "start" | Center -> "center" | End -> "end"
