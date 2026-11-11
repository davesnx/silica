open Silica

let rect x y width height = { Rect.x; y; width; height }
let viewport = rect 0. 0. 800. 600.
let anchor = rect 100. 100. 80. 20.
let popup = rect 0. 0. 200. 150.
let place ?(side = Place.Bottom) ?(align = Place.Start) () =
  { Place.side; align; offset = 4.; padding = 8. }

let check_xy name (expected_x, expected_y) (r : Place.result) =
  Alcotest.(check (pair (float 0.001) (float 0.001)))
    name (expected_x, expected_y) (r.x, r.y)

let side = Alcotest.testable (fun f s -> Fmt.string f (Place.side_name s)) ( = )

let below_when_it_fits () =
  let r = Place.compute (place ()) ~anchor ~popup ~viewport in
  check_xy "under the anchor, at its start" (100., 124.) r;
  Alcotest.check side "bottom" Place.Bottom r.side;
  Alcotest.(check (float 0.001))
    "room below"
    (600. -. 8. -. 124.)
    r.available_height;
  Alcotest.(check (float 0.001)) "room across" (800. -. 16.) r.available_width

let flips_when_there_is_more_room () =
  let anchor = rect 100. 500. 80. 20. in
  let r = Place.compute (place ()) ~anchor ~popup ~viewport in
  Alcotest.check side "top" Place.Top r.side;
  check_xy "above the anchor" (100., 500. -. 4. -. 150.) r;
  Alcotest.(check (float 0.001))
    "room above"
    (500. -. 4. -. 8.)
    r.available_height

let keeps_side_with_more_room () =
  let viewport = rect 0. 0. 800. 200. in
  let anchor = rect 100. 60. 80. 20. in
  let r = Place.compute (place ()) ~anchor ~popup ~viewport in
  Alcotest.check side "neither side fits, below has more room" Place.Bottom
    r.side

let top_flips_down () =
  let anchor = rect 100. 30. 80. 20. in
  let r = Place.compute (place ~side:Place.Top ()) ~anchor ~popup ~viewport in
  Alcotest.check side "bottom" Place.Bottom r.side;
  check_xy "below" (100., 54.) r

let aligns () =
  let anchor = rect 300. 100. 80. 20. in
  let center =
    Place.compute (place ~align:Place.Center ()) ~anchor ~popup ~viewport
  in
  check_xy "center" (240., 124.) center;
  let end_ =
    Place.compute (place ~align:Place.End ()) ~anchor ~popup ~viewport
  in
  check_xy "end" (180., 124.) end_

let shifts_into_viewport () =
  let anchor = rect 700. 100. 80. 20. in
  let r = Place.compute (place ()) ~anchor ~popup ~viewport in
  check_xy "moved left to stay inside" (800. -. 8. -. 200., 124.) r;
  let anchor = rect 2. 100. 80. 20. in
  let r = Place.compute (place ~align:Place.End ()) ~anchor ~popup ~viewport in
  check_xy "moved right to stay inside" (8., 124.) r

let sides_left_and_right () =
  let r = Place.compute (place ~side:Place.Right ()) ~anchor ~popup ~viewport in
  Alcotest.check side "right" Place.Right r.side;
  check_xy "right of the anchor, aligned at its top" (184., 100.) r;
  let r = Place.compute (place ~side:Place.Left ()) ~anchor ~popup ~viewport in
  Alcotest.check side "left has 96 pixels, right has more" Place.Right r.side;
  let anchor = rect 500. 100. 80. 20. in
  let r = Place.compute (place ~side:Place.Left ()) ~anchor ~popup ~viewport in
  Alcotest.check side "left fits" Place.Left r.side;
  check_xy "left of the anchor" (500. -. 4. -. 200., 100.) r

let anchor_place source data =
  match Source.held data source with
  | Some (Action.Anchor { place; _ }) ->
      place
  | _ ->
      Alcotest.fail "not an anchor source"

let anchor_takes_place_from_data () =
  let source =
    Source.anchor
      ~place:(fun side -> place ~side ())
      ~anchor:(Ref.make "trigger") (Ref.make "popup")
  in
  Alcotest.check side "top from the data" Place.Top
    (anchor_place source Place.Top).side;
  Alcotest.check side "left from the data" Place.Left
    (anchor_place source Place.Left).side;
  let fixed = Source.anchor ~anchor:(Ref.make "trigger") (Ref.make "popup") in
  Alcotest.(check bool)
    "default place" true
    (anchor_place fixed () = Place.default)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "silica place"
    [
      ( "place",
        [
          t "below when it fits" below_when_it_fits;
          t "flips when the other side has more room"
            flips_when_there_is_more_room;
          t "keeps the side with more room" keeps_side_with_more_room;
          t "top flips down" top_flips_down;
          t "aligns start, center, end" aligns;
          t "shifts into the viewport" shifts_into_viewport;
          t "left and right" sides_left_and_right;
          t "an anchor source takes its place from the state's data"
            anchor_takes_place_from_data;
        ]
      );
    ]
