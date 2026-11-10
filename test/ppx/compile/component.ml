type event =
  | Pressed
  | Pressed_twice
  | Submitted
  | Focused
  | Blurred
  | Typed of string
  | Key of Silica.Key.t
  | Entered
  | Left
  | Icon_failed

let input_ref = Silica.Ref.make "name"

module Label = struct
  let make ~text ~children:_ () = div ~children:[ text ] () [@JSX]
end

let view ?(hint : string option) ?(rate : Silica.Rate.t option) () :
    event Silica.Node.t =
  div ~class_:"form" ~style:"color: red" ~tabindex:1 ~hidden:false
    ~children:
      [
        (button ~on_press:Pressed ~on_double_press:Pressed_twice
           ~children:[ "go" ] () [@JSX]
        );
        (form ~on_submit:Submitted
           ~children:
             [
               (input ~ref:input_ref ~on_focus:Focused ~on_blur:Blurred
                  ~on_input:(fun s -> Typed s)
                  ?placeholder:hint () [@JSX]
               );
               (Label.make
                  ~text:(span ~children:[ "x" ] () [@JSX])
                  ~children:[ "a"; "b" ] () [@JSX]
               );
             ]
           () [@JSX]
        );
        (input
           ~on_key_down:(fun key -> Some (Key key))
           ~on_key_up:(fun key -> Some (Key key))
           () [@JSX]
        );
        (input
           ~on_key_down:(fun key -> Some (Key key))
           ~on_key_down_prevent:true ~on_key_down_rate:(Silica.Rate.Throttle 50)
           ~on_input:(fun s -> Typed s)
           ~on_input_rate:(Silica.Rate.Debounce 30)
           ?on_key_up_prevent:(Some false) ?on_key_up_rate:rate
           ~on_key_up:(fun key -> Some (Key key))
           () [@JSX]
        );
        (button ~on_press:Pressed ~on_press_rate:(Silica.Rate.Throttle 100)
           ?on_focus:(Some Focused) ?on_focus_rate:rate ~children:[ "rated" ] ()
         [@JSX]
        );
        (div
           ~on_pointer_enter:(fun _ -> Some Entered)
           ~on_pointer_leave:(fun _ -> Some Left)
           ~children:[ "hover" ] () [@JSX]
        );
        (svg ~viewBox:"0 0 1 1"
           ~children:
             [
               (use ~xlinkHref:"#a" ~children:[] () [@JSX]);
               (path ~clipPath:"url(#c)" ~children:[] () [@JSX]);
             ]
           () [@JSX]
        );
        [ (p ~children:[ "frag" ] () [@JSX]); "text" ] [@JSX];
      ]
    () [@JSX]

let fallback (error : Silica.Node.error) ~reset : event Silica.Node.t =
  div
    ~children:
      [
        Silica.Node.text error.message;
        (button ~on_reset:reset ~children:[ "retry" ] () [@JSX]);
      ]
    () [@JSX]
