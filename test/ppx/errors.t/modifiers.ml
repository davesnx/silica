let a = button ~on_press:Go ~on_press_prevent:true ~children:[] () [@JSX]
let b = input ~on_key_down_rate:(Silica.Rate.Throttle 5) () [@JSX]
