open Raylib;;
open Smoothlife;;
let width = 600 and height = 600 and title = "SmoothLife";;

let display_grid t screen_width screen_height =
  let w = Array.length t
  and h = Array.length t.(0) in
  let step_x = screen_width / w
  and step_y = screen_height / h in
  for x = 0 to w - 1 do
    for y = 0 to h -1 do
      let v = t.(x).(y) in
      let vec = Raylib.Vector4.create v v v 1.0 in
      let col = Raylib.color_from_normalized vec in
      Raylib.draw_rectangle (x*step_x) (y*step_y) step_x step_y col;
    done;
  done;;

let setup () =
  init_window width height title;;

let t = ref (get_random_grid 100 100);;
let rec loop () =
  if window_should_close () then close_window ()
  else(
    begin_drawing ();
    display_grid !t width height;
    end_drawing ();
    t := update_grid !t;
    loop ()
  );;

let () = setup () |> loop;;

