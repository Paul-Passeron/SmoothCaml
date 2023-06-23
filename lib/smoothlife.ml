type state = float;;
type grid = state array array;;

let get_empty_grid (w: int) (h: int): grid =
  Array.make_matrix w h 0.0;;

let get_random_grid w h =
  let wi = (2*w)/5
  and he = (2*h)/5 in
  let res = get_empty_grid w h in
  for dx = 0 to wi - 1 do
    for dy = 0 to he -1 do
      let x = dx + w/2 - wi/2
      and y = dy + h/2 - he/2 in
      res.(x).(y) <- Random.float 1.0;
    done;
  done;
  res;;

let r_a = 11;;
let r_i = r_a/3;;
let alpha_n = 0.028;;
let alpha_m = 0.147;;
let b_1 = 0.278;;
let b_2 = 0.365;;
let d_1 = 0.267;;
let d_2 = 0.445;;
let dt = 0.05;;

let emod a b = (a mod b + b) mod b;;


let sigma_1 x a alpha = 1.0 /. (1.0 +. exp(-.(x-.a)*.4.0/.alpha));;

let sigma_2 x a b alpha = (sigma_1 x a alpha) *. (1.0 -. (sigma_1 x b alpha));;

let sigma_m x y m = x *. (1.0 -. (sigma_1 m 0.5 alpha_m)) +. y *. (sigma_1 m 0.5 alpha_m);;

(*On suppose pour l'instant les deux alpha égaux manque de compréhension de l'article)*)

let s n m = sigma_2 n (sigma_m b_1 d_1 m) (sigma_m b_2 d_2 m) alpha_n;;

let get_n_m t x y =
  let w = Array.length t
  and h = Array.length t.(0) in
  let n = ref 0.0 and n_tot = ref 0.0
  and m = ref 0.0 and m_tot = ref 0.0 in
  for dx = -r_a to r_a do
    for dy = -r_a to r_a do
      let i = emod (x+dx) w
      and j = emod (y+dy) h in
      let dist = (dx*dx+dy*dy) in
      if dist <= r_i*r_i then (
        m_tot := !m_tot +. 1.0;
        m := !m +. t.(i).(j);
      )
      else (
        if dist <= r_a*r_a then (
          n_tot := !n_tot +. 1.0;
          n := !n +. t.(i).(j);
        )
      )
    done;
  done;
  (!n /. !n_tot, !m /. !m_tot);;

let update_grid (t: grid): grid= 
  let w = Array.length t
  and h = Array.length t.(0) in
  let res = get_empty_grid w h in
  for x = 0 to w - 1 do
    for y = 0 to h -1 do
      let n, m = get_n_m t x y in
      (*print_float n; print_string "  "; print_float m; print_endline "";*)
      let s_xy = 2.0 *. (s n m) -. 1.0 in
      let v = ref (t.(x).(y) +. dt *. s_xy) in
      if !v > 1.0 then res.(x).(y) <- 1.0
      else (
        if !v < 0.0 then res.(x).(y) <- 0.0
        else res.(x).(y) <- !v;
      )
    done;
  done;
  res;;
 
