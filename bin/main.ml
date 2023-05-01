(* Compile: ocamlfind ocamlopt -package graphics ./bin/main.ml -o maze *)

open Graphics;;

let () = if Array.length Sys.argv <> 2
  then begin
    print_string "Usage: ./maze width\n"; exit 1
  end;;

let width = int_of_string Sys.argv.(1);;
let canvas_width = 720;;
let cell_width = float_of_int canvas_width /. float_of_int width;;

type coord_t = int * int;;
type wall_t = Left | Right | Top | Bottom;;
type cell_t = wall_t list;;
type maze_t = cell_t array array;;

let maze = Array.init width (fun _ -> Array.make width [Left; Right; Top; Bottom]);;

let print_walls (cell : cell_t) : unit = List.iter (
  function
  | Left -> print_string "L"
  | Right -> print_string "R"
  | Top -> print_string "T"
  | Bottom -> print_string "B"
  ) cell;
  print_string " ";;

let print_maze (maze : maze_t) : unit = 
  Array.iter (fun row -> Array.iter 
  (fun cell -> print_walls cell) row; print_endline "") maze;;

let draw_maze (maze : maze_t) : unit =
  let calc_coord (i : int) (j : int) : coord_t =
    (int_of_float (float_of_int j *. cell_width), int_of_float (float_of_int (width - i - 1) *. cell_width))
  in
  let draw_cell (_cell : cell_t) (i : int) (j : int) : unit =
    draw_rect (fst (calc_coord i j)) (snd (calc_coord i j)) (int_of_float cell_width) (int_of_float cell_width)
  in
    Array.iteri (fun i row -> Array.iteri
    (fun j cell -> draw_cell cell i j) row) maze;;

print_maze maze;;

open_graph (" " ^ string_of_int canvas_width ^ "x" ^ string_of_int canvas_width);;

draw_maze maze;;

read_line ();;
