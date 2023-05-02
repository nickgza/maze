(* Compile: ocamlfind ocamlopt -thread -package threads -package graphics -linkpkg ./bin/main.ml -o maze *)

open Graphics;;

let purple = rgb 100 0 100;;
let green = rgb 0 200 0;;

(* Check argv *)
let () = if Array.length Sys.argv <> 2
  then begin
    print_string "Usage: ./maze width\n"; exit 1
  end;;

(* Calculate widths *)
let width = int_of_string Sys.argv.(1);;
let canvas_width = 720;;
let cell_width = float_of_int canvas_width /. float_of_int width;;

(* Define types *)
type coord_t = int * int;;
type wall_t = Bottom | Right | Top | Left;;
type cell_t = {mutable walls : wall_t list; mutable visited : bool; mutable current : bool};;
type maze_t = cell_t array array;;

(* Initialize maze *)
let maze = Array.init width (fun _ -> Array.init width (fun _ -> {walls = [Bottom; Right; Top; Left];
                                                                  visited = false;
                                                                  current = false}));;

(* Prints the maze to console *)
let print_maze (maze : maze_t) : unit = 
  let print_walls (cell : cell_t) : unit = List.iter (
    function
    | Bottom -> print_string "B"
    | Right -> print_string "R"
    | Top -> print_string "T"
    | Left -> print_string "L"
    ) cell.walls;
    print_string " "
  in
    Array.iter (fun row -> Array.iter 
    (fun cell -> print_walls cell) row; print_endline "") maze;;

(* Draw the maze *)
let draw_maze (maze : maze_t) : unit =
  let calc_coord (i : int) (j : int) : coord_t =
    (int_of_float (float_of_int j *. cell_width), int_of_float (float_of_int (width - i - 1) *. cell_width))
  in
  let draw_cell (cell : cell_t) (i : int) (j : int) : unit =
    let x, y = calc_coord i j in
    let int_cell_width = int_of_float cell_width in
      if cell.current then set_color green else if cell.visited then set_color purple else set_color black;
      fill_rect x y int_cell_width int_cell_width;
      set_color white;
      moveto x y;
      if List.mem Bottom cell.walls then rlineto int_cell_width 0 else rmoveto int_cell_width 0;
      if List.mem Right cell.walls then rlineto 0 int_cell_width else rmoveto 0 int_cell_width;
      if List.mem Top cell.walls then rlineto (-int_cell_width) 0 else rmoveto (-int_cell_width) 0 ;
      if List.mem Left cell.walls then rlineto 0 (-int_cell_width) else rmoveto 0 (-int_cell_width) ;
  in
    Array.iteri (fun i row -> Array.iteri
    (fun j cell -> draw_cell cell i j) row) maze;;

(* Wrapper for draw_maze for graphics thread *)
let rec draw_maze_wrap (maze : maze_t) : unit =
  draw_maze maze;
  Thread.delay (1. /. 30.);
  synchronize ();
  draw_maze_wrap maze;;

(* Main *)
print_maze maze;;

maze.(0).(0).visited <- true;;
maze.(1).(0).current <- true;;
  
open_graph (" " ^ string_of_int canvas_width ^ "x" ^ string_of_int canvas_width);;
auto_synchronize false;;
set_line_width 2;;

Thread.create draw_maze_wrap maze;;

read_line ();;
