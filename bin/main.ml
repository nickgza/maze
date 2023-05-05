(* Compile: ocamlfind ocamlopt -thread -package threads -package graphics -linkpkg ./bin/main.ml -o maze *)
(* Usage: ./maze width *)

open Graphics;;
open Printf;;

Random.self_init ();;

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
let maze : maze_t = Array.init width (fun _ -> Array.init width (fun _ -> {walls = [Bottom; Right; Top; Left];
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

(* Modify cells *)
let add_wall (direction : wall_t) (cell : cell_t) : unit =
  match direction with
  | Bottom -> cell.walls <- Bottom :: cell.walls
  | Right -> cell.walls <- Right :: cell.walls
  | Top -> cell.walls <- Top :: cell.walls
  | Left -> cell.walls <- Left :: cell.walls;;
let remove_wall (direction : wall_t) (cell : cell_t) : unit =
  match direction with
  | Bottom -> cell.walls <- List.filter (fun elem -> elem <> Bottom) cell.walls
  | Right -> cell.walls <- List.filter (fun elem -> elem <> Right) cell.walls
  | Top -> cell.walls <- List.filter (fun elem -> elem <> Top) cell.walls
  | Left -> cell.walls <- List.filter (fun elem -> elem <> Left) cell.walls;;

(* Set the "current" cell *)
let set_current (maze : maze_t) (set_i, set_j : coord_t) : unit =
  Array.iteri (fun i row -> Array.iteri 
  (fun j cell -> cell.current <- if i = set_i && j = set_j then true else false) row) maze;;

let remove_edges (maze : maze_t) (i, j : coord_t) : unit =
  if i = 0 then remove_wall Top maze.(i).(j);
  if i = width - 1 then remove_wall Bottom maze.(i).(j);
  if j = 0 then remove_wall Left maze.(i).(j);
  if j = width - 1 then remove_wall Right maze.(i).(j);

type neighbour_t = {wall : wall_t; coord : coord_t};;

(* DFS for maze generation *)
let dfs (maze : maze_t) (start_i, start_j : coord_t) : unit =
  let rec explore (maze : maze_t) (i : int) (j : int) : unit =
    set_current maze (i, j);
    maze.(i).(j).visited <- true;

    Thread.delay (max 0.005 (-0.2 /. float_of_int width +. 1.2 /. float_of_int width -. 0.02)); (* Don't worry where this comes from *)

    let neighbours = [| Top; Bottom; Left; Right |] in
    for n = 3 downto 1 do
      let k = Random.int (n+1) in
      let tmp = neighbours.(k) in
      neighbours.(k) <- neighbours.(n); neighbours.(n) <- tmp
    done;

    for n = 0 to Array.length neighbours - 1 do
      match neighbours.(n) with
      | Top -> if i > 0 && not maze.(i-1).(j).visited then
        (remove_wall Top maze.(i).(j); remove_wall Bottom maze.(i-1).(j); explore maze (i-1) j)
      | Bottom -> if i < width - 1 && not maze.(i+1).(j).visited then
        (remove_wall Bottom maze.(i).(j); remove_wall Top maze.(i+1).(j); explore maze (i+1) j)
      | Left -> if j > 0 && not maze.(i).(j-1).visited then
        (remove_wall Left maze.(i).(j); remove_wall Right maze.(i).(j-1); explore maze i (j-1))
      | Right -> if j < width - 1 && not maze.(i).(j+1).visited then
        (remove_wall Right maze.(i).(j); remove_wall Left maze.(i).(j+1); explore maze i (j+1))
    done
  in
    explore maze start_i start_j;;

(* Main *)
open_graph (" " ^ string_of_int canvas_width ^ "x" ^ string_of_int canvas_width);;
auto_synchronize false;;
set_line_width (if width > 30 then 1 else 2);;

let start_i, start_j = 0, 0;;
let end_i, end_j = width - 1, width - 1;;

(* New thread for graphics *)
Thread.create draw_maze_wrap maze;;

remove_edges maze (start_i, start_j);;
dfs maze (start_i, start_j);;
remove_edges maze (end_i, end_j);;

read_line ();;
