open! Imports
open! Base

module M = struct
  type t = (int * int, int) Base.Hashtbl.t * (int * int)

  let parse (args : string) : t =
    let lines =
      String.split ~on:'\n' args
      |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    let tree_map = Hashtbl.Poly.create ~size:32 () in
    List.iteri lines ~f:(fun y line ->
        let chars = String.to_list line in
        List.iteri chars ~f:(fun x c ->
            let data = if Char.equal c '#' then 1 else 0 in
            Hashtbl.set tree_map ~key:(x, y) ~data ) ) ;
    let compare_coord (x1, y1) (x2, y2) =
      match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | c -> c
    in
    let bottom_right =
      Hashtbl.fold tree_map ~init:(0, 0)
        ~f:(fun ~key:(x, y) ~data:_ (best_x, best_y) ->
          if compare_coord (x, y) (best_x, best_y) > 0 then (x, y)
          else (best_x, best_y) )
    in
    (tree_map, bottom_right)

  let is_tree ((tree_map, (max_x, _max_y)) : t) (x, y) : int =
    let x_mod = x % (max_x + 1) in
    Hashtbl.find tree_map (x_mod, y) |> Option.value ~default:0

  let traverse_count_tree ((_, (_max_x, max_y)) as t) (slope_x, slope_y) :
      int =
    let range = List.range 0 max_y in
    let final_count, _ =
      List.fold range
        ~init:(0, (slope_x, slope_y))
        ~f:(fun (count, (x, y)) _ ->
          let count = count + is_tree t (x, y) in
          let x' = x + slope_x in
          let y' = y + slope_y in
          (count, (x', y')) )
    in
    final_count

  let part1 input =
    let trees_hit = traverse_count_tree input (3, 1) in
    Stdio.printf "%d\n" trees_hit

  (* part2: parse input, then multiply results from several slopes. *)
  let part2 input =
    let slopes = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] in
    let product =
      List.map slopes ~f:(traverse_count_tree input)
      |> List.fold ~init:1 ~f:( * )
    in
    Stdio.printf "%d\n" product
end

include M
include Day.Make (M)

let example =
  "..##.......\n\
   #...#...#..\n\
   .#....#..#.\n\
   ..#.#...#.#\n\
   .#...##..#.\n\
   ..#.##.....\n\
   .#.#.#....#\n\
   .#........#\n\
   #.##...#...\n\
   #...##....#\n\
   .#..#...#.#"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
7
336
|}]
