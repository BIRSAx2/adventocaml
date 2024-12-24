open! Imports
open! Base

module M = struct
  type t = Set.M(Int).t

  let find_sum numbers target =
    Set.to_sequence numbers
    |> Sequence.find ~f:(fun x -> Set.mem numbers (target - x))
    |> Option.map ~f:(fun x -> (x, target - x))

  let find_three_sum numbers target =
    Set.to_sequence numbers
    |> Sequence.find_map ~f:(fun a ->
           match find_sum numbers (target - a) with
           | None -> None
           | Some (b, c) -> Some (a, b, c) )

  let parse inputs =
    inputs
    |> String.split_on_chars ~on:['\n']
    (* Remove empty lines if any. *)
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:Int.of_string
    |> Set.of_list (module Int)

  let part1 numbers =
    match find_sum numbers 2020 with
    | None -> Stdio.print_endline "0"
    | Some (a, b) -> Stdio.printf "%d\n" (a * b)

  let part2 numbers =
    match find_three_sum numbers 2020 with
    | None -> Stdio.print_endline "0"
    | Some (a, b, c) -> Stdio.printf "%d\n" (a * b * c)
end

include M
include Day.Make (M)

let example = "1721\n979\n366\n299\n675\n1456"

let%expect_test _ = run example ; [%expect {|
    514579
    241861950 |}]
