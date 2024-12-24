open! Imports
open! Base

module M = struct
  type t = (Base.int * Base.int * string * string) Base.list

  let parse inputs =
    inputs
    |> String.split_on_chars ~on:['\n']
    |> List.map ~f:(fun line ->
           let re = Re2.create_exn "(\\d+)-(\\d+) (\\w): (\\w+)" in
           let matches = Re2.find_submatches_exn re line in
           let minimum = Int.of_string (Option.value_exn matches.(1)) in
           let maximum = Int.of_string (Option.value_exn matches.(2)) in
           let ch = Option.value_exn matches.(3) in
           let pw = Option.value_exn matches.(4) in
           (minimum, maximum, ch, pw) )

  let is_valid_password (min, max, char, pwd) =
    let count = String.count pwd ~f:(fun c -> Char.equal c char.[0]) in
    count >= min && count <= max

  let part1 input =
    input
    |> List.filter ~f:is_valid_password
    |> List.length |> Stdio.printf "%d\n"

  let is_valid_password_with_position (min, max, char, pwd) =
    let valid_position_1 = Char.equal (String.get pwd (min - 1)) char.[0] in
    let valid_position_2 = Char.equal (String.get pwd (max - 1)) char.[0] in
    Bool.(valid_position_1 <> valid_position_2)

  let part2 inputs =
    inputs
    |> List.filter ~f:is_valid_password_with_position
    |> List.length |> Stdio.printf "%d\n"
end

include M
include Day.Make (M)

let example = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"

let%expect_test _ = run example ; [%expect {|
    2
    1 |}]
