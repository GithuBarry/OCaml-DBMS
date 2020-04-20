open OUnit2
open Csv
open Datatable

(** Tests that [file] has the expected number of [lines] *)
let num_lines_test (name:string) (file:string) (lines:int)= 
   name >:: fun _ -> 
   assert_equal ~printer:string_of_int lines (Csv.lines (Csv.load file ))

let example_tests : test list =
  [num_lines_test "sample test" "sampleCSV.csv" 4;]

(*============================================================================*)
(*============================================================================*)

(* let datatable_to_string array_array = 
  
  array_array |> Array.iter (Array.iter print_endline) *)


let generic_test (name:string) (act:string array array) (exp:string array array) = 
   name >:: fun _ -> 
   assert_equal (*~printer:string_of_int*) act exp

(* [rows]_x_[columns] *)
let zero_x_zero_a = empty
let one_x_one_a = add_col "one" zero_x_zero_a
let one_x_two_a = add_col "two" one_x_one_a
let two_x_two_a = add_row one_x_two_a
let three_x_two_a = add_row two_x_two_a
let three_x_three_a = add_col "three" three_x_two_a


let datatable_tests : test list =  
  [ generic_test "empty" [| |] zero_x_zero_a;
    (* generic_test "add1" [|[|"one"|]|] one_x_one_a; *)

  ]

(*============================================================================*)
(*============================================================================*)

let suite =
  "test suite for A2"
  >::: List.flatten [ example_tests; datatable_tests ]

let _ = run_test_tt_main suite