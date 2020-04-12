open OUnit2
open Csv

(** Tests that [file] has the expected number of [lines] *)
let num_lines_test (name:string) (file:string) (lines:int)= 
   name >:: fun _ -> 
   assert_equal ~printer:string_of_int lines (Csv.lines (Csv.load file ))

let example_tests : test list =
  [num_lines_test "sample test" "sampleCSV.csv" 4;]


let suite =
  "test suite for A2"
  >::: List.flatten [ example_tests ]

let _ = run_test_tt_main suite