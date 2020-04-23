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

let equal_test (name:string) (act) (exp) = 
   name >:: fun _ -> 
   assert_equal (*~printer:string_of_int*) act exp

(* [rows]_x_[columns] *)

let create_1x1 e = e |> add_col "one"
let create_1x2 e = create_1x1 e |> add_col "two"
let create_2x1 e= create_1x1 e |> add_row
let create_2x2 e= create_1x2 e|> add_row
let create_2x3 e= create_2x2 e|> add_col "three"
let create_3x2 e= create_2x2 e|> add_row
let create_3x3 e= create_3x2 e |> add_col "three"
let create_3x4 e= create_3x3 e|> add_col "four"
let create_4x3 e= create_3x3 e|> add_row
let create_4x4 e= create_3x4 e |> add_row
let create_4x5 e= create_4x4 e|> add_col "five"
let create_5x4 e= create_4x4 e|> add_row
let create_5x5 e= create_5x4 e |> add_col "five"
let create_filled_5x5 e = create_5x5 empty |> change_cell 1 0 "10" 
                                           |> change_cell 1 1 "11" 
                                           |> change_cell 1 2 "12" 
                                           |> change_cell 1 3 "13" 
                                           |> change_cell 1 4 "14" 
                                           |> change_cell 2 0 "20" 
                                           |> change_cell 2 1 "21" 
                                           |> change_cell 2 2 "22" 
                                           |> change_cell 2 3 "23" 
                                           |> change_cell 2 4 "24" 
                                           |> change_cell 3 0 "30" 
                                           |> change_cell 3 1 "31" 
                                           |> change_cell 3 2 "32" 
                                           |> change_cell 3 3 "33" 
                                           |> change_cell 3 4 "34" 
                                           |> change_cell 4 0 "40" 
                                           |> change_cell 4 1 "41" 
                                           |> change_cell 4 2 "42" 
                                           |> change_cell 4 3 "43" 
                                           |> change_cell 4 4 "44"

let datatable_tests : test list =  
  [ 
    (*Testing that add_col and add_row are functioning as intended*)
    equal_test "1" [| |] empty;

    equal_test "2" [|[|"one"|]|] (create_1x1 empty);

    equal_test "3" [|[|"one"; "two"|]|] (create_1x2 empty);

    equal_test "4" [|  [|"one"|]; 
                       [| ""  |]  |] (create_2x1 empty);

    equal_test "5" [|  [|"one"; "two"|]; 
                       [| ""  ; ""   |]  |] (create_2x2 empty);

    equal_test "6" [|  [|"one"; "two"; "three"|]; 
                       [| ""  ; ""   ; ""     |]  |] (create_2x3 empty);

    equal_test "7" [|  [|"one"; "two"|]; 
                       [| ""  ; ""   |]; 
                       [| ""  ; ""   |]  |] (create_3x2 empty);

    equal_test "8" [|  [|"one"; "two"; "three"|]; 
                       [| ""  ; ""   ; ""     |]; 
                       [| ""  ; ""   ; ""     |]  |] (create_3x3 empty);

    equal_test "9" [|  [|"one"; "two"; "three"; "four"|]; 
                       [| ""  ; ""   ; ""     ;  ""   |]; 
                       [| ""  ; ""   ; ""     ;  ""   |]  |] (create_3x4 empty);

    equal_test "10" [|  [|"one"; "two"; "three"|]; 
                        [| ""  ; ""   ; ""     |]; 
                        [| ""  ; ""   ; ""     |]; 
                        [| ""  ; ""   ; ""     |]  |] (create_4x3 empty);

    equal_test "11" [|  [|"one"; "two"; "three"; "four"|]; 
                        [| ""  ; ""   ; ""     ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   |]  |] (create_4x4 empty);

    equal_test "12" [|  [|"one"; "two"; "three"; "four"; "five"|]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]  |] (create_4x5 empty);

    equal_test "13" [|  [|"one"; "two"; "three"; "four"|]; 
                        [| ""  ; ""   ; ""     ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   |]  |] (create_5x4 empty);

    equal_test "14" [|  [|"one"; "two"; "three"; "four"; "five"|]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]  |] (create_5x5 empty);

    (*Testing that del_col is functioning as intended. Also a few del_row tests*)    
    equal_test "15" empty (del_col "one" (create_1x1 empty));

    equal_test "16" [|[|"one"|]|] (del_col "wrong name" (create_1x1 empty));

    equal_test "17" [|[|"two"|]|] (del_col "one" (create_1x2 empty));

    equal_test "18" [|[|"one"|]|] (del_col "two" (create_1x2 empty));

    equal_test "19" [|[|"one"|]|] (del_row 1 (create_2x1 empty));

    equal_test "20" [|[|"one"; "two"|]|] (del_row 1 (create_2x2 empty));

    equal_test "21" [|  [|"two" |]; 
                        [| ""   |]; 
                        [| ""   |]  |] (del_col "one" (create_3x2 empty));

    equal_test "22" [|  [|"one" |]; 
                        [| ""   |]; 
                        [| ""   |]  |] (del_col "two" (create_3x2 empty));

    equal_test "23" [|  [|"two"; "three"|]; 
                        [| ""   ; ""    |]; 
                        [| ""   ; ""    |]  |] (del_col "one" (create_3x3 empty));

    equal_test "24" [|  [|"one"; "three"|]; 
                        [| ""   ; ""    |]; 
                        [| ""   ; ""    |]  |] (del_col "two" (create_3x3 empty));

    equal_test "25" [|  [|"one"; "two"|]; 
                        [| ""   ; ""  |]; 
                        [| ""   ; ""  |]  |] (del_col "three" (create_3x3 empty));

    equal_test "26" [|  [|"one"; "two"; "three"|]; 
                        [| ""  ; ""   ; ""     |]; |] (del_row 1 (create_3x3 empty));

    equal_test "27" [|  [|"one"; "two"; "three"|]; 
                        [| ""  ; ""   ; ""     |]; |] (del_row 2 (create_3x3 empty));

    (*Testing that change_cell and  del_row is functioning as intended*)
    equal_test "28" [|  [|"one"; "two"; "three"; "four"; "five"|]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ;"test"; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]  |] 
                        (change_cell 2 1 "test" (create_5x5 empty));

    equal_test "29" [|  [|"one"  ;"two"  ;"three"; "four"; "five"     |]; 
                        [|"test1"; ""    ; ""    ;  ""   ;  ""        |]; 
                        [| ""    ;"test2"; ""    ;  ""   ;  ""        |]; 
                        [| ""    ; ""    ;"test3";  ""   ;  ""        |]; 
                        [| ""    ; ""    ; ""    ;"test4";  "test5"   |]  |] 
                        ( create_5x5 empty |> change_cell 1 0 "test1" 
                                           |> change_cell 2 1 "test2"
                                           |> change_cell 3 2 "test3"
                                           |> change_cell 4 3 "test4"
                                           |> change_cell 4 4 "test5");

    equal_test "30" [|  [|"one" ;"two" ;"three";"four";"five" |]; 
                        [|"10"  ;"11"  ; "12"  ; "13" ; "14"  |]; 
                        [|"20"  ;"21"  ; "22"  ; "23" ; "24"  |]; 
                        [|"30"  ;"31"  ; "32"  ; "33" ; "34"  |]; 
                        [|"40"  ;"41"  ; "42"  ; "43" ; "44"  |];  |] 
                        (create_filled_5x5 empty);
                                           
    equal_test "31" [|  [|"one" ;"two" ;"three";"four";"five" |]; 
                        [|"20"  ;"21"  ; "22"  ; "23" ; "24"  |]; 
                        [|"30"  ;"31"  ; "32"  ; "33" ; "34"  |]; 
                        [|"40"  ;"41"  ; "42"  ; "43" ; "44"  |];  |] 
                        (del_row 1 (create_filled_5x5 empty));

    equal_test "32" [|  [|"one" ;"two" ;"three";"four";"five" |]; 
                        [|"10"  ;"11"  ; "12"  ; "13" ; "14"  |]; 
                        [|"30"  ;"31"  ; "32"  ; "33" ; "34"  |]; 
                        [|"40"  ;"41"  ; "42"  ; "43" ; "44"  |];  |] 
                        (del_row 2 (create_filled_5x5 empty));

    equal_test "33" [|  [|"one" ;"two" ;"three";"four";"five" |]; 
                        [|"10"  ;"11"  ; "12"  ; "13" ; "14"  |]; 
                        [|"20"  ;"21"  ; "22"  ; "23" ; "24"  |]; 
                        [|"40"  ;"41"  ; "42"  ; "43" ; "44"  |];  |] 
                        (del_row 3 (create_filled_5x5 empty));

    equal_test "34" [|  [|"one" ;"two" ;"three";"four";"five" |]; 
                        [|"10"  ;"11"  ; "12"  ; "13" ; "14"  |]; 
                        [|"20"  ;"21"  ; "22"  ; "23" ; "24"  |]; 
                        [|"30"  ;"31"  ; "32"  ; "33" ; "34"  |];  |] 
                        (del_row 4 (create_filled_5x5 empty));

    equal_test "35" [|  [|"one" ;"two" ;"three";"four";"five" |]; 
                        [|"30"  ;"31"  ; "32"  ; "33" ; "34"  |]; 
                        [|"40"  ;"41"  ; "42"  ; "43" ; "44"  |];  |] 
                        (create_filled_5x5 empty |> del_row 1 |> del_row 1);

    (*Testing that get_col_data is working as intended*)
    equal_test "36" [|"one" ; "10" ; "20" ; "30" ; "40" |]  
        (get_col_data "one" (create_filled_5x5 empty));
    
    equal_test "37" [|"two" ; "11" ; "21" ; "31" ; "41" |]  
        (get_col_data "two" (create_filled_5x5 empty));
    
    equal_test "38" [|"three" ; "12" ; "22" ; "32" ; "42" |]  
        (get_col_data "three" (create_filled_5x5 empty));
    
    equal_test "39" [|"four" ; "13" ; "23" ; "33" ; "43" |]  
        (get_col_data "four" (create_filled_5x5 empty));
    
    equal_test "40" [|"five" ; "14" ; "24" ; "34" ; "44" |]  
        (get_col_data "five" (create_filled_5x5 empty));
  ]

(*============================================================================*)
(*============================================================================*)

let suite =
  "test suite for A2"
  >::: List.flatten [ example_tests; datatable_tests ]

let _ = run_test_tt_main suite