open OUnit2
open Csv
open Datatable
open Command
open Iohandler

(* Test plan: We automatically tested all top-level functions defined in 
   Datatable and Command. Most testing for iohandler was done manually, as 
   writing OUnit tests that manipulate files across multiple directories is not 
   sensible. The test cases for both Datatable and Command were developed using 
   glass box methods: we designed tests to exhaust every branch of our pattern 
   matching and conditionals, to ensure that they worked as expected. 
   This is why this test suite demonstrates the correctness of the system. We
   tested Command.parse with every type of valid input, and every top-level 
   function with every type of command object. Finally, we did several manual 
   tests with csv's with thousands of rows, and found that all aspects
   of the system (Command, Datatable, Iohandler, Main), are correct.*)

(** [print_array array] prints every element in [array]*)
let print_array array =
  Array.iter print_string array

(** [print_2D_array array] prints [array]  in a matrix format*)
let print_2D_array (array:string array array) = 
  let (acc:string ref) = ref "" in 
  array |> Array.iter (fun x -> 
      Array.iter (fun x ->  acc:= !acc^x; acc:=!acc^"  ") x; 
      acc:= !acc^"/n"); !acc

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
    assert_equal ~printer: print_2D_array act exp

(** [tester name expected result] constructs an OUnit test named [name] 
    that asserts the quality of [expected] with [result]. *)
let tester (name:string) (expected) (result) = 
  name >:: fun _ -> assert_equal expected result

let sampleCSV e = e |> add_col "Name" |> add_col "Age" |> add_col "Loc" 
                  |> add_row |> add_row |> add_row
                  |> change_cell 1 0 "Leo" 
                  |> change_cell 1 1 "24" 
                  |> change_cell 1 2 "USA"
                  |> change_cell 2 0 "Thomas" 
                  |> change_cell 2 1 "20"  
                  |> change_cell 2 2 "USA" 
                  |> change_cell 3 0 "Barry" 
                  |> change_cell 3 1 "20"
                  |> change_cell 3 2 "CHN"    

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
let create_filled_5x5 e = create_5x5 empty 
                          |> change_cell 1 0 "10" 
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
                        [| ""  ; ""   ; ""     ;  ""   |] |] (create_4x4 empty);

    equal_test "12" [|  [|"one"; "two"; "three"; "four"; "five"|]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]  |] 
      (create_4x5 empty);

    equal_test "13" [|  [|"one"; "two"; "three"; "four"|]; 
                        [| ""  ; ""   ; ""     ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   |] |] (create_5x4 empty);

    equal_test "14" [|  [|"one"; "two"; "three"; "four"; "five"|]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]; 
                        [| ""  ; ""   ; ""     ;  ""   ;  ""   |]  |] 
      (create_5x5 empty);

    (*Testing that del_col and del_row are functioning as intended. *)    
    equal_test "15" empty (del_col "one" (create_1x1 empty));

    (* Test that it throws Invalid_argument *)
    (*equal_test "16" [|[|"one"|]|] (del_col "wrong name" (create_1x1 empty));*)

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

    equal_test "23" [| [|"two"; "three"|]; 
                       [| ""   ; ""    |]; 
                       [| ""   ; ""    |] |] (del_col "one" (create_3x3 empty));

    equal_test "24" [| [|"one"; "three"|]; 
                       [| ""   ; ""    |]; 
                       [| ""   ; ""    |] |] (del_col "two" (create_3x3 empty));

    equal_test "25" [| [|"one"; "two"|]; 
                       [| ""   ; ""  |]; 
                       [| ""   ; ""  |] |] (del_col "three" (create_3x3 empty));

    equal_test "26" [|  [|"one"; "two"; "three"|]; 
                        [| ""  ; ""   ; ""     |]; |] 
      (del_row 1 (create_3x3 empty));

    equal_test "27" [|  [|"one"; "two"; "three"|]; 
                        [| ""  ; ""   ; ""     |]; |] 
      (del_row 2 (create_3x3 empty));

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

    (* Testing that get_cols_data is working as intended *)
    equal_test "36" [|  [|"two"; "three"|]; 
                        [| ""   ; ""    |]; 
                        [| ""   ; ""    |]  |] 
      (get_cols_data ["two";"three"] (create_3x3 empty));

    equal_test "37" [|  [|"one"; "three"|]; 
                        [| ""   ; ""    |]; 
                        [| ""   ; ""    |]  |] 
      (get_cols_data ["one";"three"] (create_3x3 empty));

    equal_test "38" [|  [|"one"|]; 
                        [| ""   |]; 
                        [| ""   |]  |] 
      (get_cols_data ["one"] (create_3x3 empty));

    equal_test "39" [|  [|"two"|]; 
                        [| ""   |]; 
                        [| ""   |]  |] 
      (get_cols_data ["two"] (create_3x3 empty));

    equal_test "40" [|  [|"three"|]; 
                        [| ""   |]; 
                        [| ""   |]  |] 
      (get_cols_data ["three"] (create_3x3 empty));

    (*Testing that select is woking as intended*) 
    equal_test "41" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] (sampleCSV empty);

    equal_test "42" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Thomas"; "20" ; "USA"|]; |] 
      (select (where (Expr ("Name", EQ, "Thomas")) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "43" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (select (where (Expr ("Age", GT, "-5")) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "44" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|];  |] 
      (select (where (Expr ("Age", GT, "20")) (sampleCSV empty)) 
         (sampleCSV empty));

    equal_test "45" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (select (where (Expr ("Age", LT, "24")) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "46" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Thomas"; "20" ; "USA"|]; |] 
      (select (where (Binary (AND, Expr ("Age", EQ, "20"), 
                              Expr ("Loc", EQ, "USA"))) (sampleCSV empty) ) (sampleCSV empty));

    equal_test "47" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (select (where (Binary (OR, Expr ("Age", GT, "21"), 
                              (Binary (AND, Expr ("Loc", EQ, "CHN"), Expr ("Name", EQ, "Barry"))))) 
                 (sampleCSV empty) ) (sampleCSV empty));

    equal_test "48" [| [|"Name"  ; "Age"; "Loc"|]; |] 
      (select (where (Binary (AND, Expr ("Name", EQ, "Thomas"),
                              (Binary (AND, Expr ("Name", EQ, "Leo"), Expr ("Name", EQ, "Barry"))))) 
                 (sampleCSV empty) ) (sampleCSV empty));

    equal_test "49"  [| [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Thomas"; "20" ; "USA"|]; |] 
      (select (where (Binary (OR, Expr ("Name", EQ, "Thomas"), 
                              (Binary (AND, Expr ("Name", EQ, "Leo"), Expr ("Name", EQ, "Barry"))))) 
                 (sampleCSV empty) ) (sampleCSV empty));

    equal_test "50"  [| [|"one" ;"two" ;"three";"four";"five"|]; 
                        [|"10"  ;"11"  ; "12"  ; "13" ; "14" |];  
                        [|"30"  ;"31"  ; "32"  ; "33" ; "34" |]; 
                        [|"40"  ;"41"  ; "42"  ; "43" ; "44" |];  |] 
      (select(where (Binary (OR, Expr ("one", GTEQ, "30"),
                             (Binary (AND, Expr ("two", EQ, "11"), Expr ("four", LTEQ, "13"))))) 
                (create_filled_5x5 empty) ) (create_filled_5x5 empty));

    (*Testing that delete is woking as intended*) 
    equal_test "51" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (delete (where (Expr ("Name", EQ, "Thomas")) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "52" [|  [|"Name"  ; "Age"; "Loc"|];  |] 
      (delete (where (Expr ("Age", GT, "-5")) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "53" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (delete (where (Expr ("Age", GT, "20")) (sampleCSV empty)) 
         (sampleCSV empty));

    equal_test "54" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|];  |] 
      (delete (where (Expr ("Age", LT, "24")) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "55" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (delete (where (Binary (AND, Expr ("Age", EQ, "20"), 
                              Expr ("Loc", EQ, "USA"))) (sampleCSV empty) ) (sampleCSV empty));

    equal_test "56" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (delete (where (Binary (AND, Expr ("Age", EQ, "20"), 
                              Expr ("Age", EQ, "24"))) (sampleCSV empty) ) (sampleCSV empty));

    equal_test "57" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Thomas"; "20" ; "USA"|];  |] 
      (delete (where (Binary (OR, Expr ("Age", GT, "21"),
                              (Binary (AND, Expr ("Loc", EQ, "CHN"), Expr ("Name", EQ, "Barry"))))) 
                 (sampleCSV empty) ) (sampleCSV empty));

    equal_test "58" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (delete (where (Binary (AND, Expr ("Name", EQ, "Thomas"),
                              (Binary (AND, Expr ("Name", EQ, "Leo"), Expr ("Name", EQ, "Barry"))))) 
                 (sampleCSV empty) ) (sampleCSV empty));

    equal_test "59" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (delete (where (Binary (OR, Expr ("Name", EQ, "Thomas"),
                              (Binary (AND, Expr ("Name", EQ, "Leo"), Expr ("Name", EQ, "Barry"))))) 
                 (sampleCSV empty) ) (sampleCSV empty));

    equal_test "60" [|  [|"one" ;"two" ;"three";"four";"five" |];  
                        [|"20"  ;"21"  ; "22"  ; "23" ; "24"  |];  |] 
      (delete (where (Binary (OR, Expr ("one", GTEQ, "30"),
                              (Binary (AND, Expr ("two", EQ, "11"), Expr ("four", LTEQ, "13"))))) 
                 (create_filled_5x5 empty) ) (create_filled_5x5 empty));

    (*Testing that update is woking as intended*) 
    equal_test "61" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "100"; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (update (where (Expr ("Name", EQ, "Thomas")) (sampleCSV empty) ) 
         ([("Age","100")]) ((sampleCSV empty)));

    equal_test "62" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Tom"   ; "42" ; "ITH"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (update (where (Expr ("Name", EQ, "Thomas")) (sampleCSV empty) ) 
         ([("Name","Tom");("Age","42");("Loc","ITH"); ]) ((sampleCSV empty)));

    equal_test "63" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Nate"  ; "24" ; "USA"|]; 
                        [|"Nate"  ; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (update (where (Binary (AND, Expr ("Age", GTEQ, "0"), 
                              Expr ("Loc", EQ, "USA"))) (sampleCSV empty) ) 
         ([("Name","Nate")]) ((sampleCSV empty))); 

    equal_test "64" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Nate"  ; "24" ; "USA"|]; 
                        [|"Nate"  ; "20" ; "USA"|]; 
                        [|"Nate" ; "20"  ; "CHN"|]  |] 
      (update (where (Binary (OR, Expr ("Age", EQ, "24"), 
                              Expr ("Age", EQ, "20"))) (sampleCSV empty) ) 
         ([("Name","Nate")]) ((sampleCSV empty))); 

    equal_test "65" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (update (where (Expr ("Age", EQ, "21")) (sampleCSV empty) )  
         ([("Loc","CAN")]) ((sampleCSV empty)));

    (*Testing that insert is woking as intended*)
    equal_test "66" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|];  
                        [|"Nate"  ; "40" ; "USA"|]  |] 
      (insert (["Nate";"40"; "USA"]) (None) 
         (sampleCSV empty) ) ;

    equal_test "67" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|];  
                        [|"Nate"  ; "40" ; "USA"|]  |] 
      (insert (["Nate";"40"; "USA"])  (Some (Columns ["Name";"Age";"Loc"]))  
         (sampleCSV empty) ) ;

    equal_test "68" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|];  
                        [|"Nate"  ; ""   ; "USA"|]  |] 
      (insert (["Nate"; "USA"]) (Some (Columns ["Name";"Loc"])) 
         (sampleCSV empty) ) ;

    equal_test "69" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|];  
                        [|"Nate"  ; ""   ; ""   |]  |] 
      (insert (["Nate"]) (Some (Columns ["Name"])) (sampleCSV empty) ) ;

    equal_test "70" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|];  
                        [| ""     ; ""   ; ""   |]  |] 
      (insert ([]) (Some (Columns [])) (sampleCSV empty) ) ;

    (*Testing that NotExpr is working properly *)
    equal_test "71" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (select (where (NotExpr ("Name", EQ, "Thomas")) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "72" [|  [|"Name"  ; "Age"; "Loc"|]; |] 
      (select (where (NotExpr ("Age", GT, "-5")) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "73" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (select (where (NotExpr ("Age", GT, "20")) (sampleCSV empty)) 
         (sampleCSV empty));

    equal_test "74" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Barry" ; "20" ; "CHN"|]  |] 
      (select (where (Binary (AND, Expr ("Age", EQ, "20"), 
                              NotExpr ("Loc", EQ, "USA"))) (sampleCSV empty) ) 
         (sampleCSV empty));

    equal_test "75" [|  [|"Name"  ; "Age"; "Loc"|]; 
                        [|"Leo"   ; "24" ; "USA"|]; 
                        [|"Thomas"; "20" ; "USA"|];  |] 
      (select (where (Binary (OR, NotExpr ("Age", LT, "21"), 
                              (Binary (AND, Expr ("Age", EQ, "20"), NotExpr ("Name", EQ, "Barry"))))) 
                 (sampleCSV empty) ) (sampleCSV empty));

    (* Testing that order_by is working properly *)
    equal_test "76" [|  [|"Name"  ; "Age"; "Loc"|];
                        [|"Barry" ; "20" ; "CHN"|]; 
                        [|"Leo"   ; "24" ; "USA"|];  
                        [|"Thomas"; "20" ; "USA"|]; |] 
      (order_by [("Name", true);] (sampleCSV empty));

    equal_test "77" [|  [|"Name"  ; "Age"; "Loc"|];
                        [|"Thomas"; "20" ; "USA"|]; 
                        [|"Leo"   ; "24" ; "USA"|];  
                        [|"Barry" ; "20" ; "CHN"|];  |] 
      (order_by [("Name", false);] (sampleCSV empty));  

    equal_test "78" [|  [|"Name"  ; "Age"; "Loc"|];
                        [|"Leo"   ; "24" ; "USA"|];
                        [|"Barry" ; "20" ; "CHN"|];
                        [|"Thomas"; "20" ; "USA"|];  |] 
      (order_by [("Age", true);] (sampleCSV empty)); 

    equal_test "79" [|  [|"Name"  ; "Age"; "Loc"|];
                        [|"Thomas"; "20" ; "USA"|];
                        [|"Barry" ; "20" ; "CHN"|];
                        [|"Leo"   ; "24" ; "USA"|];  |]
      (order_by [("Age", false);] (sampleCSV empty)); 

    equal_test "80" [|  [|"one" ;"two" ;"three";"four";"five"|]; 
                        [|"40"  ;"41"  ; "42"  ; "43" ; "44" |];  
                        [|"30"  ;"31"  ; "32"  ; "33" ; "34" |]; 
                        [|"20"  ;"21"  ; "22"  ; "23" ; "24" |]; 
                        [|"10"  ;"11"  ; "12"  ; "13" ; "14" |];  |]
      (order_by [("three", true);]  
         (create_filled_5x5 empty));                         
  ]

let parser_tests : test list =  
  [
    tester "parser DELETE 1" 
      (Delete, [From "lemon"; Where (Expr ("name", EQ, "lemon"))], [])
      (parse "DELETE FROM lemon WHERE name = lemon");

    tester "parser DELETE 2" 
      (Delete, [From "Roster"], []) (parse "DELETE FROM Roster");

    tester "parser DELETE 3" 
      (Delete, [From "cornell univ roster"; 
                Where (Expr ("net id number", GT, "3"))], [])
      (parse "DELETE FROM cornell univ roster WHERE net id number > 3");

    tester "parse INSERT 1" 
      (InsertInto ("x", Some (Columns ["col1"; "col2"])), 
       [Values ["val1"; "val2"]],[]) 
      (parse "INSERT INTO x (col1,col2) VALUES ( val1 , val2 )");

    tester "parse INSERT 2" 
      (InsertInto ("x", Some (Columns ["col1"; "col2"])), 
       [Values ["val1"; "val2"]],[]) 
      (parse "INSERT INTO x (col1,col2) VALUES (val1,val2)");

    tester "parse INSERT 3" 
      (InsertInto ("x", None), [Values ["val1"; "val2"]],[]) 
      (parse "INSERT INTO x VALUES (val1,val2)");

    tester "parse INSERT 4" 
      (InsertInto ("x", Some (Columns ["col1"; "col2"])), 
       [Values ["val1"; "val2"]],[]) 
      (parse "INSERT INTO x ( col1,col2 )  VALUES ( val1,val2 )");

    tester "parse SELECT 1" 
      (Select Wildcard, [From "X"; Where (Expr ("Y", GT, "0"))], [])
      (parse "SELECT * FROM X WHERE Y > 0");

    tester "parse SELECT 2" (Select Wildcard, [From "X"], [])
      (parse "SELECT * FROM X");

    tester "parse SELECT 3: OR has high priority" 
      (Select Wildcard, [From "medicine"; Where (
           Binary (OR,Binary 
                     (AND, Expr ("status", EQ, "valid"), Expr ("medicine", EQ, "valid")),
                   Expr ("emergency auth", EQ, "true")))],
       [])
      (parse "SELECT * FROM medicine WHERE status = valid AND medicine = valid OR emergency auth = true");

    tester "parse SELECT 4" 
      (Select Wildcard,
       [From "table name"; Where (Binary (OR,
                                          Binary (AND, Expr ("status", EQ, "valid"), Expr ("medicine", EQ, "valid")),
                                          Binary (AND, Expr ("emergency auth", EQ, "true"),
                                                  Expr ("is banned", EQ, "false"))))],
       [OrderBy [("Date", true); ("No", false)]])
      (parse "SELECT * FROM table name WHERE status = valid AND medicine = valid OR emergency auth = true AND is banned = false ORDER BY Date DESC, No ASC");

    tester "parse SELECT 5 with NOT" 
      (Select Wildcard,
       [From "table name";Where(Binary (OR,
                                        Binary (AND, Expr ("status", EQ, "valid"), Expr ("medicine", EQ, "valid")),
                                        Binary (AND, Expr ("emergency auth", EQ, "true"),
                                                NotExpr ("is banned", EQ, "false"))))],
       [OrderBy [("Date", true); ("No", false)]])
      (parse "SELECT * FROM table name WHERE status = valid AND medicine = valid OR emergency auth = true AND NOT is banned = false ORDER BY Date DESC, No ASC");

    tester "Parse UPDATE 1" (Update "table", [Where 
                                                (Expr ("age", GT, "21")); Set [("name", "adult"); ("drinking", "ok")]],[]) 
      (parse "UPDATE table SET name = adult, drinking = ok WHERE age > 21");

    tester "Parse UPDATE 2" (Update "table", [Where 
                                                (Binary (AND, Expr ("age", GT, "21"), Expr ("status", EQ, "active")));
                                              Set [("name", "adult"); ("drinking", "ok")]], []) 
      (parse "UPDATE table SET name = adult, drinking = ok WHERE age > 21 AND status = active");
  ]

let iohandler_test = [
  tester "testing list_of_dir" ["./leo";"./examples"] 
    (Iohandler.list_of_dir "  ./leo   ,  ./examples");
  tester "testing list_of_dir" [""] (Iohandler.list_of_dir "");
  tester "testing list_of_dir" ["./leo"] (Iohandler.list_of_dir " ./leo ");
]

let suite =
  "test suite for A2"
  >::: List.flatten [ example_tests; datatable_tests; parser_tests; iohandler_test]

let _ = run_test_tt_main suite