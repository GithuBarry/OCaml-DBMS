open Csv
open Hashtbl
open Sys

let rmv_csv_tl (s:string) = String.sub s 0 (String.length s -4)

let is_csv (s:string) : bool =
  let len = String.length s in
  if len < 4 then false
  else String.sub s (len-4) (4) = ".csv" 

let all_csv_files directory =
  let all_files = directory|>Sys.readdir|>Array.to_list in
  List.fold_left 
    (fun x e -> if is_csv e then e::x else x) [] all_files

(**[csv_array filename] is a string array array representation of [filename]. 
   Throws exception if [filename] does not exist in current directory*)
let csv_array filename = let csv_read = Csv.load filename in
  csv_read|>Csv.to_array

(**[csv_columns filename] is number rows in a [filename] in current directory*)
let csv_columns filename = Csv.(filename|>load|>lines)

(**[to_csv array_csv] is a csv from csv array representation [array_csv]. *)
let to_csv array_csv = let csv1 = Csv.(array_csv|>of_array)
  in csv1

(**[save fn array_csv] saves [array_csv] to filename [fn] in current directory. 
   [fn] does not contain .csv at the end.*)
let save fn array_csv = 
  Csv.save (fn^".csv") (to_csv array_csv)


(**[delete_file dir file] removes [file] from a given [dir].*)
let delete_file dir file = 
  let original_directory = Sys.getcwd () in
  Sys.chdir dir;
  Sys.remove (file^".csv");
  Sys.chdir original_directory

(** [update_csv_files fn hastbl] updates the csv_files in [fn] with data from
    [hastbl].*)
let update_csv_files fn hastbl =
  let original_directory = Sys.getcwd () in
  let csv_with_dir = Hashtbl.find hastbl fn in
  let dir_of_file = fst (csv_with_dir) in
  Sys.chdir dir_of_file;
  save fn (csv_with_dir|>snd);
  Sys.chdir original_directory

(**[is_dir dir] returns true if [dir] is existing dir. [false] otherewise.*)
let is_dir dir = 
  try is_directory dir with x -> false

let rec add_data_to_htbl tbl dir = function
  | [] -> ()
  | h::t -> 
    (Hashtbl.add tbl (rmv_csv_tl h) (dir,(csv_array h)) );
    add_data_to_htbl tbl dir t

(** [csvs_in_hastbl dir tbl] changes first to given [dir] to attain [csv] files.
    Once files are attained, comes back to original directory*)
let csvs_in_hastbl tbl dir  =
  let original_directory = Sys.getcwd () in
  let file_titles = (dir |> all_csv_files) in
  Sys.chdir dir;
  add_data_to_htbl tbl dir file_titles;
  Sys.chdir original_directory

let list_of_dir str_dir =
  let list_with_space = String.split_on_char ',' str_dir in
  List.map String.trim list_with_space

