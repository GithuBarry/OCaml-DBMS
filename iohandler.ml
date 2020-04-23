open Csv
open Hashtbl
open Sys

let is_csv (s:string) : bool =
  let len = String.length s in
  if len < 4 then false
  else String.sub s (len-4) (4) = ".csv" 

let all_csv_files cur_directory = 
  let all_files = cur_directory|>Sys.readdir|>Array.to_list in
  List.fold_left (fun x e -> if is_csv e then e::x else x) [] all_files

(* [csv_array filename] is a string array array representation of [filename] *)
let csv_array filename = let csv_read = Csv.load filename in
  csv_read|>Csv.to_array

(* [csv_columns filename] is number rows in a [filename]*)
let csv_columns filename = Csv.(filename|>load|>lines)

(* [to_csv array_csv] is a csv from csv array representation [array_csv].*)
let to_csv array_csv = let csv1 = Csv.(array_csv|>of_array)
  in csv1

(* [save fn array_csv] saves [array_csv] to filename [fn]*)
let save fn array_csv = Csv.save fn (to_csv array_csv)

let rec add_es_to_htbl tbl = function
  | [] -> ()
  | h::t -> 
    (Hashtbl.add tbl h (csv_array h) );
    add_es_to_htbl tbl t

let csvs_in_hastbl dir tbl = 
  add_es_to_htbl tbl (dir|>all_csv_files)