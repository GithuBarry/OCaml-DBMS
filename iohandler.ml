open Csv

(* [csv_array filename] is a string array array representation of [filename] *)
let csv_array filename = let csv_read = Csv.load filename in
  csv_read|>Csv.transpose|>Csv.to_array

(* [csv_columns filename] is number rows in a [filename]*)
let csv_columns filename = Csv.(filename|>load|>lines)

(* [to_csv array_csv] is a csv from csv array representation [array_csv].*)
let to_csv array_csv = let csv1 = Csv.(array_csv|>of_array|>transpose)
  in csv1

(* [save fn csv] saves csv data to filename [fn]*)
let save fn csv = Csv.save fn csv 

(* Some questions to talk about
   1. Let us say we draw up multiple files. Some parsing should be done in main.
   We need to store a lot of [string array array], because we will be using 
   multiple csv files. How are we going to store this [string array array]*)