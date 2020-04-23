
val all_csv_files : string -> string list

(* [csv_array filename] is a string array array representation of [filename] *)
val csv_array : string -> string array array

val save : string -> string array array -> unit

val csvs_in_hashtbl: string -> (string, string array array) Hashtbl.t -> unit