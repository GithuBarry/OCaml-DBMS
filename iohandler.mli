
(** [all_csv_files dir] is all csvs [filename] in [dir]. *)
val all_csv_files : string -> string list

(** [csv_array filename] is a string array array representation of [filename] *)
val csv_array : string -> string array array

(**[delete_file dir file] removes [file] from a given [dir]. [file] does not
   have ".csv" at the end, as Hashtable has stored keys with out ".csv"*)
val delete_file: string -> string -> unit

(** [update_csv_files fn hastbl] updates the csv_file [fn] with data from
    [hastbl].*)
val update_csv_files: 
  string -> (string, string*string array array) Hashtbl.t -> unit

(**[is_dir dir] returns true if [dir] is existing dir. [false] otherewise.*)
val is_dir: string -> bool

(** [csvs_in_hastbl database] saves all csvs in [dir] to [database].*)
val csvs_in_hastbl: 
  (string, string*string array array) Hashtbl.t -> string -> unit

(** [list_of_dir str_dir] is a string list of [str_dir]. This function splices
    [str_dir] whenever there is a comma. Then extra whitespaces are ignored. 
    Lastly strings that are spliced are stored individually in string list.*)
val list_of_dir: string -> string list