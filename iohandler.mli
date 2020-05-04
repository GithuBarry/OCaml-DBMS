
(** [all_csv_files dir] is all csvs [filename] in [dir]. *)
val all_csv_files : string -> string list

(** [csv_array filename] is a string array array representation of [filename] *)
val csv_array : string -> string array array

(** [save filename data] saves [data] to [filename]. *)
val save : string -> string array array -> unit

(** [csv_array dir database] saves all csvs in [dir] to [database] *)
val csvs_in_hastbl: string -> (string, string array array) Hashtbl.t -> unit

(**[is_dir dir] returns true if [dir] is existing dir. [false] otherewise.*)
val is_dir: string -> bool