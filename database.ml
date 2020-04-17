(** [t] represents a database as a hashtable with the keys as strings that
    represent column headers, and the values as string arrays, which represent
    the data below each header *)
type t = (string, string array) Hashtbl.t

type header = string

type contents = string array

let add_col tbl s = failwith "unimplemented"

let del_col tbl s = failwith "unimplemented"


