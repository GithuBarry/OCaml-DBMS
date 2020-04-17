(** This module represents a database with a single table *)

(** The type of a datatable *)
type t (* = (string, string array) Hashtbl.t *)

(** The type of a column header *)
type header = string

(** The type of data contained below a header *)
type contents = string array

(** [add_col tbl s] is [tbl] with a header named [s] added. 
    Requires: [tbl] does not already contain a header named [s]*)
val add_col : t -> header -> t

(** [del_col tbl s] is [tbl] without header [s] *)
val del_col : t -> header -> t
