(** This module represents a datatable *)

(** The type of a datatable *)
type t =  string array array 

(** [empty] is the empty datatable *)
val empty : t

(** [is_empty tbl] is [true] is [tbl] is empty *)
val is_empty : t -> bool

(** [num_cols tbl] is the number of columns in [tbl] *)
val num_cols : t -> int

(** [num_cols tbl] is the number of rows in [tbl]*)
val num_rows: t -> int

val get_cols: t -> string array

(** [contains_col tbl s] is [true] if [tbl] contains a column with header named 
    [s]
    Requires: The first row of [tbl] contains the headers.*)
val contains_col : t -> string -> bool

(** [add_col s tbl] is [tbl] with the column with header named [s] added. 
    Note: If [tbl] is empty, also adds a row. 
    Raises: [Invalid_Argument] if [tbl] already contain a header named [s] *)
val add_col : string -> t -> t

(** [del_col s tbl] is [tbl] without the column with header named [s].
    Requires: [tbl] contains a column named [s] 
    Raises: [Invalid_Argument] if [tbl] does not contains a column named [s] *)
val del_col : string -> t -> t  

(** [add_row tbl] is [tbl] with an empty row added. 
    Requires: [tbl] is not empty
    Raises: [Invalid_Argument] if [tbl] is empty.*)
val add_row : t -> t

(** [del_row i tbl] is [tbl] without the [i]th row. Cannot delete the top row.
    Raises: [Invalid_argument] if i is outside of the range 1 to 
    ([num_rows] tbl - 1)*)
val del_row : int -> t -> t

(** [change_cell tbl i j v] is [tbl] with the [i]th row and [j]th column set to
    [v]. Cannot change column name.
    Raises: [Invalid_argument] if i is outside of the range 1 to 
    ([num_rows] tbl - 1), or j is outside of the range 0 to ([num_cols] tbl - 1)*)
val change_cell : int -> int -> string -> t -> t

(** [get_col_data tbl s] is the data stored in [tbl] in the columns whose names 
    are listed in [s]
    Requires: [tbl] contains all columns named in [s] 
    Raises: [Invalid_Argument] if [tbl] does not contains a column named in [s] *)
val get_cols_data : string list -> t ->  string array array
