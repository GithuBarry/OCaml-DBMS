(** This module represents a datatable *)
open Command


(** The type of a datatable *)
type t = string array array

(** A representation of indexes of some associated table, where true in index i
    indicates that the ith row of the table satisfied the conditions in WHERE.*)
type filter = bool array

(** [empty] is the empty datatable *)
val empty : t

(** [is_empty tbl] is [true] is [tbl] is empty *)
val is_empty : t -> bool

(** [num_cols tbl] is the number of columns in [tbl] *)
val num_cols : t -> int

(** [num_cols tbl] is the number of rows in [tbl]*)
val num_rows : t -> int

(** [get_cols tbl] is the columns of table. *)
val get_cols : t -> string array

(** [contains_col tbl s] is [true] if [tbl] contains a column with header named
    [s] 
    Requires: The first row of [tbl] contains the headers.*)
val contains_col : t -> string -> bool

(** [add_col s tbl] is [tbl] with the column with header named [s] added. Note:
    If [tbl] is empty, also adds a row. 
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
    Raises: [Invalid_argument] if 
    i is outside of the range 1 to ([num_rows] tbl - 1), or 
    j is outside of the range 0 to ([num_cols] tbl - 1) *)
val change_cell : int -> int -> string -> t -> t

(** [get_col_data tbl s] is the data stored in [tbl] in the columns whose names
    are listed in [s] 
    Requires: [tbl] contains all columns named in [s] 
    Raises: [Invalid_Argument] if [tbl] does not contains a column named 
    in [s] *)
val get_cols_data : string list -> t -> t

(** [where conds tbl] is a [filter] representing the rows in [tbl] that satisfy
    the conditions given in [conds] 
    Requires: [tbl] contains all columns referenced in [conds] 
    Raises: [Invalid_Argument] if [tbl] does not contains a column named in 
    [conds] *)
val where : expr_objects -> t -> filter

(** [all_pass tbl] is a [filter] in which all rows in [tbl] are true *)
val all_pass : t -> filter 

(** [select filter tbl] is the rows of [tbl] whose index coorespond to a true 
    entry at the same index in [filter]. 
    Requires: The length of filter = the number of rows originally in [tbl] *)
val select : filter -> t -> t

(** [delete filter tbl] is the rows of [tbl] whose index coorespond to a false 
    entry at the same index in [filter]. 
    Requires: The length of filter = the number of rows originally in [tbl]*)
val delete : filter -> t -> t

(** [update filter set_objs tbl] is [tbl], with each row specified in [filter] 
    modified to match the column, value pairs given in [set_objs].  
    Raises: [Invalid_Argument] if [tbl] does not contain a column named in 
    [set_objs] *) 
val update : filter -> set_objects -> t -> t

(** [insert val_lst col_opt tbl] is [tbl], with a new row containing the values
    in [val_lst] in the columns specified in [col_opt] 
    Raises: [Invalid_Argument] if [tbl] does not contains a column named in 
    [col_opt], or if the lists of values/columns are not the same length. Or, 
    [Failure] if Wildcard command is illegally passed.*)
val insert : value_object list -> column_objects option -> t -> t

(** [order_by col_bool_lst tbl] is [tbl] with each row sorted in accordance 
    with [col_bool_lst].
    Raises: [Invalid_Argument] if [tbl] does not contains a column named in 
    [col_bool_lst] *) 
val order_by : (column_object * bool) list -> t -> t




