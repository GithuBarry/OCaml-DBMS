(** AF: A datatable represented as an array of rows, for which each row is 
    represented as array of columns. The first row is the headers of the table. 
    Any empty cells are represented as the empty string. 
    RI: The datatable is "rectangular" , all rows have the same number of 
    columns, and all columns have the same number of rows. All columns have 
    unique names. *)
type t = string array array

let empty = Array.make_matrix 0 0 ""

let is_empty tbl = Array.length tbl = 0

let num_cols tbl = Array.length tbl.(0)

let num_rows tbl = Array.length tbl

let contains_col tbl s = Array.mem s tbl.(0)

let add_col s tbl =
if is_empty tbl then [| [| s |] |] else
if contains_col tbl s then raise (Invalid_argument "Duplicate Column Name")
else
    let append idx row = tbl.(idx) <- Array.append row [| "" |] in
    Array.iteri append tbl;
    tbl.(0).(num_cols tbl - 1) <- s;
    tbl

(** [find_index row s] is the index of [row] which contains [s], or -1 if s is
    not in [row]*)
let find_index s row =
  if not (Array.mem s row) then -1
  else
    let rec idx row s acc =
      if row.(acc) = s then acc else idx row s (acc + 1)
    in
    idx row s 0

(** [remove_index i array] is [array] without the element at index [i] 
    Raises: [Invalid_argument] if i is outside of the range 0 to 
    (Array.length array - 1) *)
let remove_index i array =
  if i < 0 || i > Array.length array - 1 then
    raise (Invalid_argument "index out of bounds")
  else
    let new_row1 = Array.sub array 0 i in
    let new_row2 = Array.sub array (i + 1) (Array.length array - (i + 1)) in
    Array.append new_row1 new_row2

let del_col s tbl =
  if is_empty tbl then tbl
  else if num_cols tbl = 1 && contains_col tbl s then empty
  else
    let index_to_del = find_index s tbl.(0) in
    Array.map (remove_index index_to_del) tbl
    
let add_row tbl =
  if is_empty tbl then raise (Invalid_argument "Can't add row to empty table")
  else Array.append tbl (Array.make_matrix 1 (num_cols tbl) "")

let del_row i tbl = 
    if i = 0 then raise (Invalid_argument "Can't delete first row of table") 
    else remove_index i tbl

let change_cell tbl i j value =
    if i < 0 || i > (num_rows tbl - 1) || j < 0 || j > (num_cols tbl - 1)
    then raise (Invalid_argument "index out of bounds") else
    if i = 0 then raise (Invalid_argument "Can't modify column names") else
    tbl.(i).(j) <- value;
    tbl

(* Format.printf "@[<v>%a@]@." *)
