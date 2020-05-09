open Command
(** AF: A datatable represented as an array of rows, for which each row is 
    represented as array of columns. The first row is the header of the table. 
    Any empty cells are represented as the empty string. 
    RI: The datatable is "rectangular": all rows have the same number of 
    columns, and all columns have the same number of rows. All columns have 
    unique names. *)
type t = string array array

type filter = bool array

let empty = Array.make_matrix 0 0 ""

let is_empty tbl = Array.length tbl = 0

let num_cols tbl = Array.length tbl.(0)

let num_rows tbl = Array.length tbl

let get_cols tbl = tbl.(0)

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
  if is_empty tbl then raise (Invalid_argument "table is empty" )
  else if not (contains_col tbl s) then
    raise (Invalid_argument "Invalid column name")
  else if num_cols tbl = 1 then empty
  else
    let index_to_del = find_index s tbl.(0) in
    Array.map (remove_index index_to_del) tbl

let add_row tbl =
  if is_empty tbl then raise (Invalid_argument "Can't add row to empty table")
  else Array.append tbl (Array.make_matrix 1 (num_cols tbl) "")

let del_row i tbl = 
  if i = 0 then raise (Invalid_argument "Can't delete first row of table") 
  else remove_index i tbl

let change_cell i j value tbl=
  if i < 0 || i > (num_rows tbl - 1) || j < 0 || j > (num_cols tbl - 1)
  then raise (Invalid_argument "index out of bounds") else
  if i = 0 then raise (Invalid_argument "Can't modify column names") else
    tbl.(i).(j) <- value;
  tbl

let get_cols_data (c_list:string list) (tbl:string array array) =
  let tbl_copy = Array.copy tbl in
  let cols = Array.to_list (get_cols tbl_copy) in
  let del_lst = List.filter (fun e -> not (List.mem e c_list)) cols in
  let rec iter lst (tbl_copy: string array array) =
    match lst with
    | [] -> tbl_copy
    | hd::tl -> iter tl (del_col hd tbl_copy)
  in 
  iter del_lst tbl_copy

(** [union_array array_1 array_2] is a new array that has a 1 in every index 
    where EITHER [array_1] or [array_2] had a 1. All other indices are 0.
    Requires: [array_1] and [array_2] have equal length.*)
let union_array array_1 array_2 =
  let rec union a1 a2 len res i =
    if i = len then res
    else if a1.(i) = true || a2.(i) = true then (
      res.(i) <- true;
      union a1 a2 len res (i + 1) )
    else union a1 a2 len res (i + 1)
  in
  let len = Array.length array_1 in
  union array_1 array_2 len (Array.make len false) 0


(** [union_array array_1 array_2] is a new array that has a 1 in every index 
    where BOTH [array_1] and [array_2] had a 1. All other indices are 0.
    Requires: [array_1] and [array_2] have equal length.*)
let intersect_array array_1 array_2 =
  let rec intersect a1 a2 len res i =
    if i = len then res
    else if a1.(i) = true && a2.(i) = true then (
      res.(i) <- true;
      intersect a1 a2 len res (i + 1) )
    else intersect a1 a2 len res (i + 1)
  in
  let len = Array.length array_1 in
  intersect array_1 array_2 len (Array.make len false) 0

(** [inverse_filter filter] is filter with every index negated. *) 
let inverse_filter filter = Array.map (fun b -> not b) filter

(** [filter_table tbl col_num comp obj res_array] modifies [res_array] in place,
    so that each of its indices which correspond to a row in [tbl] that 
    satisfies the condition [col_num] [comp] [obj] is changed to 1. 
    Raises: [Failure] if a [bi_re] other than [EQ] is used on non-integers.*)
let filter_table tbl col_index comp obj res =
  let check_row col_index obj res comp conv index row =
    if index <> 0 && comp (conv row.(col_index)) (conv obj) then
      res.(index) <- true
    else ()
  in
  match comp with
  | EQ -> Array.iteri (check_row col_index obj res ( = ) (fun x -> x)) tbl
  | LT -> Array.iteri (check_row col_index obj res ( < ) int_of_string) tbl
  | GT -> Array.iteri (check_row col_index obj res ( > ) int_of_string) tbl
  | GTEQ -> Array.iteri (check_row col_index obj res ( >= ) int_of_string) tbl
  | LTEQ -> Array.iteri (check_row col_index obj res ( <= ) int_of_string) tbl

let rec where conds tbl =
  match conds with
  | Binary (AND, expr1, expr2) ->
    intersect_array (where expr1 tbl) (where expr2 tbl)
  | Binary (OR, expr1, expr2) -> union_array (where expr1 tbl) (where expr2 tbl)
  | Expr (col, comp, obj) ->
    let col_index = find_index col tbl.(0) in
    if col_index = -1 then raise (Invalid_argument "Invalid column name")
    else
      let row_num = num_rows tbl in
      let rows_to_keep = Array.make row_num false in
      filter_table tbl col_index comp obj rows_to_keep;
      rows_to_keep
  |NotExpr (col, comp, obj) -> 
    inverse_filter (where (Expr (col, comp, obj)) tbl)

let all_pass tbl =
  let res = Array.make (num_rows tbl) true in
  res.(0) <- false;
  res

(** [del_rows rows_to_keep len tbl] is [tbl], which originally had length [len], 
    with every index corresponding to true in [rows_to_del] removed. For 
    example, if only the first and last indices of [rows_to_keep] were true, 
    then [del_rows rows_to_keep len tbl] would be [tbl] without its first or 
    last elements.*)
let del_rows rows_to_keep len tbl =
  let rec del_rows rows_to_keep row_num tbl i =
    if i < 0 then tbl
    else if rows_to_keep.(i) = true then
      del_rows rows_to_keep row_num (remove_index i tbl) (i - 1)
    else del_rows rows_to_keep row_num tbl (i - 1)
  in
  del_rows rows_to_keep len tbl (len - 1)

let select filter tbl =  
  let inv_filter = inverse_filter filter in
  inv_filter.(0) <- false; 
  del_rows inv_filter (num_rows tbl) tbl 

let delete filter tbl =
  del_rows filter (num_rows tbl) tbl 

let rec update filter set_objects tbl =
  match set_objects with
  | [] -> tbl
  | (col, value) :: tl ->
    let col_index = find_index col tbl.(0) in
    if col_index = -1 then raise (Invalid_argument "Invalid column name")
    else
      Array.iteri
        (fun i row ->
           if filter.(i) = true then row.(col_index) <- value else ())
        tbl;
    update filter tl tbl

let insert value_object_lst column_objects_opt tbl =
  let cols =
    match column_objects_opt with
    | Some (Columns lst) -> lst
    | None -> Array.to_list tbl.(0)
    | Some Wildcard -> raise (Failure "Invalid command: can't use * for insert")
  in
  let n_tbl = add_row tbl in
  let rec update_row cols vals row_index =
    match (cols, vals) with
    | [], [] -> n_tbl
    | col :: tl1, value :: tl2 ->
        let col_index = find_index col n_tbl.(0) in
        if col_index = -1 then raise (Invalid_argument "Invalid column name")
        else n_tbl.(row_index).(col_index) <- value;
        update_row tl1 tl2 row_index
    | _ ->
        raise
          (Invalid_argument "Number of values did not match number of columns.")
  in
  update_row cols value_object_lst (num_rows n_tbl - 1)

(** [char_cmp c1 c2] is -1, 0, or 1, depending on if the ASCII code of [c1] is 
    less than, equal to, or greater than the ASCII code of [c2], respectively.*) 
let char_cmp c1 c2 = let v1 = Char.code c1 in
  let v2 = Char.code c2 in
  if v1<v2 then 1 else if v1=v2 then 0 else -1 

(** [alphabetical_cmp s1 s2] is:
    a. 1 if s1 has higher alphabetical order than s2
    b. 0 if s1 and s2 are equal
    c. -1 if s1 has lower alphabetical order than s2
    Requires: s1 and s2 are both lower case. Can use [String.lowercase_ascii] on
    inputs before applying this funciton to satisfy this condition. *)
let rec alphabetical_cmp s1 s2 = match (s1,s2) with
  |("","")->0
  |("", x ) -> 1
  |(x, "") -> -1
  |(s1, s2) -> 
    let cmp = char_cmp s1.[0] s2.[0] in
    if cmp <> 0 then cmp else let s1' = String.sub s1 1 (String.length(s1)-1) in
      let s2' = String.sub s2 1 (String.length(s2)-1) in
      alphabetical_cmp s1' s2'

(** [row_compare i row1 row2] is -1, 0, or 1, depending on if the [i]th index of
    [row1] is less than, equal to, or greater than the [i]th index of [row2], 
    respectively. 
    Requires: row1, row2 have length >= i + 1 *)
let row_compare i row1 row2 =
  let v1 = row1.(i) in 
  let v2 = row2.(i) in 
  try
    let v1_int = int_of_string v1 in
    let v2_int = int_of_string v2 in
    compare v1_int v2_int
  with Failure x ->
    alphabetical_cmp (String.lowercase_ascii v1) (String.lowercase_ascii v2)

(** [rev tbl] is [tbl] with it's rows in reverse order. *)
let rev tbl =
  let num_rows = num_rows tbl - 1 in
  for i = 0 to num_rows / 2 do
    let temp = tbl.(i) in
    tbl.(i) <- tbl.(num_rows - i);
    tbl.(num_rows - i) <- temp
  done;
  tbl

(** [prepend header tbl] is [tbl] with [header] prepended to be the top row.
    Should be used only on a table with no column names, where [header] contains 
    the column names.
    Requires: The length of [header] = the number of columns in [tbl] *)
let prepend (header : string array) (tbl : string array array) =
  let len = num_cols tbl in
  let n_tbl = Array.append (Array.make_matrix 1 len "") tbl in
  for i = 0 to len - 1 do
    n_tbl.(0).(i) <- header.(i)
  done;
  n_tbl

let order_by column_object_bool_lst tbl =
  let header = Array.copy tbl.(0) in
  let n_tbl = remove_index 0 tbl in
  match column_object_bool_lst with
  | [] -> n_tbl
  | (col, b) :: tl ->
      let col_index = find_index col header in
      if col_index = -1 then raise (Invalid_argument "Invalid column name")
      else Array.stable_sort (row_compare col_index) n_tbl;
      if b then prepend header (rev n_tbl) else prepend header n_tbl
