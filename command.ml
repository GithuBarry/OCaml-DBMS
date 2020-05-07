(**
   Parsing of SQL commands.
*)
type bi_op = 
    AND
  | OR

type bi_re = 
    GT
  | EQ
  | LT
  | GTEQ
  | LTEQ

type column_object = string

type column_objects = 
    Wildcard
  | Columns of column_object list

type table_name = string

type value_object = string

type set_objects = (column_object * value_object) list

type expr_objects = 
    Expr of column_object * bi_re * value_object
  | Binary of bi_op * expr_objects * expr_objects 
  | NotExpr of column_object * bi_re * value_object

type command_verb = 
  | Select of column_objects
  | InsertInto of table_name * (column_objects option)
  | Update of table_name
  | Delete
  | Quit

type command_subject =   
  | From of table_name
  | Where of expr_objects
  | Values of value_object list
  | Set of set_objects

type command_formatter = 
  (* [bool list] is [true] when oder is [DESC]*)
  | OrderBy of (column_object * bool) list

type command = command_verb * command_subject list 
               * command_formatter list

exception Empty

exception Malformed

(** [list_partition keys body] is 
    [[everything before first appearance of an element in str], 
    [everything after the str element]]
    Raise: [Not_found] when cannot find [str] in the [body]
*)
let list_partition keys body = 
  (** [list_partition_helper keys tail head] is [[v1, v2,...],key,[... vn]] 
      for any key that in [keys] 
      Raise: [Not_found] when cannot find sny element of [str] in [tail] *)
  let rec list_partition_helper keys tail head = match tail with
      h::t when List.mem h keys ->  (List.rev head, h , t)
    | h::t -> list_partition_helper keys t (h::head)
    | [] -> raise Not_found
  in
  list_partition_helper keys body []

(** [keyword_partition keywords body] is association list of 
    [keywords] and its immediately following content in [body] until the next 
    occurence of element from [keywords].
    if [keywords] are [k1;k2;...] and [body] is [k1; b1; b2; k2; b3; k3 ...]
    then [keyword_partition keywords body] is [(k1,[b1,b2]);(k2,[b3]);... ]
*)
let keyword_partition keywords body = 
  (** [keyword_partition_helper keywords body outcome] is list of tuples of 
      [keywords] and its immediately content before in [body] until the 
      previous keyword.
      if [keywords] are [k1;k2;...] and [body] is [b3; k2; b2; b1; k1;]
      then [keyword_partition_helper keywords body outcome] 
      is [(k1,[b1,b2]);(k2,[b3]);... ]
  *)
  let rec keyword_partition_helper keywords body outcome = 
    if List.filter (fun x -> List.mem x body) keywords = [] then outcome else
      let (head, matched_key, tail) = list_partition keywords body in
      keyword_partition_helper keywords tail ((matched_key,head)::outcome)
  in
  keyword_partition_helper keywords (List.rev body) [] 
  |> List.map (fun (a,list) -> (a,List.rev list))


(** [parse_column str_list] returns a column title out of [str_list]
    and [str_list] should contain exactly one column name
    Raise: [Empty] when [str_list] is empty
*)
let parse_column str_list : column_object=  match str_list with
    [] -> raise Empty
  | list -> list |> String.concat " "

(** [parse_orders str_list] is the tuple list with first element being the 
    [column name] and second element indicating if [decreasing] *)
let parse_orders str_list : (column_object * bool) list = match str_list with
    [] -> raise Empty
  | list -> 
    list |> String.concat " " 
    |> String.split_on_char ',' 
    |> List.map String.trim |>  
    List.map (fun x -> 
        if String.length x < 5 
        then (x,false) 
        else if String.sub x ((String.length x)- 3) 3 = "ASC" 
        then (String.sub x 0 (String.length x - 4),false) 
        else if String.sub x ((String.length x)- 4) 4 = "DESC" 
        then (String.sub x 0 (String.length x - 5),true) 
        else (x,false))

(** [parse_columns str_list] returns column title(s) out of [str_list]
    Raise: [Empty] when [str_list] is empty
*)
let parse_columns str_list =  match str_list with
    [] -> raise Empty
  | ["*"] -> Wildcard
  | list -> Columns (list |> String.concat " " 
                     |> String.split_on_char ',' 
                     |> List.map String.trim)



(** [parse_bi_re str] returns a [bi_re] corrosponding tp [str]
    Raise: Failure when [str] is illeagal*)
let parse_bi_re str = match str with 
    ">" -> GT
  | "=" -> EQ
  | "<" -> LT
  | ">=" -> GTEQ
  | "<=" -> LTEQ
  | _ -> failwith("Internal Error: Illeagal Relation")

(** [parse_bi_op str] returns a [bi_op] corrosponding tp [str]
    Raise: Failure when [str] is illeagal*)
let parse_bi_op str = match str with 
    "AND" -> AND
  | "OR" -> OR
  | _ -> failwith("Internal Error: Illeagal Operator")

(** [parse_value str_list] returns a [value_object]
    Raise: Empty when [str_list] is empty *)
let parse_value str_list =  match str_list with
    [] -> raise Empty
  | list -> list |> String.concat " "

(** [parse_values str_list] returns values out of [str_list]
    Raise: [Empty] when [str_list] is empty
*)
let parse_values str_list =  match str_list with
    [] -> raise Empty
  | list -> Values (list |> String.concat " " 
                    |> String.split_on_char ',' 
                    |> List.map String.trim)

(** [parse_expr str_list] returns a where [command_subject] out of [str_list]
    [OR] is always processed before [AND] as [AND] has higher priority
    Requires: str_list not to include "WHERE"
    Raise: [Empty] when [str_list] is empty
    Raise: [Malformed] when [str_list] cannot be parsed to a where object
*)
let rec parse_expr str_list = 
  if  (List.mem "OR" str_list) then 
    let (before, bi_op, after) = list_partition ["OR"] str_list 
    in
    Binary (parse_bi_op bi_op, parse_expr before, parse_expr after)
  else if (List.mem "AND" str_list) then 
    let (before, bi_op, after) = list_partition ["AND"] str_list 
    in
    Binary (parse_bi_op bi_op, parse_expr before, parse_expr after)
  else let (before, re, after) = list_partition [">";"=";"<";">=";"<="] str_list 
    in
    match before with 
      "NOT"::t -> NotExpr (parse_column before, parse_bi_re re, parse_value after)
    | _ -> Expr (parse_column before, parse_bi_re re, parse_value after)


(** [parse_table_name str_list] returns a [table_name] out of [str_list]
    [str_list], if have multiple elemenets, should be separated by " " in a 
    table_name
    Raise: [Empty] when [str_list] is empty
*)
let parse_table_name str_list : table_name = match str_list with 
    [] -> raise Empty
  | x -> String.concat " " str_list


(** [parse_select str_list] is the command from [str_list] 
    with command_verb being [Select]
    Requires: [str_list] should be string list inluding the keyword "SELECT"
*)
let parse_select str_list : command = 
  let assoc_list = 
    keyword_partition ["SELECT";"FROM";"WHERE"; "ORDER";"BY"] str_list in
  let after_select = List.assoc "SELECT" assoc_list in 
  let after_from = List.assoc "FROM" assoc_list in
  let subject = if List.assoc_opt "WHERE" assoc_list = None 
    then [From (parse_table_name after_from)] 
    else [From (parse_table_name after_from); 
          Where (parse_expr (List.assoc "WHERE" assoc_list))]
  in
  let formatter = if List.assoc_opt "BY" assoc_list = None then [] 
    else [OrderBy (parse_orders (List.assoc "BY" assoc_list))] in
  Select (parse_columns after_select), subject, formatter

(** [parse_set str_list] returns [set_objects] which [str_list] represents*)
let parse_set str_list : set_objects = 
  str_list 
  |> String.concat " " 
  |> String.split_on_char ',' 
  |> List.map String.trim 
  |> List.map (String.split_on_char '=') 
  |> List.map (function (h::t) -> (h,String.concat " " t)
                      | []->raise Malformed) 
  |> List.map (fun (x,y) -> (String.trim x, String.trim y))


(** [parse_select str_list] is the command from [str_list] 
    with command_verb being [Parse]
    Requires: [str_list] should be string list inluding the keyword "UPDATE"
*)
let parse_update str_list : command = 
  let assoc_list = keyword_partition ["UPDATE";"SET";"WHERE"] str_list in
  let after_update = List.assoc "UPDATE" assoc_list in 
  let after_set = List.assoc "SET" assoc_list in
  let subject = 
    if List.assoc_opt "WHERE" assoc_list = None 
    then [Set (parse_set after_set)] 
    else [Where (parse_expr (List.assoc "WHERE" assoc_list)); 
          Set (parse_set after_set)]
  in
  Update (parse_table_name after_update), subject, []

(** [remove_all str_lst char_list] is the [str_lst] without any char present in 
    [char_list]
*)
let rec remove_all char_list str_lst: string list = match char_list with 
  | [] -> str_lst
  | h::t -> remove_all t 
              (str_lst|> List.map (fun x -> String.split_on_char h x
                                            |> String.concat ""))

(** [rmem_from_all str_lst char] is [Some string] where [string] is the 
    first string with [char] in the list, and [None] if does not exist
*)
let find_from_all char str_lst: string option =
  match List.filter (fun x -> String.contains x char) str_lst with
    h::t -> Some h
  | _ -> None

(** [parse_select str_list] is the command from [str_list] 
    with command_verb being [INSERT INTO]
    Requires: [str_list] should be string list inluding the keyword "INSERT"
*)
let parse_insert str_list : command = 
  let assoc_list = keyword_partition ["INTO";"VALUES"] str_list in
  let after_insert = List.assoc "INTO" assoc_list in 
  let after_values = List.assoc "VALUES" assoc_list in
  let first_parenthesis = List.assoc "INTO" assoc_list |> find_from_all '(' in
  let table_name =  match first_parenthesis with 
      None -> after_insert 
    | Some first -> let (a, _ ,_ ) = list_partition [first] after_insert in a 
  in 
  let cols_opt = match first_parenthesis with 
      None -> None 
    | Some first -> let (_, b ,c) = list_partition [first] after_insert in 
      Some (b::c |> remove_all ['(';')'] |> parse_columns)
  in
  InsertInto (parse_table_name table_name, cols_opt), 
  [parse_values (remove_all ['(';')'] after_values)] ,[]


(** [parse_delete str_list] is the command from [str_list] 
    with command_verb being [DELETE]
    Requires: [str_list] should be string list inluding the keyword "DELETE"
*)
let parse_delete str_list : command = 
  let assoc_list = 
    keyword_partition ["DELETE";"FROM";"WHERE"] str_list in
  let after_from = List.assoc "FROM" assoc_list in 
  let subjects = if List.mem_assoc "WHERE" assoc_list
    then [From (parse_table_name after_from); 
          Where (parse_expr (List.assoc "WHERE" assoc_list))] 
    else [From (parse_table_name after_from)]
  in
  Delete, subjects, []


(** No support for 
    - use of [']
    - Any title, value, name with consecutively 2+ spaces. e.g. "Last     Name" 
    - Column name with [,]
    - Not
    - No space in bi-relation. "x>0" must be written as "x > 0" and etc. 
    - Greater than wrtten as "> =" or similar separation of style
    - Adding a space after a comma e.g. "a, b" is ["[a],[ b]"] not ["[a],[b]"]
*)
let parse str : command= 
  let command_list_raw = String.split_on_char ' ' str in 
  let command_list = List.filter (fun x -> x <> "") command_list_raw in 
  match command_list with 
    "SELECT"::_ -> parse_select command_list
  | "INSERT"::_ -> parse_insert command_list
  | "UPDATE"::_ -> parse_update command_list
  | "DELETE"::_ -> parse_delete command_list
  | fst::_ when String.lowercase_ascii fst = "quit" -> (Quit, [], [])
  | _ -> raise Malformed





