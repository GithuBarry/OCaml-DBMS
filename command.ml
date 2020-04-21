
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

type column_objects = column_object list

type table_name = string

type value_objects = string

type set_objects = (column_object * value_objects) list

type expr_objects = 
    Expr of column_object * bi_re * value_objects
  | Binary of bi_op * expr_objects * expr_objects 

type command_verb = 
  | Select of column_objects
  | InsertInto of table_name * (column_objects option)
  | Update of table_name
  | Delete
  | Quit

type command_subject =   
  | From of table_name
  | Where of expr_objects
  | Values of value_objects
  | Set of set_objects

type command_formatter = 
  | OrderBy of column_objects
  | Distinct

type command = command_verb * command_subject list 
               * command_formatter list

exception Empty

exception Malformed


let parse str = failwith("Unimplemented")
