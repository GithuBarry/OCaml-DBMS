type expr_objects = string list

type column_objects = string list

type table_name = string

type command_verb = 
  | Select of column_objects
  | InsertInto of table_name * (column_objects option)
  | Update of table_name
  | Delete
  | Quit

type command_subject =   
  | From of table_name
  | Where of expr_objects
  | Values of expr_objects
  | Set of expr_objects

type command_formatter = 
  | OrderBy of column_objects
  | Distinct

type command = command_verb * command_subject list 
               * command_formatter list