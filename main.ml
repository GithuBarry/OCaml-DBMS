open Command
open Iohandler
open Datatable
open Hashtbl
(* The user interactive command line *)

(** [print_array array] prints every element in [array]*)
let print_array array =
  Array.iter print_string array

(** [print_2D_array array] prints [array]  in a matrix format*)
let print_2D_array (array:string array array) = 
  array |> Array.iter (fun x -> 
      Array.iter (fun x -> print_string x; print_string "  ") x; 
      print_endline "")

(** [database] stores filename, associated directory and file content.
    Such data base is stored as a Hashtable, with type 
    (table_name, (directory of file * Datable of file)) Hashtbl.t*)
let database = Hashtbl.create 100

(** [update_database name table] updates [name] to [name] in [database] *)
let update_database name table = 
  Hashtbl.replace database name ((Hashtbl.find database name|> fst), table)

(** [get_table_name command_subject_lst] is the [table_name] represented in 
    [FROM] in [command_subject_lst] 
    Raise: [Not_found] when [FROM] is not found*)
let rec get_table_name command_subject_lst = 
  match command_subject_lst with 
  | [] -> raise Not_found
  | (From table_name) :: tail -> table_name
  | h::t -> get_table_name t

(** [get_table_name command_subject_lst] is the [values] represented in 
    [VALUES] in [command_subject_lst] 
    Raise: [Not_found] when [VALUES] is not found*)
let rec get_values command_subject_lst = 
  match command_subject_lst with 
  | [] -> raise Not_found
  | (Values values) :: tail -> values
  | h::t -> get_values t

(** [get_sets command_subject_lst] is the [sets] represented in 
    [SET] in [command_subject_lst] 
    Raise: [Not_found] when [SET] is not found*)
let rec get_sets command_subject_lst = 
  match command_subject_lst with 
  | [] -> raise Not_found
  | (Set set_binds) :: tail -> set_binds
  | h::t -> get_sets t

(** [get_where_exprs_opt command_subject_lst] is the [expr_objs] represented in 
    [WHERE] in [command_subject_lst] 
    Raise: [Not_found] when [FROM] is not found
*)
let rec get_where_exprs_opt command_subject_lst = 
  match command_subject_lst with 
  | [] -> None
  | (Where expr_objs) :: tail -> Some expr_objs
  | h::t -> get_where_exprs_opt t

(** [get_table table_name] is the [table] in [database] *)
let get_table table_name = 
  table_name|>Hashtbl.find database|>snd

(** [get_columns table_name column_objects] is the selected [column] indicated 
    by [column_objects] in table with the name [table_name]*)
let get_columns table column_objects = 
  match column_objects with
  | Wildcard -> table
  | Columns cols -> table|> get_cols_data cols

(** [get_formatted command_formatter table] is the formatted [table]
    and [command_formatter] can be nil
    Raises: [Failure] when unimplemented formatter is present in [table]*)
let get_formatted command_formatter table = match command_formatter with 
    [] -> table
  | [OrderBy x] -> order_by x table
  | _ -> failwith "Unexpected formatter"

let rec rep_loop () : unit=
  print_string "\n\n Enter command: \n\n > ";
  try begin 
    let (command_verb, command_subject_lst, command_formatter_lst) = 
      parse (read_line ()) 
    in 
    match command_verb with 
    | Select column_objects -> begin
        let table_name = get_table_name command_subject_lst in 
        let raw_table = get_table table_name in
        let formatted_table = get_formatted command_formatter_lst raw_table in 
        let filtered_table = 
          match get_where_exprs_opt command_subject_lst with 
          | None -> formatted_table
          | Some exprs -> select (where exprs formatted_table) formatted_table 
        in
        let cols = get_columns filtered_table column_objects in
        cols |> print_2D_array; rep_loop ()
      end
    | InsertInto (table_name, column_objects_opt) -> begin
        let raw_table = get_table table_name in
        let inserted_table = insert (get_values command_subject_lst) 
            column_objects_opt raw_table
        in
        update_database table_name inserted_table;
        inserted_table |>print_2D_array; rep_loop ()
      end
    | Update table_name -> begin
        let raw_table = get_table table_name in
        let updated_table = match get_where_exprs_opt command_subject_lst with 
          | None -> update (all_pass raw_table) 
                      (get_sets command_subject_lst) raw_table
          | Some exprs -> update (where exprs raw_table) 
                            (get_sets command_subject_lst) raw_table
        in
        update_database table_name updated_table;
        updated_table |>print_2D_array; rep_loop ()
      end
    | Delete-> begin
        let table_name = get_table_name command_subject_lst in 
        let raw_table = get_table table_name in
        match get_where_exprs_opt command_subject_lst with
          None -> begin 
            Iohandler.delete_file (Hashtbl.find database table_name|> fst) 
              table_name; 
            print_string table_name; 
            print_endline " deleted." 
          end
        | Some exprs -> begin
            let modified_table = delete (where exprs raw_table) raw_table in
            update_database table_name modified_table;
            modified_table |>print_2D_array; rep_loop ()
          end
      end
    | Quit -> print_endline("Quiting database"); exit 0
  end 
  with 
  | Empty -> 
    print_endline " Please enter non empty command \n > ";
    rep_loop ()
  | Malformed ->       
    print_endline " Invalid command \n > ";
    rep_loop ()
  | Not_found ->       
    print_endline " Invalid table/column name \n > ";
    rep_loop ()
  | Failure x -> 
    print_string " Failure: "; print_string x;
    print_endline " Please try again \n > "; rep_loop ()
  | _ -> print_endline " Please try again \n > "; rep_loop ()

(** [main ()] prompts for the directory/folder to open, then initiates REPL. *)
let rec main () =
  ANSITerminal.(print_string [ red ] "\n\n Welcome to the OCaml-DBMS.\n");
  print_endline " Please enter the directory of the Database.\n";
  print_string "> ";
  try (let rec get_database () = 
         let dir_list = ()|>read_line|>list_of_dir in
         match dir_list with 
         | [""] -> 
           print_string " Please enter non empty command \n > "; get_database()
         | [str] when Iohandler.is_dir str = false ->
           print_string " Please enter valid directory \n > "; get_database()
         | str_list when List.filter Iohandler.is_dir str_list <> str_list -> 
           print_string " Please enter valid directories \n > "; get_database()
         | dir_list -> 
           List.iter (fun dir -> (Iohandler.csvs_in_hastbl database dir)) dir_list
       in
       get_database (); 
       rep_loop ())
  with
    Failure x -> print_string "Restarting program because "; print_string x; 
    print_endline ""; main ()
  | _ -> print_string "Error. Restarting program"; main ()

(** Start the program *)
let () = main ()
