open Command
open Iohandler
open Datatable

(**  *)
let rec rep_loop (* (db : Database.t) *) =
  print_string "\n\n Enter command: \n\n > ";

  (*let input = try parse (read_line ()) with Error handling? *)
  (* in match input with *)
  (* list of commands *)

  (** [main ()] prompts for the directory/folder to open, then initiates REPL. *)
  let main () =
    ANSITerminal.(print_string [ red ] "\n\n Welcome to ___.\n");
    print_endline " Please enter ___.\n";
    print_string "> ";
    match read_line () with _ -> failwith "unimplemented"

  in () (*To make compile *)

(*let () = main () *)
