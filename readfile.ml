(* 
  Read file
  Read string written in a file.
*)


let read_file filename = (* get the entire file in a list of string *)
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines ;;
;;

let first_line str_list = (* get the first string in a list of string *)
  match str_list with
  |[] -> ""
  |h::t -> h
;;

let read_first_line filename = (* get the first line of a file *)
  first_line (read_file filename)
;;


