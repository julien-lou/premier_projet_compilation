open Parser
open Asmeditor
open Lexer
open Typecheck
open X86_64
open Format
open Readfile


let filename = Sys.argv.(1)
;;

let asm_filename =
  let rec aux str n current_str =
    if str.[n] = '.' then
      current_str ^ ".s"
    else
      aux str (n + 1) (current_str ^ (String.make 1 str.[n]))
  in aux filename 0 ""
;;

let formula = read_first_line filename
;;

let lex = lexical_analysis formula (* lexical analysis *)
;;

let syn = syntax_analysis lex (* syntax analysis *)
;;

let type_e = static_analysis syn (* static analysis *)
;;

let () =
  let code = create_code syn in
    let c = open_out asm_filename in
    let fmt = formatter_of_out_channel c in
    X86_64.print_program fmt code; (* printing out the asm code *)
    close_out c
;;
