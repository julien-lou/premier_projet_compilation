open Parser
open Asmeditor
open Lexer
open Typecheck
open X86_64
open Format


let formula = "2.*.3.14*.3.14*.3.14 -. 3.14*.3.14-.3.14"
;;

let lex = lexical_analysis formula
;;

let syn = syntax_analysis lex
;;

let type_e = static_analysis syn
;;

let () =
  let code = create_code syn in
    let c = open_out "expression.s" in
    let fmt = formatter_of_out_channel c in
    X86_64.print_program fmt code;
    close_out c
;;