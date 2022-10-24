open Parser
open Asmeditor
open Lexer
open Typecheck
open X86_64
open Format


let formula = "(3 + 4) + (5 + 6)"
;;

let lex = lexical_analysis formula
;;

let syn = syntax_analysis lex
;;

let type_e = static_analysis syn
;;

let () =
  let code = {  text =
    begin_main ++
    compile_expr syn ++
    movq (reg rax) (reg rdi) ++
    compile_printint ++
    xorq (reg rax) (reg rax) ++
    ret ++
  define_functions;
    data = label "S_int" ++ string "%d\n"; } in
    let c = open_out "add.s" in
    let fmt = formatter_of_out_channel c in
    X86_64.print_program fmt code;
    close_out c
;;