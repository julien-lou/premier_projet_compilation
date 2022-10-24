open Format
open X86_64
open Asyntax


let rec compile_expr e =
  match e with
  |Parentheses(arith_e) -> compile_expr arith_e
  |Integer(number) -> movq (imm number) (reg rax)
  |Function(name_of_fn, arith_e) -> 
    compile_expr arith_e ++
    movq (reg rax) (reg rdi) ++
    (fn_of_name name_of_fn)
  |Operation(name_of_op, e1, e2) ->
    compile_expr e1 ++
    pushq (reg rax) ++
    compile_expr e2 ++
    movq (reg rax) (reg rsi) ++
    popq rdi ++
    (operation_of_name name_of_op)
  |Empty -> raise (Error "An expression cannot be empty.")

and fn_of_name name_of_fn =
  match name_of_fn with 
  |"Plus" ->
    compile_Plus
  |_ ->
    raise (Error (Printf.sprintf "The function %s is not defined." name_of_fn))

and operation_of_name name_of_op =
  match name_of_op with
  |"Addition" ->
    compile_addition
  |_ ->
    raise (Error (Printf.sprintf "The operation %s is not defined." name_of_op))
    
and compile_Plus = 
  movq (reg rdi) (reg rax)

and compile_addition =
  call "add_int"
;;

let compile_printint =
  call "print_int"

let begin_main =
  globl "main" ++ label "main"

let end_main =
  xorq  (reg rax) (reg rax) ++
  ret

let define_functions =
  inline "
plus:
        movq    %rdi, %rax

add_int:
        addq    %rdi, %rsi
        movq    %rsi, %rax
        ret

print_int:
        movq  %rdi, %rsi
        movq  $S_int, %rdi
        xorq  %rax, %rax
        call  printf
        ret

          "

