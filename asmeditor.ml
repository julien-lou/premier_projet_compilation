open Format
open X86_64
open Asyntax
open Typecheck


let n = ref 0 (* n counts the quantity of data already used *)

let rec compile_expr e =
  let type_e = static_analysis e in
  match e with
  |Parentheses(arith_e) -> 
    begin
      let t = (compile_expr arith_e).text in
      let d = (compile_expr arith_e).data in
       { text = t; data = d; }
    end
  |Integer(number) -> 
    { text = movq (imm number) (reg rax); data = nop; }
  |Float(number) -> n := !n + 1;
    { text = movq (inline (Printf.sprintf ".F%d" !n)) (reg rax); 
    data = label (Printf.sprintf ".F%d" !n) ++ 
      inline (Printf.sprintf "    .double %f" number); }
  |Function(name_of_fn, arith_e) -> 
    begin 
      let t = (compile_expr arith_e).text in
      let d = (compile_expr arith_e).data in
      let type_arg = static_analysis arith_e in
      { text = t ++
      movq (reg rax) (reg rdi) ++
      (fn_of_name name_of_fn type_arg);
      data = d; }
    end
  |Operation(name_of_op, e1, e2) ->
    begin 
      let t1 = (compile_expr e1).text in
      let d1 = (compile_expr e1).data in
      let t2 = (compile_expr e2).text in
      let d2 = (compile_expr e2).data in
      if type_e = "integer" then
        {
        text = t1 ++
        pushq (reg rax) ++
        t2 ++
        movq (reg rax) (reg rsi) ++
        popq rdi ++
        (operation_of_name name_of_op);
        data = d1 ++ d2;
        }
      else
        {
          text = t1 ++
          subq (imm 8) (reg rsp) ++
          inline "movq  %rax, (%rsp)" ++
          t2 ++
          movq (reg rax) (reg rsi) ++
          inline "movq  (%rsp), %rdi" ++
          addq (imm 8) (reg rsp) ++
          (operation_of_name name_of_op);
          data = d1 ++ d2;
        }
    end
  |Empty -> raise (Error "An expression cannot be empty.")

and fn_of_name name_of_fn type_arg =
  match name_of_fn with 
  |"Plus" ->
    compile_Plus
  |"Minus" ->
    begin
      match type_arg with
      |"integer" -> compile_Minus
  |_ ->
    raise (Error (Printf.sprintf "The function %s is not defined." name_of_fn))

and operation_of_name name_of_op =
  match name_of_op with
  |"Addition" -> compile_add_int
  |"Substraction" -> compile_sub_int
  |"Multiplication" -> compile_mul_int
  |"Division" -> compile_div_int
  |"Modulo" -> compile_mod_int
  |_ ->
    raise (Error (Printf.sprintf "The operation %s is not defined." name_of_op))
    
and compile_Plus = 
  movq (reg rdi) (reg rax)

and compile_Minus =
  negq (reg rdi) ++
  movq (reg rdi) (reg rax)
and compile_add_int =
  call "add_int"

and compile_sub_int =
  call "sub_int"

and compile_mul_int =
  call "mul_int"

and compile_div_int =
  call "div_int"

and compile_mod_int =
  call "mod_int"

and compile_add_float =
  call "add_float"
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
        addq    %rsi, %rdi
        movq    %rdi, %rax
        ret

sub_int:
        subq    %rsi, %rdi
        movq    %rdi, %rax
        ret

mul_int:
        imulq    %rsi, %rdi
        movq    %rdi, %rax
        ret

div_int:
        xorq    %rdx, %rdx   
        testq   %rdi, %rdi
        jns     resume_div
        notq    %rdx  #case where the dividend is negative
  resume_div:    
        movq    %rdi, %rax                
        idivq   %rsi
        ret

mod_int:
        xorq    %rdx, %rdx
        testq   %rdi, %rdi
        jns     positive_mod
        notq    %rdx
        movq    %rdi, %rax        
        idivq   %rsi
        testq   %rsi, %rsi
        jns     rsi_positive
        negq    %rsi  
  rsi_positive:
        addq    %rsi, %rdx
        movq    %rdx, %rax
        ret
  positive_mod: #if the dividend is positive
        movq    %rdi, %rax
        idivq   %rsi
        movq    %rdx, %rax
        ret

print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret

add_float:
        movq    %rdi, %xmm0
        movq    %rsi, %xmm1
        addsd	  %xmm0, %xmm1
        movq	  %xmm1, %rax
        ret

print_float:
        movq	  %rdi, %xmm0
        movq	  $1, %rax	#rax is the number of float arguments
        movq    $S_float, %rdi
        call    printf
        xorq	  %rax, %rax
        ret
          "

let define_labels = 
  label "S_int" ++ string "%d\n" ++
  label "S_float" ++ string "%f\n"

let create_code e =
  let comp = compile_expr e in
  let t = comp.text in
  let d = comp.data in
  { text =
    begin_main ++
    t ++
    movq (reg rax) (reg rdi) ++
    compile_printint ++
    xorq (reg rax) (reg rax) ++
    ret ++
  define_functions;
    data =
    d ++
    define_labels; }
