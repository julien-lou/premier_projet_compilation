open Format
open X86_64
open Asyntax
open Typecheck

let n = ref 0
let rec compile_expr e =  (* n counts the quantity of data already used *)
  let type_e = static_analysis e in
  match e with
  |Parentheses(arith_e) -> 
    begin
      let result = (compile_expr arith_e) in
       { text = result.text; data = result.data; }
    end
  |Integer(number) -> 
    { text = movq (imm number) (reg rax); data = nop; }
  |Float(number) ->
    n := !n + 1;
    { text = inline ((Printf.sprintf "        movq  .F%d," !n) ^ " %rax\n"); 
      data = label (Printf.sprintf ".F%d" !n) ++ 
        inline (Printf.sprintf "        .double %f\n" number); }
  |Function(name_of_fn, arith_e) -> 
    begin 
      let result = (compile_expr arith_e) in
      let type_arg = static_analysis arith_e in
      { text = result.text ++
      movq (reg rax) (reg rdi) ++
      (fn_of_name name_of_fn type_arg);
      data = result.data; }
    end
  |Operation(name_of_op, e1, e2) ->
    begin 
      let result1 = (compile_expr e1) in
      let result2 = (compile_expr e2) in
      if type_e = "integer" then
        {
        text = result1.text ++
        pushq (reg rax) ++
        result2.text ++
        movq (reg rax) (reg rsi) ++
        popq rdi ++
        (operation_of_name name_of_op);
        data = result1.data ++ result2.data;
        }
      else
        {
          text = result1.text ++
          subq (imm 8) (reg rsp) ++
          inline "        movq  %rax, (%rsp)\n" ++
          result2.text ++
          movq (reg rax) (reg rsi) ++
          inline "        movq  (%rsp), %rdi\n" ++
          addq (imm 8) (reg rsp) ++
          (operation_of_name name_of_op);
          data = result1.data ++ result2.data;
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
      |"integer" -> compile_Minus_int
      |"float" -> compile_Minus_float
      |_ -> raise (Error "An expression cannot be empty.")
    end
  |"Int" -> compile_Int
  |"Float" -> compile_Float
  |_ ->
    raise (Error (Printf.sprintf "The function %s is not defined." name_of_fn))

and operation_of_name name_of_op =
  match name_of_op with
  |"Addition_int" -> compile_add_int
  |"Addition_float" -> compile_add_float
  |"Substraction_int" -> compile_sub_int
  |"Substraction_float" -> compile_sub_float
  |"Multiplication_int" -> compile_mul_int
  |"Multiplication_float" -> compile_mul_float
  |"Division_int" -> compile_div_int
  |"Modulo_int" -> compile_mod_int
  |_ ->
    raise (Error (Printf.sprintf "The operation %s is not defined." name_of_op))
    
and compile_Plus = 
  call "plus"

and compile_Minus_int =
  call "minus_int"

and compile_Minus_float =
  call "minus_float"

and compile_Int =
  call "convert_to_int"

and compile_Float =
  call "convert_to_float"

and compile_add_int =
  call "add_int"

and compile_add_float =
  call "add_float"

and compile_sub_int =
  call "sub_int"

and compile_sub_float =
  call "sub_float"

and compile_mul_int =
  call "mul_int"

and compile_mul_float =
  call "mul_float"

and compile_div_int =
  call "div_int"

and compile_mod_int =
  call "mod_int"
;;

let compile_printint =
  call "print_int"

let compile_printfloat =
  call "print_float"

let begin_main =
  globl "main" ++ label "main"

let end_main =
  xorq  (reg rax) (reg rax) ++
  ret

let define_functions =
  inline "
plus:
        movq    %rdi, %rax

minus_int:
        negq  %rdi
        movq  %rdi, %rax

minus_float:
        movq  Minus_one, %xmm1
        movq  %rdi, %xmm0
        mulsd %xmm1, %xmm0
        movq  %xmm0, %rax
        ret

convert_to_int:
        movq	%rdi, %xmm0
        cvttsd2siq	%xmm0, %rax
        ret

conver_to_float:
        cvtsi2sdq %rdi, %xmm0
        movq	%xmm0, %rax
        ret

add_int:
        addq    %rsi, %rdi
        movq    %rdi, %rax
        ret

add_float:
        movq    %rdi, %xmm0
        movq    %rsi, %xmm1
        addsd	  %xmm1, %xmm0
        movq	  %xmm0, %rax
        ret

sub_int:
        subq    %rsi, %rdi
        movq    %rdi, %rax
        ret
    
sub_float:
        movq    %rdi, %xmm0
        movq    %rsi, %xmm1
        subsd	  %xmm1, %xmm0
        movq	  %xmm0, %rax
        ret

mul_int:
        imulq    %rsi, %rdi
        movq    %rdi, %rax
        ret
  
mul_float:
        movq    %rdi, %xmm0
        movq    %rsi, %xmm1
        mulsd	  %xmm1, %xmm0
        movq	  %xmm0, %rax
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
	      testq	  %rdx, %rdx
	      je	    null_remainder
        testq   %rsi, %rsi
        jns     rsi_positive
        negq    %rsi  
  rsi_positive:
        addq    %rsi, %rdx
  null_remainder:
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
  label "S_float" ++ string "%f\n" ++
  label "Minus_one" ++ inline "        .double -1\n" ++
  inline "
  "

let create_code e =
  let comp = compile_expr e in
  let t = comp.text in
  let d = comp.data in
  if static_analysis e = "integer" then
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
  else
    { text =
      begin_main ++
      t ++
      movq (reg rax) (reg rdi) ++
      compile_printfloat ++
      xorq (reg rax) (reg rax) ++
      ret ++
    define_functions;
      data =
      d ++
      define_labels; }
