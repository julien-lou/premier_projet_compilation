val n : int ref

val compile_expr : Asyntax.arith_exp -> X86_64.program

val fn_of_name : string -> X86_64.text

val operation_of_name_type : string -> string -> X86_64.text

val compile_Plus : X86_64.text

val compile_add_int : X86_64.text

val compile_add_float : X86_64.text

val compile_printint : X86_64.text

val begin_main : X86_64.text

val end_main : X86_64.text

val define_functions : X86_64.text

val create_code : Asyntax.arith_exp -> X86_64.program