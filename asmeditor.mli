val compile_expr : Asyntax.arith_exp -> X86_64.text

val fn_of_name : string -> X86_64.text

val operation_of_name : string -> X86_64.text

val compile_Plus : X86_64.text

val compile_addition : X86_64.text

val compile_printint : X86_64.text

val begin_main : X86_64.text

val end_main : X86_64.text

val define_functions : X86_64.text