(* 
  asm editor
  Edit the *.s file based on the TAST.
*)

(* create the assembly code based on the TAST *)
val create_code : Asyntax.arith_exp -> X86_64.program