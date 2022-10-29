(*
  Type checking
  Check if the AST is correctly typed. (TAST)
*)

(* return the type of the expression, interrupt if the expression is not correctly typed *)
val static_analysis : Asyntax.arith_exp -> string