(*
  Parser
  Do the syntax analysis on the list of lexemes and return an AST (Abstract Syntax Tree)
*)

(* return an AST from a list of lexemes *)
val syntax_analysis : Lexer.lexeme list -> Asyntax.arith_exp