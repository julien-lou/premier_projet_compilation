open Lexer
open Asyntax

val largest_independent_expression :
  Lexer.lexeme list -> Lexer.lexeme list * Lexer.lexeme list
val strip_parentheses : 'a list -> 'a list
val syntax_analysis : Lexer.lexeme list -> Asyntax.arith_exp