type lexeme =
    Integer_lex of int
  | Plus_sign
  | Left_parenthesis
  | Right_parenthesis

val complete_word : string -> int -> string

val lexical_analysis : string -> lexeme list