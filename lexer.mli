type lexeme =
    Integer_lex of int
  | Plus_sign
  | Minus_sign
  | Multiplication_sign
  | Division_sign
  | Modulus_sign
  | Left_parenthesis
  | Right_parenthesis
  
val lexical_analysis : string -> lexeme list