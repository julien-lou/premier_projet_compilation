(* 
  Lexer
  Get the list of lexemes from a string.
*)


type lexeme =
    Integer_lex of int
  | Float_lex of float
  | Plus_sign
  | Minus_sign
  | Multiplication_sign
  | Division_sign
  | Modulus_sign
  | Left_parenthesis
  | Right_parenthesis
  | Plus_sign_dot
  | Minus_sign_dot
  | Multiplication_sign_dot
  | Function_lex of string
  

(* get the list of lexemes from a string *)
val lexical_analysis : string -> lexeme list