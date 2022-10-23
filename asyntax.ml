exception Error of string

type arith_exp =
  |Empty
  |Parentheses of arith_exp
  |Integer of int
  |Function of string * arith_exp
  |Operation of string * arith_exp * arith_exp
;;

