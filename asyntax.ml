exception Error of string
;;

type arith_exp =
  |Empty
  |Parentheses of arith_exp
  |Integer of int
  |Float of float
  |Function of string * arith_exp
  |Operation of string * arith_exp * arith_exp
;;

