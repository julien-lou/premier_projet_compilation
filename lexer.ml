type lexeme =
  |Integer_lex of int
  |Plus_sign
  |Minus_sign
  |Multiplication_sign
  |Division_sign
  |Modulus_sign
  |Left_parenthesis
  |Right_parenthesis
;;

let is_digit c =
  match c with
  |'0' .. '9' -> true
  |_ -> false

let complete_word f n =
  let rec get_number f1 n1 =
    let len = String.length f1 in
    if n1 >= len
    then ""
    else begin
      match f1.[n1] with
      |' ' -> ""
      |'(' -> ""
      |')' -> ""
      |'+' -> ""
      |'-' -> 
        begin
          if n1 < len - 1 && is_digit f1.[n1 + 1] then
            (String.make 1 f1.[n1]) ^ (get_number f1 (n1 + 1))
          else
            ""
        end
      |'*' -> ""
      |'/' -> ""
      |'%' -> ""
      |_ -> (String.make 1 f1.[n1]) ^ (get_number f1 (n1 + 1))
    end
  in
  let len = String.length f in
    if n >= len
    then ""
    else begin
      match f.[n] with
      |' ' -> " "
      |'(' -> "("
      |')' -> ")"
      |'+' ->
        begin
          if n < len - 1 && is_digit f.[n + 1] then
            (String.make 1 f.[n]) ^ (get_number f (n + 1))
          else
            "+"
        end
      |'-' ->
        begin
          if n < len - 1 && is_digit f.[n + 1] then
            (String.make 1 f.[n]) ^ (get_number f (n + 1))
          else
            "-"
        end
      |'*' -> "*"
      |'/' -> "/"
      |'%' -> "%"
      |_ -> get_number f n
    end
;;

let lexical_analysis f =
  let len = (String.length f) in
  let rec aux index str =
    if index < len then match (complete_word str index) with
      |"" -> (aux (index + 1) str)
      |" " -> (aux (index + 1) str)
      |"(" -> Left_parenthesis::(aux (index + 1) str)
      |")" -> Right_parenthesis::(aux (index + 1) str)
      |"+" -> Plus_sign::(aux (index + 1) str)
      |"-" -> Minus_sign::(aux (index + 1) str)
      |"*" -> Multiplication_sign::(aux (index + 1) str)
      |"/" -> Division_sign::(aux (index + 1) str)
      |"%" -> Modulus_sign::(aux (index + 1) str)
      |_ -> Integer_lex(int_of_string (complete_word str index))::(aux (index + (String.length (complete_word str index))) str)
    else []
  in aux 0 f
;;
