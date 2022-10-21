type lexeme =
  |Integer of int
  |Plus_sign
  |Left_parenthesis
  |Right_parenthesis
;;

let rec complete_word f n =
  let len = String.length f in
  if n >= len
    then ""
  else if f.[n] = ' '
    then ""
  else (String.make 1 f.[n]) ^ complete_word f (n + 1)
;;

let lexical_analysis f =
  let len = (String.length f) in
  let rec aux index str =
    if index < len then match (complete_word str index) with
      |"" -> (aux (index + 1) str)
      |"(" -> Left_parenthesis::(aux (index + 1) str)
      |")" -> Right_parenthesis::(aux (index + 1) str)
      |"+" -> Plus_sign::(aux (index + 1) str)
      |_ -> Integer(int_of_string (complete_word str index))::(aux (index + (String.length (complete_word str index))) str)
    else []
  in aux 0 f
;;

