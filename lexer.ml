type lexeme =
  |Integer_lex of int
  |Float_lex of float
  |Plus_sign
  |Minus_sign
  |Multiplication_sign
  |Division_sign
  |Modulus_sign
  |Left_parenthesis
  |Right_parenthesis
  |Plus_sign_dot
  |Minus_sign_dot
  |Multiplication_sign_dot
  |Function_lex of string
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
      |'-' -> ""
      |'*' -> ""
      |'/' -> ""
      |'%' -> ""
      |_ -> (String.make 1 f1.[n1]) ^ (get_number f1 (n1 + 1))
    end
  in
  let rec get_string f2 n2 =
    let len = String.length f2 in
    if n2 >= len
    then ""
    else begin
      match f2.[n2] with
      |' ' -> ""
      |'(' -> ""
      |')' -> ""
      |'+' -> ""
      |'-' -> ""
      |'*' -> ""
      |'/' -> ""
      |'%' -> ""
      |_ when (is_digit f2.[n2]) -> ""
      |_ -> (String.make 1 f2.[n2]) ^ (get_string f2 (n2 + 1))
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
          else if n < len - 1 && f.[n + 1] = '.' then
            "+."
          else
            "+"
        end
      |'-' ->
        begin
          if n < len - 1 && is_digit f.[n + 1] then
            (String.make 1 f.[n]) ^ (get_number f (n + 1))
          else if n < len - 1 && f.[n + 1] = '.' then
            "-."
          else
            "-"
        end
      |'*' -> 
        begin
          if n < len - 1 && f.[n + 1] = '.' then
            "*."
          else
            "*"
        end
      |'/' -> "/"
      |'%' -> "%"
      |'.' -> "." ^ (get_number f (n + 1))
      |_ -> 
        begin
          if (is_digit f.[n]) then
            get_number f n
          else
            get_string f n
        end
    end
;;

let contain_dot f =
  let len = (String.length f) in
  let rec aux str n =
    if n = len then
      false
    else
      if str.[n] = '.' then
        true
      else
        aux str (n + 1)
  in aux f 0

let is_letter c =
  match c with
  |'a' .. 'z' -> true
  |_ -> false

let contain_letter f =
  let len = (String.length f) in
  let rec aux str n =
    if n = len then
      false
    else
      if (is_letter str.[n]) then
        true
      else
        aux str (n + 1)
  in aux f 0

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
      |"+." -> Plus_sign_dot::(aux (index + 2) str)
      |"-." -> Minus_sign_dot::(aux (index + 2) str)
      |"*." -> Multiplication_sign_dot::(aux (index + 2) str)
      |_ when (contain_letter (complete_word str index)) -> 
        begin
          let comp_word = (complete_word str index) in
          if comp_word.[0] = '.' then
            Function_lex("0" ^ comp_word)::(aux (index + (String.length comp_word)) str)
          else
            Function_lex(comp_word)::(aux (index + (String.length comp_word)) str)
        end
      |_ when (contain_dot (complete_word str index)) -> Float_lex(float_of_string (complete_word str index))::(aux (index + (String.length (complete_word str index))) str)
      |_ -> Integer_lex(int_of_string (complete_word str index))::(aux (index + (String.length (complete_word str index))) str)
    else []
  in aux 0 f
;;