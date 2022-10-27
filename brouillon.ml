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
          if n < len - 1 && f.[n + 1] = '.' then
            "+."
          else
            "+"
        end
      |'-' ->
        begin
          if n < len - 1 && f.[n + 1] = '.' then
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

let pre_lexical_analysis f =
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

let is_next_number lexeme_list =
  match lexeme_list with
  |[] -> false
  |h::t ->
    begin
      match h with
      |Integer_lex(_) -> true
      |Float_lex(_) -> true
      |_ -> false
    end

let specific_cases lexeme_list = (* 1++1 is equal to 1 + (+1) *)
  let rec aux list_of_lexeme str_sign no_operator_before =
    match list_of_lexeme with
    |[] -> []
    |h::t ->
      match h with
      |Integer_lex(number) ->
        begin
          if str_sign = "" then
            Integer_lex(number)::(aux t "" true)
          else
            Integer_lex(int_of_string (str_sign ^ (string_of_int number)))::(aux t "" true)
        end
      |Float_lex(number) ->
        begin
          if str_sign = "" then
            Float_lex(number)::(aux t "" true)
          else
            Float_lex(float_of_string (str_sign ^ (string_of_float number)))::(aux t "" true)
        end
      |Plus_sign ->
        begin
          if (no_operator_before) then
            Plus_sign::(aux t "" false)
          else if (is_next_number t) then
            (aux t "+" false)
          else
            Plus_sign::(aux t "+" false)
        end
      |Minus_sign ->
        begin
          if (no_operator_before) then
            Minus_sign::(aux t "" false)
          else if (is_next_number t) then
            (aux t "-" false)
          else
            Plus_sign::(aux t "-" false)
        end
      |Right_parenthesis -> Right_parenthesis::(aux t "" true)
      |_ -> h::(aux t "" false)
  in aux lexeme_list "" false


let lexical_analysis f =
  (specific_cases (pre_lexical_analysis f))

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

let rec largest_independent_expression lexeme_list =
  let rec aux list_of_lexeme int_stack =
    match list_of_lexeme with
      |[] -> ([], [])
      |h::[] -> 
        begin match h with
          |Integer_lex(number) -> 
            begin 
              if int_stack = 0 then
                ([Integer_lex(number)], [])
              else 
                raise (Error "Missing right parenthesis.")
            end
          |Float_lex(number) -> 
            begin 
              if int_stack = 0 then
                ([Float_lex(number)], [])
              else 
                raise (Error "Missing right parenthesis.")
            end
          |Plus_sign -> raise (Error "Missing argument in addition.")
          |Minus_sign -> raise (Error "Missing argument in substraction.")
          |Multiplication_sign -> raise (Error "Missing argument in multiplication.")
          |Division_sign -> raise (Error "Missing argument in division.")
          |Modulus_sign -> raise (Error "Missing argument in modulo.")
          |Plus_sign_dot -> raise (Error "Missing argument in addition.")
          |Minus_sign_dot -> raise (Error "Missing argument in substraction.")
          |Multiplication_sign_dot -> raise (Error "Missing argument in multiplication.")
          |Function_lex(fn_name) -> raise (Error (Printf.sprintf "Missing argument in function %s." fn_name))
          |Left_parenthesis -> raise (Error "Missing right parenthesis.")
          |Right_parenthesis -> 
            begin 
              if int_stack = 1 then
                ([Right_parenthesis], [])
              else 
                raise (Error "Missing right parenthesis.")
            end
        end
      |h::t -> 
        begin match h with
          |Integer_lex(number) ->
            begin
              if int_stack = 0 then
                ([Integer_lex(number)], t)  
              else
                begin 
                  let left, right = (aux t int_stack) in
                  ([Integer_lex(number)] @ left, right)
                end
            end
          |Float_lex(number) ->
            begin
              if int_stack = 0 then
                ([Float_lex(number)], t)  
              else
                begin 
                  let left, right = (aux t int_stack) in
                  ([Float_lex(number)] @ left, right)
                end
            end
          |Plus_sign ->
            begin
              let left, right = (aux t int_stack) in
              ([Plus_sign] @ left, right)
            end
          |Minus_sign ->
            begin
              let left, right = (aux t int_stack) in
              ([Minus_sign] @ left, right)
            end
          |Multiplication_sign ->
            begin
              let left, right = (aux t int_stack) in
              ([Multiplication_sign] @ left, right)
            end
          |Division_sign ->
            begin
              let left, right = (aux t int_stack) in
              ([Division_sign] @ left, right)
            end
          |Modulus_sign ->
            begin
              let left, right = (aux t int_stack) in
              ([Modulus_sign] @ left, right)
            end
          |Plus_sign_dot ->
            begin
              let left, right = (aux t int_stack) in
              ([Plus_sign_dot] @ left, right)
            end
          |Minus_sign_dot ->
            begin
              let left, right = (aux t int_stack) in
              ([Minus_sign_dot] @ left, right)
            end
          |Multiplication_sign_dot ->
            begin
              let left, right = (aux t int_stack) in
              ([Multiplication_sign_dot] @ left, right)
            end
          |Function_lex(fn_name) ->
            begin
              let left, right = (aux t int_stack) in
              ([Function_lex(fn_name)] @ left, right)
            end
          |Left_parenthesis ->
            begin
              let left, right = (aux t (int_stack + 1)) in
              ([Left_parenthesis] @ left, right)
            end
          |Right_parenthesis -> 
            begin
              if int_stack <= 0 then
                raise (Error "Too many right parentheses.")
              else if int_stack = 1 then
                ([Right_parenthesis], t)
              else
                begin
                  let left, right = (aux t (int_stack - 1)) in
                  ([Right_parenthesis] @ left, right)
                end
            end
        end
  in aux lexeme_list 0
;;

let rec largest_priority_expression lexeme_list =
  let rec aux list_of_lexeme left =
    let independent, rest = largest_independent_expression list_of_lexeme in
    match rest with
    |[] -> (left @ independent, rest)
    |h::t ->
      begin match h with
        |Plus_sign |Minus_sign | Plus_sign_dot |Minus_sign_dot -> (left @ independent, rest) (* no priority *)
        |_ -> (aux t (left @ independent @ [h]))
      end
    in aux lexeme_list []
;;

let strip_parentheses lexeme_list =
  let rec strip_right_parenthesis lex_list =
    match lex_list with
      |[] -> []
      |h::[] -> []
      |h::t -> [h] @ (strip_right_parenthesis t)
  in
  match lexeme_list with
    |[] -> []
    |h::t -> (strip_right_parenthesis t)
;;

let head l =
  match l with
  |[] -> Integer_lex(0)
  |h::t -> h

let syntax_analysis lexeme_list = 
  let rec aux current_list left_exp =
    match current_list with
      |[] -> left_exp
      |h::[] -> 
        begin match h with
          |Integer_lex(number) -> 
            begin
              if left_exp = Empty then
                Integer(number)
              else
                raise (Error "Missing operator between arguments.")
            end
          |Float_lex(number) ->
            begin
              if left_exp = Empty then
                Float(number)
              else
                raise (Error "Missing operator between arguments.")
            end
          |Plus_sign -> raise (Error "Missing argument in addition.")
          |Minus_sign -> raise (Error "Missing argument in substraction.")
          |Multiplication_sign -> raise (Error "Missing argument in multiplication.")
          |Division_sign -> raise (Error "Missing argument in division.")
          |Modulus_sign -> raise (Error "Missing argument in modulus.")
          |Plus_sign_dot -> raise (Error "Missing argument in addition.")
          |Minus_sign_dot -> raise (Error "Missing argument in substraction.")
          |Multiplication_sign_dot -> raise (Error "Missing argument in multiplication.")
          |Function_lex(fn_name) -> raise (Error (Printf.sprintf "Missing argument in function %s." fn_name))
          |Left_parenthesis -> raise (Error "Missing right parenthesis.")
          |Right_parenthesis -> raise (Error "Parser error : syntax_analysis.")
        end
      |h::t ->  
        begin match h with
          |Integer_lex(number) -> 
            begin
              if left_exp = Empty then
                aux t (Integer(number))
              else
                raise (Error "Missing operator between arguments.")
            end
          |Float_lex(number) ->
            begin
              if left_exp = Empty then
                aux t (Float(number))
              else
                raise (Error "Missing operator between arguments.")
            end
          |Plus_sign -> 
            begin 
              match left_exp with
                |Empty ->
                  begin
                    match (head t) with
                    |Left_parenthesis ->
                      begin
                        let priority_exp, rest = largest_priority_expression t in
                        aux rest (Function("Plus", (aux priority_exp Empty)))
                      end
                    |Integer_lex(number) ->
                      begin
                        let priority_exp, rest = largest_priority_expression t in
                        aux rest (Function("Plus", (aux priority_exp Empty)))
                      end
                    |Float_lex(number) ->
                      begin
                        let priority_exp, rest = largest_priority_expression t in
                        aux rest (Function("Plus", (aux priority_exp Empty)))
                      end
                    |_ -> raise (Error "Missing parentheses after addition sign.")
                  end
                |_ ->
                  begin 
                    let priority_exp, rest = largest_priority_expression t in
                    aux rest (Operation("Addition_int", left_exp, (aux priority_exp Empty)))
                  end
            end
          |Plus_sign_dot ->
            begin 
              match left_exp with
                |Empty -> raise (Error "Missing parentheses after addition sign.")
                |_ ->
                  begin 
                    let priority_exp, rest = largest_priority_expression t in
                    aux rest (Operation("Addition_float", left_exp, (aux priority_exp Empty)))
                  end
            end
          |Minus_sign -> 
            begin 
              match left_exp with
                |Empty ->
                  begin
                    match (head t) with
                    |Left_parenthesis ->
                      begin
                        let priority_exp, rest = largest_priority_expression t in
                        aux rest (Function("Minus", (aux priority_exp Empty)))
                      end
                    |Integer_lex(number) ->
                      begin
                        let priority_exp, rest = largest_priority_expression t in
                        aux rest (Function("Minus", (aux priority_exp Empty)))
                      end
                    |Float_lex(number) ->
                      begin
                        let priority_exp, rest = largest_priority_expression t in
                        aux rest (Function("Minus", (aux priority_exp Empty)))
                      end
                    |_ -> raise (Error "Missing parentheses after substraction sign.")
                  end
                |_ ->
                  begin 
                    let priority_exp, rest = largest_priority_expression t in
                    aux rest (Operation("Substraction_int", left_exp, (aux priority_exp Empty)))
                  end
            end
          |Minus_sign_dot ->
            begin 
              match left_exp with
                |Empty -> raise (Error "Missing parentheses after substraction sign.")
                |_ ->
                  begin 
                    let priority_exp, rest = largest_priority_expression t in
                    aux rest (Operation("Substraction_float", left_exp, (aux priority_exp Empty)))
                  end
            end
          |Multiplication_sign -> 
            begin 
              match left_exp with
                |Empty -> raise (Error "Missing left argument in multiplication.")
                |_ ->
                  begin 
                    let independent_exp, rest = largest_independent_expression t in
                    aux rest (Operation("Multiplication_int", left_exp, (aux independent_exp Empty)))
                  end
            end
          |Multiplication_sign_dot ->
            begin 
              match left_exp with
                |Empty -> raise (Error "Missing left argument in multiplication.")
                |_ ->
                  begin 
                    let independent_exp, rest = largest_independent_expression t in
                    aux rest (Operation("Multiplication_float", left_exp, (aux independent_exp Empty)))
                  end
            end
          |Division_sign -> 
            begin 
              match left_exp with
                |Empty -> raise (Error "Missing left argument in division.")
                |_ ->
                  begin 
                    let independent_exp, rest = largest_independent_expression t in
                    aux rest (Operation("Division", left_exp, (aux independent_exp Empty)))
                  end
            end
          |Modulus_sign -> 
            begin 
              match left_exp with
                |Empty -> raise (Error "Missing left argument in modulus.")
                |_ ->
                  begin 
                    let independent_exp, rest = largest_independent_expression t in
                    aux rest (Operation("Modulo", left_exp, (aux independent_exp Empty)))
                  end
            end
          |Function_lex(fn_name) ->
            begin 
              match left_exp with
                |Empty ->
                  begin 
                    if (head t) = Left_parenthesis then
                      let priority_exp, rest = largest_priority_expression t in
                      aux rest (Function(fn_name, (aux priority_exp Empty)))
                    else
                      raise (Error (Printf.sprintf "Missing parentheses after function %s." fn_name))
                  end
                |_ -> raise (Error "Missing operator between arguments.")
            end
          |Left_parenthesis ->
            begin
              let independent_exp,rest = largest_independent_expression current_list in
              if (not (left_exp = Empty)) then
                raise (Error "Missing operator between arguments.")
              else
                aux rest (Parentheses(aux (strip_parentheses independent_exp) Empty))
            end
          |Right_parenthesis -> raise (Error "Missing left parenthesis.")
        end
  in aux lexeme_list Empty
;;


(* gives the type of the AST *)
let rec static_analysis tree =
  match tree with
  |Empty -> "empty"
  |Parentheses(subtree) -> static_analysis subtree
  |Integer(number) -> "integer"
  |Float(number) -> "float"
  |Function(fn_name, arg) ->
    begin match fn_name with
      |"Plus" -> static_analysis arg
      |"Minus" -> static_analysis arg
      |"int" ->
        begin
          let type_arg = (static_analysis arg) in
          if type_arg = "empty" then
            raise (Error "The function int has no argument.")
          else if type_arg = "integer" then
            raise (Error "The function int cannot be applied on an integer.")
          else
            "integer"
        end
      |"float" ->
        begin
          let type_arg = (static_analysis arg) in
          if type_arg = "empty" then
            raise (Error "The function float has no argument.")
          else if type_arg = "float" then
            raise (Error "The function int cannot be applied on a float.")
          else
            "float"
        end
      |_ -> raise (Error (Printf.sprintf "The function %s is not defined." fn_name))
    end
  |Operation(op_name, arg1, arg2) ->
    begin match op_name with
      |"Addition_int" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "integer" then
            "integer"
          else
            raise (Error "The operator + only applies to integers.")
        end
      |"Substraction_int" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "integer" then
            "integer"
          else
            raise (Error "The operator - only applies to integers.")
        end
      |"Multiplication_int" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "integer" then
            "integer"
          else
            raise (Error "The operator * only applies to integers.")
        end
      |"Division_int" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "integer" then
            "integer"
          else
            raise (Error "The operator / only applies to integers.")
        end
      |"Modulo_int" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "integer" then
            "integer"
          else
            raise (Error "The operator % only applies to integers.")
        end
      |"Addition_float" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "float" then
            "float"
          else
            raise (Error "The operator +. only applies to floats.")
        end
      |"Substraction_float" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "float" then
            "float"
          else
            raise (Error "The operator -. only applies to floats.")
        end
      |"Multiplication_float" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "float" then
            "float"
          else
            raise (Error "The operator *. only applies to floats.")
        end
      |_ -> raise (Error (Printf.sprintf "The operator %s is not recognised." op_name))
    end
;;


let formula = "3 + 4 * 5"
;;