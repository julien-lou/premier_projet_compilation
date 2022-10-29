(*
  Parser
  Do the syntax analysis on the list of lexemes and return an AST (Abstract Syntax Tree)
*)

open Lexer
open Asyntax

let rec largest_independent_expression lexeme_list = (* get the largest independent expression (in parentheses or number) starting from the first element of the list *)
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

let rec largest_priority_expression lexeme_list = (* get the largest expression that is executed before a sum or a substraction starting from the list *)
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

let tail l=
  match l with
  |[] -> []
  |h::t -> t

  let syntax_analysis lexeme_list = (* return an AST from a list of lexemes *)
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
                      aux rest (Operation("Division_int", left_exp, (aux independent_exp Empty)))
                    end
              end
            |Modulus_sign -> 
              begin 
                match left_exp with
                  |Empty -> raise (Error "Missing left argument in modulus.")
                  |_ ->
                    begin 
                      let independent_exp, rest = largest_independent_expression t in
                      aux rest (Operation("Modulo_int", left_exp, (aux independent_exp Empty)))
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