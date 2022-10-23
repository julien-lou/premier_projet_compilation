open Lexer
open Asyntax

let rec largest_independent_expression lexeme_list =
  let rec aux list_of_lexeme int_stack =
    match list_of_lexeme with
      |[] -> raise (Error "Parser error : lie.")
      |h::[] -> 
        begin match h with
          |Integer_lex(number) -> 
            begin 
              if int_stack = 0 then
                ([Integer_lex(number)], [])
              else 
                raise (Error "Missing right parenthesis.")
            end
          |Plus_sign -> raise (Error "Missing argument in addition.")
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
                ([Integer_lex(number)], t)  (* change here for multiplication*)
              else
                begin 
                  let left, right = (aux t int_stack) in
                  ([Integer_lex(number)] @ left, right)
                end
            end
          |Plus_sign ->
            begin
              let left, right = (aux t int_stack) in
              ([Plus_sign] @ left, right)
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


let syntax_analysis lexeme_list = 
  let rec aux current_list left_exp =
    match current_list with
      |[] -> left_exp
      |h::[] -> 
        begin match h with
          |Integer_lex(number) -> Integer(number)
          |Plus_sign -> raise (Error "The expression is incorrect.")
          |Left_parenthesis -> raise (Error "Missing right parenthesis.")
          |Right_parenthesis -> raise (Error "Parser error : syntax_analysis.")
        end
      |h::t ->  
        begin match h with
          |Integer_lex(number) -> aux t (Integer(number))
          |Plus_sign -> 
            begin 
              match left_exp with
                |Empty ->
                  begin 
                    let independent_exp, rest = largest_independent_expression t in
                    aux rest (Function("Plus", (aux independent_exp Empty)))
                  end
                |_ ->
                  begin 
                    let independent_exp, rest = largest_independent_expression t in
                    aux rest (Operation("Addition", left_exp, (aux independent_exp Empty)))
                  end
            end
          |Left_parenthesis ->
            begin
              let independent_exp,rest = largest_independent_expression current_list in
              aux rest (Parentheses(aux (strip_parentheses independent_exp) Empty))
            end
          |Right_parenthesis -> raise (Error "Missing left parenthesis.")
        end
  in aux lexeme_list Empty
;;


