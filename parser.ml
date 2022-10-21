exception Error of string

let rec syntax_analysis lexeme_list =

  



and full_expression incomplete_lexeme_list =


let rec largest_independent_expression lexeme_list =
  let rec aux list_of_lexeme int_stack =
    match list_of_lexeme with
      |[] -> raise (Error "Parser error.")
      |h::[] -> 
        begin match h with
          |Integer(number) -> 
            begin 
              if int_stack = 0 then
                [Integer(number)]
              else 
                raise (Error "Missing parentheses.")
            end
          |Plus_sign -> raise (Error "The expression is incorrect.")
          |Left_parenthesis -> raise (Error "Missing parentheses.")
          |Right_parenthesis -> 
            begin 
              if int_stack = 1 then
                [Right_parenthesis]
              else 
                raise (Error "Missing parentheses.")
            end
        end
      |h::t -> 
        begin match h with
          |Integer(number) ->
            begin
              if int_stack = 0 then
                [Integer(number)]  (* change here for multiplication*)
              else
                [Integer(number)] @ (aux t int_stack)
            end
          |Plus_sign -> [Plus_sign] @ (aux t int_stack)
          |Left_parenthesis -> [Left_parenthesis] @ (aux t (int_stack + 1))
          |Right_parenthesis -> 
            begin
              if int_stack <= 0 then
                raise (Error "Too many right parentheses.")
              else if int_stack = 1 then
                [Right_parenthesis]
              else
                [Right_parenthesis] @ (aux t (int_stack - 1))
            end
        end
  in aux lexeme_list 0
;;
        

