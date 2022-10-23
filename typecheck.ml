open Asyntax

(* gives the type of the AST *)
let rec static_analysis tree =
  match tree with
  |Empty -> "empty"
  |Parentheses(subtree) -> static_analysis subtree
  |Integer(number) -> "integer"
  |Function(fn_name, arg) ->
    begin match fn_name with
      |"Plus" -> static_analysis arg
      |_ -> raise (Error (Printf.sprintf "The function %s is not defined." fn_name))
    end
  |Operation(op_name, arg1, arg2) ->
    begin match op_name with
      |"Addition" ->
        begin
          let type1 = static_analysis arg1
          and type2 = static_analysis arg2 in
          if type1 = type2 && type1 = "integer" then
            "integer"
          else
            raise (Error "The operator + only applies to integers.")
        end
      |_ -> raise (Error (Printf.sprintf "The operator %s is not recognised." op_name))
    end
;;