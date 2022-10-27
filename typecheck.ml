open Asyntax

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