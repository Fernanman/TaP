open Ast

module StringMap = Map.Make(String);;

(* Accepts StringMap, expr as args now *)
let rec eval mapping = function
      (* Pattern matching the function arguments *)
        Lit(x) -> (mapping, x)
      | Var(indentifier) -> (mapping, StringMap.find indentifier mapping)
      | Asn(indentifier, eq_expr) -> let (eq_mapping, eq_value) = eval mapping eq_expr in
            (eq_mapping |> StringMap.add indentifier eq_value, eq_value)   
      | Seq(expr1, expr2) -> let (new_mapping, _) = eval mapping expr1 in
            eval new_mapping expr2
      | Binop(e1, op, e2) ->
            let (_, v1)  = eval mapping e1 in
            let (_, v2) = eval mapping e2 in
            (match op with
                  Add -> (mapping, v1 + v2)
                  | Sub -> (mapping, v1 - v2)
                  | Mul -> (mapping, v1 * v2)
                  | Div -> (mapping, v1 / v2))

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let (_, result) = eval StringMap.empty expr in
  print_endline (string_of_int result)
