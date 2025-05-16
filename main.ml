open Ast

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let _ = Parser.program Scanner.token lexbuf in
    print_endline "Parsing succeeded!"
  with
  | Parsing.Parse_error -> print_endline "Syntax error"
  | Failure msg -> Printf.printf "Lexer error: %s\n" msg