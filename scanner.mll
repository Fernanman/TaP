(* Ocamllex scanner for NanoC *)

{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let integer = digit+
let number = digit* '.' digit+
let alphabet = ['a'-'z' 'A'-'Z']
let identifier = alphabet (alphabet | digit)*
let whitespace = [' ' '\t' '\r']+
let line_terminator = ('\n' | ". ")

(* Do not have comments as of now *)
rule token = parse
  whitespace { token lexbuf } (* Whitespace *)
| line_terminator       { NL }
| '('      { LPAREN }
| ')'      { RPAREN }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "&&"     { AND }
| "||"     { OR }
| '\''     { QUOTE }
(* Literals *)
| integer as lem  { INT_LIT(int_of_string lem) }
(* | number as lem { NUM_LIT(float_of_string lem) } *)
(* Identifiers - Check for keywords first because they are case insensitive so have to standardize them by lowering *)
| identifier as lem { 
    let standard_id = String.lowercase_ascii lem in
    match standard_id with
    | "true" -> BOOL_LIT(true)
    | "false" -> BOOL_LIT(false)
    | "plus" -> PLUS
    | "minus" -> MINUS
    | "mult" -> TIMES
    | "div" -> DIVIDE
    | "mod" -> MOD
    | "is" -> ASSIGN
    | "if" -> IF
    | "else" -> ELSE
    | "for" -> FOR
    | "while" -> WHILE
    | "fun" -> FUN
    | "end" -> END
    | "break" -> BREAK
    | "cont" -> CONT
    | "free" -> FREE
    | "null" -> NULL
    | "at" -> AT
    | "class" -> CLASS
    | "and" -> AND
    | "or" -> OR
    | "not" -> NOT
    | "list" -> LIST
    | "equals" -> EQ
    | "lt" -> LT
    | "gt" -> GT
    | "leq" -> LEQ
    | "geq" -> GEQ
    | "in" -> IN
    | "as" -> AS
    | "string" -> STRING
    | "boolean" -> BOOLEAN
    | "map" -> MAP
    | "set" -> SET
    | "number" -> NUMBER
    | "to" -> TO
    | _ -> IDENTIFIER(standard_id) 
 }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
