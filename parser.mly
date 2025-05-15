/* Ocamlyacc parser for TaP */


%{
open Ast
%}


%token LPAREN RPAREN PLUS MINUS TIMES DIVIDE MOD ASSIGN NL
%token EQ NEQ LT AND OR NOT GT LEQ GEQ
%token IF ELSE WHILE FUN END FOR 
%token RETURN COMMA BREAK CONT STEP NULL IN AS TO AT
%token STRING BOOLEAN MAP SET LIST NUMBER INTEGER
%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <float> NUM_LIT
%token <string> STRING_LIT
%token <string> IDENTIFIER
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%nonassoc IN
%left OR
%left AND
%left EQ NEQ LEQ GEQ
%left LT GT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left AS
%nonassoc AT

%%

program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], []) }
 | vdecl_rule NL decls { (($1 :: fst $3), snd $3) }
 | fdecl_rule NL decls { (fst $3, ($1 :: snd $3)) }

vdecl_list_rule:
  /*nothing*/                   { [] }
  | vdecl_rule NL vdecl_list_rule  { $1 :: $3 }

vdecl_rule:
  typ_rule IDENTIFIER { ($1, $2) }

typ_rule:
  | NUMBER    { Num }
  | BOOLEAN    { Bool }
  | INTEGER { Int }
  | STRING { String }
  | LIST typ_rule { List $2 }

fdecl_rule:
  FUN vdecl_rule LPAREN param_list RPAREN NL vdecl_list_rule stmt_list_rule END FUN
  {
    {
      rtyp = fst $2;
      fname = snd $2;
      formals = $4;
      locals = $7;
      body = $8;
    }
  }

stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule NL stmt_list_rule  { $1::$3 }

stmt_rule:
  expr_rule                                                           { Expr $1         }
  | IF expr_rule NL stmt_list_rule END IF                                    { If ($2, Block $4)     }
  | WHILE expr_rule NL stmt_list_rule END WHILE                              { While ($2, Block $4)  }
  | FOR IDENTIFIER IN expr_rule TO expr_rule optional_step NL stmt_list_rule END FOR  { For ($2, $4, $6, Block $9)}
  | BREAK                                                                    { Break }
  | CONT                                                              { Continue }
  | RETURN expr_rule                        { Return $2 }

optional_step:
    /* nothing */ { None }
    | STEP INT_LIT { Some $2 }

param_list:
    /* nothing */ { [] }
  | typ_rule IDENTIFIER { [($1, $2)] }
  | typ_rule IDENTIFIER COMMA param_list { ($1, $2) :: $4 }

expr_list:
    /* nothing */              { [] }
  | expr_rule                  { [$1] }
  | expr_rule COMMA expr_list { $1 :: $3 }

expr_rule:
  | BOOL_LIT                      { BoolLit $1              }
  | INT_LIT                       { IntLit $1               }
  | NUM_LIT                       { NumLit $1               }
  | STRING_LIT                     { StringLit $1            }
  | IDENTIFIER                    { Id $1                   }
  | expr_rule AS typ_rule         { As ($1, $3)             }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)     }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)     }
  | expr_rule TIMES expr_rule     { Binop ($1, Mult, $3)    }
  | expr_rule DIVIDE expr_rule    { Binop ($1, Div, $3)     }
  | expr_rule MOD expr_rule       { Binop ($1, Mod, $3)     }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3)   }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)     }
  | expr_rule LEQ expr_rule       { Binop ($1, Leq, $3)     }
  | expr_rule GEQ expr_rule       { Binop ($1, Geq, $3)     }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)    }
  | expr_rule GT expr_rule        { Binop ($1, Greater, $3) }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)     }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)      }
  | IDENTIFIER LPAREN expr_list RPAREN { Call ($1, $3) }
  | expr_rule AT expr_rule { At ($1, $3) } /* expr needs to be IntLit */
  | expr_rule IN expr_rule { Contains ($1, $3) } /* membership check */
  | IDENTIFIER ASSIGN expr_rule   { Assign ($1, $3) }
  | list_literal { $1 }
  | LPAREN expr_rule RPAREN { $2 } /* expr_rule can't be empty */

list_literal:
  | LPAREN RPAREN { ListLit [] }
  | LPAREN list_items RPAREN  { ListLit $2 }

list_items:
  | expr_rule COMMA                  { [$1] }
  | expr_rule COMMA list_items       { $1 :: $3 }

/* Example of lists: (), (,), (1,), (1,2,) */
/* Example of non-lists: (1), (1, 2, 3) */