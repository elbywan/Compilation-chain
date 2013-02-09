exception Error

type token = 
  | THEN
  | STARDOT
  | STAR
  | SLASHDOT
  | SLASH
  | RPAREN
  | RBRACKET
  | PLUSDOT
  | PLUS
  | NEW
  | MINUSDOT
  | MINUS
  | LTDOT
  | LT
  | LPAREN
  | LET
  | LEFTARROW
  | LBRACKET
  | INT of (int)
  | IN
  | IFZ
  | ID of (string)
  | FUN
  | FLOAT of (float)
  | FID of (string)
  | EQ
  | EOF
  | ELSE
  | COMMA


val toplevel_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (WillowAST.expression)
val toplevel_declaration: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (WillowAST.function_declaration)
val compilation_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (WillowAST.program)