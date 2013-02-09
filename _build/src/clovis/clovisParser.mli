exception Error

type token = 
  | WITH
  | VAL
  | UID of (string)
  | THEN
  | STARDOT
  | STAR
  | SLASHDOT
  | SLASH
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | PROJ
  | PLUSDOT
  | PLUS
  | PIPE
  | NEW
  | MINUSDOT
  | MINUS
  | MATCH
  | LTDOT
  | LT
  | LPAREN
  | LOOKUP
  | LET
  | LBRACKET
  | LBRACE
  | INT of (int)
  | IN
  | IFZ
  | ID of (string)
  | FLOAT of (float)
  | FIX
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | COMMA
  | CLOSURE
  | ASSIGN
  | ARROW
  | APPLY
  | AND


val toplevel_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ClovisAST.expression)
val toplevel_declaration: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ClovisAST.declaration)