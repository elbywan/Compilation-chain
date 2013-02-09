exception Error

type token = 
  | WITH
  | VAL
  | UID of (string)
  | TYPE
  | TYINT
  | TYFLOAT
  | TYARRAY
  | THEN
  | STARDOT
  | STAR
  | SLASHDOT
  | SLASH
  | SARROW
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | PROJ
  | PLUSDOT
  | PLUS
  | PIPE
  | OF
  | NEW
  | MINUSDOT
  | MINUS
  | MATCH
  | LTDOT
  | LT
  | LPAREN
  | LET
  | LBRACKET
  | LBRACE
  | INT of (int)
  | IN
  | IFZ
  | ID of (string)
  | FUN
  | FLOAT of (float)
  | FIX
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | COMMA
  | COLON
  | ASSIGN
  | ARROW
  | AND


val toplevel_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (HamletAST.declaration list * HamletAST.expression)
val toplevel_declaration: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (HamletAST.declaration)