%{
  open ClovisAST
  open Position

  let parse_error = Error.error "during parsing"

%}

%token<string> ID
%token<int> INT
%token LET REC PROJ SLASH PLUS MINUS STAR LPAREN RPAREN EQ
%token IFZ THEN ELSE COMMA IN FIX AND ARROW DOT VAL APPLY LBRACE RBRACE
%token EOF CLOSURE LOOKUP PIPE
(** UPDATE ++ **)
%token<string> UID
%token<float> FLOAT
%token SLASHDOT PLUSDOT MINUSDOT STARDOT LBRACKET RBRACKET
%token LT LTDOT
%token END ASSIGN WITH MATCH NEW

%left     PLUS MINUS
%left     STAR SLASH
(** UPDATE ++ **)
%nonassoc LT LTDOT 
%left     PLUSDOT MINUSDOT
%left     STARDOT SLASHDOT

%start<ClovisAST.expression>
  toplevel_expression 

%start<ClovisAST.declaration>
  toplevel_declaration

%%

toplevel_expression: e=expression EOF
{
  e
}
| error
{
  parse_error (Position.lex_join $startpos $endpos) "Syntax error."
}

toplevel_declaration: d=declaration EOF
{
  d
}
| error
{
  parse_error (Position.lex_join $startpos $endpos) "Syntax error."
}

declaration: VAL x=identifier EQ e=expression {
  (x, e)
}

expression: e=expression2 {
  e
}

expression2: 
 CLOSURE LBRACE ds=declaration* PIPE x=identifier ARROW e=expression2 RBRACE
{
  Closure (ds, x, e)
}
| FIX x=identifier DOT e=expression2
{
  Fix (x, e)
}
| LET x=identifier EQ lhs=expression2 IN rhs=expression2
{
  Let ((x, lhs), rhs)
}
| LET REC
  fs=separated_nonempty_list(AND, separated_pair(identifier, EQ, expression2))
  IN
  rhs=expression2
{
  let (xs, es) = List.split fs in
    LetRec (xs, es, rhs)
}
| IFZ cond=expression1 THEN lhs=expression2 ELSE rhs=expression2
{
  Apply (Prim IfZ, Tuple [ cond; lhs; rhs ])
}
(** UPDATE ++ **)
| MATCH s=expression WITH PIPE? bs=separated_list(PIPE, branch) END
{
  Match (s, bs)
}
| e=expression0 LBRACKET idx=expression RBRACKET ASSIGN v=expression1
{
  ArrayWrite (e, idx, v)
}
(** FIN UPDATE ++ **)
| e=expression1 
{
  e
}

expression1: APPLY e1=expression1 e2=expression0 
{
  Apply (e1, e2)
}
| lhs=expression1 op=binop rhs=expression1
{
  Apply (Prim op, Tuple [ lhs; rhs ])
}
(** UPDATE ++ **)
| e=expression0 LBRACKET idx=expression RBRACKET
{
  ArrayRead (e, idx)
}
| d=data_constructor LBRACE es=separated_list(COMMA, expression) RBRACE
{
  Data (d, es)
}
(** FIN UPDATE ++ **)
| e=expression0
{
  e
}

expression0: x=identifier
{
  Var x
}
| LOOKUP x=identifier
{
  Lookup x
}
| x=INT
{
  Int x
}
(** UPDATE ++ **)
| x=FLOAT
{
  Float x
}
| d=data_constructor
{
  Data (d, [])
}
(** FIN UPDATE ++ **)
| LPAREN es=separated_list(COMMA, expression2) RPAREN
{
  match es with
    | [e] -> e
    | es -> Tuple es
}
| PROJ LPAREN x=INT RPAREN
{
  Prim (Proj x)
}
(** UPDATE ++ **)
| NEW 
{
  Prim AllocArray
}

branch: p=pattern ARROW e=expression 
{
  (p, e)
}

pattern: x=identifier
{
  PVar x
}
| x=data_constructor LBRACE ps=separated_list (COMMA, pattern) RBRACE
{
  PData (x, ps)
}
| x=data_constructor
{
  PData (x, [])
}
| LPAREN e=expression ARROW p=pattern RPAREN
{
  PView (e, p)
}
(** FIN UPDATE ++ **)

%inline binop:
  PLUS     { Add }
| MINUS    { Sub }
| STAR     { Mul }
| SLASH    { Div }
| LT       { LessThanInt }
| PLUSDOT  { AddFloat }
| MINUSDOT { SubFloat }
| STARDOT  { MulFloat }
| SLASHDOT { DivFloat }
| LTDOT    { LessThanFloat }

identifier: x=ID
{
  Identifier.mk ClovisIdentifier.value_kind x
}

data_constructor: x=UID
{
  Identifier.mk HamletIdentifier.data_constructor_kind x
}
