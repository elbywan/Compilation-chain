type expression = 
  | Var of var
  | Int of int
  | Lookup of var
  | Let of declaration * expression
  | Apply of expression * expression
  | Closure of declaration list * var * expression
  | Fix of var * expression
  | LetRec of var list * expression list * expression
  | Tuple of expression list
  | Prim of primitive
  (** Updates **)
  | Float of float
  | ArrayRead  of expression * expression
  | ArrayWrite of expression * expression * expression
  | Data of data_constructor * expression list
  | Match of expression * clause list

and clause = pattern * expression
and data_constructor = Identifier.t

and pattern = 
  | PVar of var
  | PData of data_constructor * pattern list
  | PView of expression * pattern

and primitive = 
  | Add
  | Sub
  | Mul
  | Div
  | AddFloat
  | SubFloat
  | MulFloat
  | DivFloat
  | IfZ
  | LessThanInt
  | LessThanFloat
  | Proj of int
  | AllocArray

and value = 
  | VVar     of var
  | VInt     of int
  | VClosure of valuation list * var * expression
  | VTuple   of value list

and declaration = var * expression

and valuation = var * value

and var = Identifier.t
