open Identifier
open ClovisAST

let identifier fmt x = 
  Format.fprintf fmt "%s" (Identifier.as_string x)

let paren fmt f e = 
  Format.fprintf fmt "(%a)" f e

let rec expression fmt e = 
  match e with
  | Var x -> 
      identifier fmt x
  | Lookup x ->
      Format.fprintf fmt "lookup %a" identifier x
  | Int x -> 
      Format.fprintf fmt "%d" x
  | Apply (Prim IfZ, Tuple [ cond; lhs; rhs ]) ->
      Format.fprintf fmt "ifz %a then@, @[%a@]@, else@, @[%a@]" 
        expression cond
        expression lhs
        expression rhs
  | Apply (Prim ((Mul | Div | Add | Sub | AddFloat | SubFloat | MulFloat | DivFloat | LessThanInt | LessThanFloat) as p), Tuple [ lhs; rhs ]) -> 
      Format.fprintf fmt "%a %s %a" 
        (paren_expression p) lhs
        (primitive p)
        (paren_expression p) rhs
  | Apply (f, x) -> 
      Format.fprintf fmt "apply (%a) (%a)" expression f expression x
  | Data (d, es) ->
      Format.fprintf fmt "%a {%a}" 
	identifier d
	(fun fmt -> Misc.FormatUtils.separated_list fmt ", " expression) es
  | Prim p -> Format.fprintf fmt "%s" (primitive p)
  | Closure (ds, x, e) ->
      Format.fprintf fmt "closure { %a | %a =>@, @[%a@] }"
        declarations ds
        identifier x
        expression e
  | Let (d, e) ->
      Format.fprintf fmt "let %a @,in @,@[%a@]" (** Ajout espace après le in **)
        def d
        expression e
  | LetRec (xs, es, e) ->
      Format.fprintf fmt "let rec@, @[%a@]@, in @,@[%a@]" (** Ajout espace après le in **)
        defs (List.combine xs es)
        expression e
  | Fix (x, e) ->
      Format.fprintf fmt "fix %a.@, @[%a@]" 
        identifier x
        expression e
  | Tuple es ->
      paren fmt 
        (fun fmt -> Misc.FormatUtils.separated_list fmt ", " expression) es
  | Match (s, bs) ->
    Format.fprintf fmt "match %a with@, @[%a@]\nend" (** Ajout du END **)
      expression s
      branches bs
  | ArrayWrite (e, idx, v) ->
    Format.fprintf fmt "%a [%a] := %a" 
      expression e
      expression idx
      expression v
  | ArrayRead (e, idx) ->
    Format.fprintf fmt "%a [%a]" 
      expression e
      expression idx
  | Float f -> 
    Format.fprintf fmt "%s" (string_of_float f)

and branches fmt bs = 
  Misc.FormatUtils.separated_list fmt "| " branch bs

and branch fmt (p, e) = 
  Format.fprintf fmt "%a => @[%a@]" 
    pattern p
    expression e

and pattern fmt = function
  | PVar x -> identifier fmt x
  | PData (k, ps) -> 
    Format.fprintf fmt "%a {%a}"
      identifier k
      patterns ps
  | PView (e, p) ->
    Format.fprintf fmt "(%a => %a)" 
      expression e
      pattern p

and patterns fmt ps =         
  Misc.FormatUtils.separated_list fmt ", " pattern ps


and paren_expression p fmt = function
  | Apply (Prim ((Mul | Div | Add | Sub | AddFloat | SubFloat | MulFloat | DivFloat | LessThanInt | LessThanFloat) as p'), Tuple [ lhs; rhs ]) as e ->
      (match p, p' with
         | (Mul | Div), (Add | Sub) -> paren fmt expression e
         | _ -> expression fmt e)
  | e -> expression fmt e

and defs fmt ms = 
  Misc.FormatUtils.separated_list fmt "and " def ms

and def fmt (x, e) = 
  Format.fprintf fmt "%a = %a" 
    identifier x
    expression e

and declarations fmt ms = 
  Misc.FormatUtils.separated_list fmt " " declaration ms

and declaration fmt (x, e) =
  Format.fprintf fmt "val %a = %a"
    identifier x
    expression e

and primitive = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | AddFloat -> "+."
  | SubFloat -> "-."
  | MulFloat -> "*."
  | DivFloat -> "/."
  | LessThanInt -> "<"
  | LessThanFloat -> "<."
  | AllocArray -> "new"
  | Proj i -> Printf.sprintf "proj (%d)" i
  | IfZ -> "ifz"

and value fmt = function
  | VInt x -> Format.fprintf fmt "%d" x
  | VClosure (vs, x, e) -> 
      Format.fprintf fmt "closure { %a | %a => %a }" 
        valuations vs
        identifier x
        expression e
  | VVar x ->
      identifier fmt x
  | VTuple vs ->
      paren fmt (fun fmt -> Misc.FormatUtils.separated_list fmt ", " value) vs

and valuation fmt (x, v) = 
  Format.fprintf fmt "%a = %a" 
    identifier x
    value v

and valuations fmt vs =
  Misc.FormatUtils.separated_list fmt ", " valuation vs
