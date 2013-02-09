open Identifier
open HamletAST

let identifier fmt x = 
  Format.fprintf fmt "%s" (Identifier.as_string x)

let paren fmt f e = 
  Format.fprintf fmt "(%a)" f e

let paren_if pred f fmt e = 
  if (pred e) then 
    paren fmt f e
  else 
    f fmt e

let rec expression fmt e = 
  match e with
  | Var x -> 
      identifier fmt x
  | Int x -> 
      Format.fprintf fmt "%d" x
  | App (Prim IfZ, Tuple [ cond; lhs; rhs ]) ->
      Format.fprintf fmt "if %a = 0 then begin @, @[%a@] () @, end else begin @, @[%a@] () end" 
        expression cond
        expression lhs
        expression rhs
  | App (Prim ((LessThanInt | LessThanFloat) as p), Tuple [ lhs; rhs ]) -> 
      Format.fprintf fmt "(fun x -> if x = true then 1 else 0) (%a %s %a)" 
        (paren_expression p) lhs
        (primitive p)
        (paren_expression p) rhs
  | App (Prim ((Mul | Div | Add | Sub | AddFloat | SubFloat | MulFloat | DivFloat) as p), Tuple [ lhs; rhs ]) -> 
      Format.fprintf fmt "%a %s %a" 
        (paren_expression p) lhs
        (primitive p)
        (paren_expression p) rhs
  | App (f, x) -> 
      Format.fprintf fmt "%a (%a)" expression f expression x
  | Data (d, es) ->
      begin match es with 
	| [] -> Format.fprintf fmt "%a " 
		identifier d
	| _  -> Format.fprintf fmt "%a (%a)" 
		identifier d
		(fun fmt -> Misc.FormatUtils.separated_list fmt ", " expression) es
      end
  | Prim p -> Format.fprintf fmt "%s" (primitive p)
  | Fun (x, e) ->
      Format.fprintf fmt "(fun %a ->@, @[%a@])"
        binder x
        expression e
  | LetRec (xs, es, e) ->
      Format.fprintf fmt "let rec@, @[%a@]@, in @,@[%a@]"
        defs (List.combine xs es)
        expression e
  | Fix (x, e) ->
      Format.fprintf fmt "let rec %a.@, @[%a@]" 
        binder x
        expression e
  | Tuple es ->
      paren fmt 
        (fun fmt -> Misc.FormatUtils.separated_list fmt ", " expression) es
  | Match (s, bs) ->
    Format.fprintf fmt " ( match %a with @\n | @[%a@] )\n"
      expression s
      branches bs
  | ArrayWrite (e, idx, v) ->
    Format.fprintf fmt "(%a.(%a) <- %a; %a)"
      expression e
      expression idx
      expression v
      expression e
  | ArrayRead (e, idx) ->
    Format.fprintf fmt "%a.(%a) "
      expression e
      expression idx
  | Float f -> 
    Format.fprintf fmt "%s" (string_of_float f)
  | Annot (e, t) -> 
    Format.fprintf fmt "(%a : %a)" expression e ty t

and binder fmt = function
  | (x, None) -> identifier fmt x
  | (x, Some t) -> Format.fprintf fmt "(%a : %a)" identifier x ty t

and branches fmt bs = 
  Misc.FormatUtils.separated_list fmt " | " branch bs

and branch fmt (p, e) = 
  Format.fprintf fmt "%a -> %a @\n" 
    pattern p
    expression e

and pattern fmt = function
  | PVar x -> identifier fmt x
  | PData (k, ps) -> 
    begin match ps with 
	| [] -> Format.fprintf fmt "%a " 
		identifier k
	| _  -> Format.fprintf fmt "%a (%a)"
		identifier k
		patterns ps
    end
  | PView (e, p) ->
    Format.fprintf fmt "(%a -> %a)" 
      expression e
      pattern p

and patterns fmt ps =         
  Misc.FormatUtils.separated_list fmt ", " pattern ps

and paren_expression p fmt = function
  (** UPDATE : ajout nouveaux binop **)
  | App (App (Prim ((Mul | Div | Add | Sub | AddFloat | SubFloat | MulFloat | DivFloat | LessThanInt | LessThanFloat) as p'), lhs), rhs) as e ->
      (match p, p' with
         | (Mul | Div), (Add | Sub) -> paren fmt expression e
         | _ -> expression fmt e)
  | e -> expression fmt e

and def fmt (x, e) = 
  Format.fprintf fmt "%a = %a" 
    binder x
    expression e

and defs fmt ms = 
  Misc.FormatUtils.separated_list fmt ", " def ms

and primitive = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | AddFloat -> "+."
  | SubFloat -> "-."
  | MulFloat -> "*."
  | DivFloat -> "/."
  | LessThanInt
  | LessThanFloat -> "<"
  | AllocArray -> "Array.make"
  | Proj _ -> Printf.sprintf "failwith (\"Projections not supported in Ocaml.\")"
  | IfZ -> "if"

and value fmt = function
  | VInt x -> Format.fprintf fmt "%d" x
  | VFix (x, e) -> 
      Format.fprintf fmt "fix %a. %a" 
        identifier x
        expression e
  | VFun (x, e) -> 
      Format.fprintf fmt "fun %a -> %a" 
        identifier x
        expression e
  | VVar x ->
      identifier fmt x
  | VTuple vs ->
      paren fmt (fun fmt -> Misc.FormatUtils.separated_list fmt ", " value) vs

and var_declaration fmt (x, e) =
  Format.fprintf fmt "let %a = %a"
    identifier x
    expression e

and is_arrow = 
  function TyArrow _ -> true | _ -> false

and ty fmt = function
  | TyInt -> Format.fprintf fmt "int"
  | TyFloat -> Format.fprintf fmt "float"
  | TyADT x -> identifier fmt x
  | TyArray t -> Format.fprintf fmt "%a array" ty t
  | TyRecursive (lhs, rhs) ->
     Format.fprintf fmt "%a -> %a" 
      (paren_if is_arrow ty) lhs
      ty rhs
  | TyArrow (lhs, rhs, _, _, _) -> 
    Format.fprintf fmt "%a -> %a" 
      (paren_if is_arrow ty) lhs
      ty rhs
  | TyObject -> Format.fprintf fmt "object"
  | TyTuple ts ->  paren fmt (fun fmt -> Misc.FormatUtils.separated_list fmt "* " ty) ts

let constructor_declaration fmt (d, tys) =
  Format.fprintf fmt "%a%s%a@ "
    identifier d
    (if tys = [] then "" else " of ")
    (fun fmt -> Misc.FormatUtils.separated_list fmt " * " ty) tys

let type_declaration fmt (x, ds) =
  Format.fprintf fmt "@[type %a =@, %a@]"
    identifier x
    (fun fmt -> Misc.FormatUtils.separated_list fmt "| " constructor_declaration) ds

let declaration fmt = function
  | DVar (x, e) -> var_declaration fmt (x, e)
  | DType (x, ds) -> type_declaration fmt (x, ds)

let declarations fmt ds = 
  Format.fprintf fmt "@[%a@]"
    (fun fmt -> Misc.FormatUtils.vertical_list fmt declaration) 
    ds

