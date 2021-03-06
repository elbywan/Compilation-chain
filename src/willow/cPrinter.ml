open Identifier
open WillowAST

let identifier fmt x =
  Format.fprintf fmt "%s" (Identifier.as_string x)

let function_identifier fmt x =
  let s = (Identifier.as_string x) in
  Format.fprintf fmt "%s" (String.sub s 1 ((String.length s)-1))

let paren fmt f e =
  Format.fprintf fmt "(%a)" f e

let rec expression fmt e =
  match e with
  | Var x ->
      identifier fmt x

  | FVar x ->
      function_identifier fmt x

  | Int x ->
      Format.fprintf fmt "%d" x

  | Float f -> 
      Format.fprintf fmt "(int) %s" (string_of_float f)

  | Prim p -> Format.fprintf fmt "%s" (primitive p)

  | Let (x, lhs, rhs) ->
      Format.fprintf fmt "void** %a = @[%a@]; @\n@[%a@]"
        identifier x
        expression lhs
        expression rhs

  | BlockAlloc ts ->
      Format.fprintf fmt "(void * []) { %a }" block ts

  | BlockRead (e, i) ->
      Format.fprintf fmt "((void **) %a[%a])"
        expression e
        expression i

  | BlockWrite (e, i, rvalue) ->
      Format.fprintf fmt "%a; %a[%a] = %a"
        expression e
	expression e
        expression i
        expression rvalue

  | Call (Prim AllocArray, BlockAlloc _, BlockAlloc [size ; map]) -> 
      Format.fprintf fmt "void** tmp[%a] = {%a};"
        expression size
	expression map

  | Call (Prim IfZ, _, BlockAlloc [ cond; lhs; rhs ]) ->
      Format.fprintf fmt "if (%a == 0){ @,@[%a;@] } else { @[%a;@] }"
        expression cond
        expression lhs
        expression rhs

  | Call (Prim ((Mul | Div | Add | Sub | LessThanInt) as p), _, BlockAlloc [ lhs; rhs ]) ->
      Format.fprintf fmt "((int) (%a)) %s ((int) (%a))"
        (paren_expression p) lhs
        (primitive p)
        (paren_expression p) rhs

  | Call (Prim ((AddFloat | SubFloat | MulFloat | DivFloat | LessThanFloat) as p), _, BlockAlloc [ lhs; rhs ]) ->
      Format.fprintf fmt "((int) (%a)) %s ((int) (%a))"
        (paren_expression p) lhs
        (primitive p)
        (paren_expression p) rhs

  | Call (f, env, x) ->
      Format.fprintf fmt "((fp) %a) (%a, %a)"
        expression f
        expression env
        expression x

and paren_expression p fmt = function
  | Call (Prim ((Mul | Div | Add | Sub | AddFloat | SubFloat | MulFloat | DivFloat | LessThanInt | LessThanFloat) as p'), _, BlockAlloc [ lhs; rhs ])
      as e ->
      (match p, p' with
         | (Mul | Div), (Add | Sub) -> paren fmt expression e
         | _ -> expression fmt e)
  | e -> expression fmt e

and block fmt ms =
  Misc.FormatUtils.separated_list fmt ", " expression ms

and def fmt (x, e) =
  Format.fprintf fmt "%a = %a"
    identifier x
    expression e

and primitive = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | IfZ -> "if"
  | AddFloat -> "+"
  | SubFloat -> "-"
  | MulFloat -> "*"
  | DivFloat -> "/"
  | LessThanInt -> "<"
  | LessThanFloat -> "<"
  | AllocArray -> "new"

and function_declaration fmt (f, env, x, body) =
  Format.fprintf fmt "@[void ** %a(void ** %a, void ** %a){  @,@[<hov 2>%a@]@] ;@, }"
    function_identifier f
    identifier env
    identifier x
    expression body

and function_declarations fmt fdefs =
  Misc.FormatUtils.separated_list fmt " " function_declaration fdefs

and program fmt (fdefs, e) =
  Format.fprintf fmt "@[%a@]@ @,@[%a@]"
    function_declarations fdefs
    preambule e

and preambule fmt e =
  Format.fprintf fmt "@\nint main(){ @\n @[%a;@] @\n}@\n"
    expression e

and create_h (fdefs, e) =
  let rec loop fdefs = match fdefs with
    | [] -> ""
    | (fvar,var1,var2,_)::next -> ("\nvoid ** "^(String.sub (Identifier.as_string fvar) 1 ((String.length (Identifier.as_string fvar)) - 1))^"(void ** "^(Identifier.as_string var1)^", void ** "^(Identifier.as_string var2)^");")^(loop next)
  in (loop fdefs)

