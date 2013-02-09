open HamletAST

module Env : sig
  type t

  val empty : t

  type closed_value = 
    | CVInt      of int
    | CVFloat    of float
    | CVArray    of closed_value array
    | CVTuple    of closed_value list
    | CVClosure  of t * var * code
    | CVData     of data_constructor * closed_value list
  and code = 
    | Expression    of expression
    | PrimitiveCode of (closed_value -> closed_value)

  val bind_value      : var -> closed_value -> t -> t
  val change_value    : var -> closed_value -> t -> unit
  val lookup_value    : var -> t -> closed_value

  val show : t -> string
  val show_closed_value : closed_value -> string

  (* Ajout *)
  val is_empty : t -> bool

end = struct

  type t = (var * closed_value ref) list

  and closed_value = 
    | CVInt     of int
    | CVFloat   of float
    | CVArray   of closed_value array
    | CVTuple   of closed_value list
    | CVClosure of t * var * code
    | CVData    of data_constructor * closed_value list

  and code = 
    | Expression    of expression
    | PrimitiveCode of (closed_value -> closed_value)

  let rec show_closed_value = function
    | CVInt x -> string_of_int x
    | CVFloat f -> string_of_float f
    | CVArray arr ->  
	"(" ^ String.concat ", " (Array.to_list (Array.map show_closed_value arr)) ^ ")"
    | CVTuple vs -> 
        "(" ^ String.concat ", " (List.map show_closed_value vs) ^ ")"
    | CVClosure _ ->
      "<abstraction>"
    | CVData (const, closed_value) -> "Data{"^(Identifier.as_string const)^"}:["^ String.concat ", " (List.map show_closed_value closed_value) ^"]"

  let show env = 
    String.concat ";" 
      (List.map (fun (x, v) -> Identifier.as_string x ^ ": " ^ 
                 show_closed_value !v)
         env)

  let empty = []

  let bind_value x v env = 
    (x, ref v) :: env

  let lookup_value x env = 
    ! (List.assoc x env)

  let change_value x v env = 
    (List.assoc x env) := v

  let is_empty env = match env with
    | [] -> true
    | _ -> false

end

let interpretation_error =  Error.global_error "during interpretation"

let mk_closure env x e =
  Env.CVClosure (env, x, e)

let lookup x env = 
  try 
    Env.lookup_value x env
  with Not_found -> 
    interpretation_error
      (Printf.sprintf "The variable `%s' is unbound." (Identifier.as_string x))

(* Ajouts *)
let rec check_match cval pattern = match pattern with
  | PVar var -> true
  | PData (const,plist) -> ( match cval with 
			   | Env.CVData (cst,clvl) -> if cst = const then begin 
						      (List.fold_left2 (fun accu c1 c2 -> if accu = false then false else (check_match c1 c2)) true clvl plist)
						      end
					              else false
			   | _ -> false )
  | PView (_,_) -> interpretation_error "Views not supported (yet...).\n"

let rec compare_matching cval pattern env = match pattern with 
  | PVar var -> Env.bind_value var cval env
 (* match cval with Env.CVar x -> true | _ -> false *)
 (* match Cons (a,list) with | Cons b,l -> *)
  | PData (const,plist) -> (match cval with 
			   | Env.CVData (cst,clvl) -> if cst = const then begin 
						      (List.fold_left2 (fun accu c1 c2 -> (compare_matching c1 c2 accu)) env clvl plist)
						      end
					              else Env.empty
			   | _ -> Env.empty )
  | PView (_,_) -> interpretation_error "Views not supported (yet...).\n"
(* Fin ajouts *)

let rec eval_expression env e = 
  match e with
  | Var x -> 
      lookup x env
  | Int x -> 
      Env.CVInt x
  | App (f, x) ->
      let fonc = eval_expression env f in
      let args = eval_expression env x in
      apply_closure fonc args
  | Fun (x, e) ->
      (mk_closure env (fst x) (Env.Expression e))
  | Prim p -> 
      primitive p
  | Fix (x, e) ->
      let newenv = Env.bind_value (fst x) (Env.CVInt 0) env in
      Env.change_value (fst x) (eval_expression newenv e) newenv;
      eval_expression newenv e
  | Tuple (es) -> 
      let rec loop li accu = match li with
	| []    -> Env.CVTuple (List.rev accu)
	| a::b  -> loop b ((eval_expression env a)::accu)
      in loop es []
  | LetRec (xs, es, e) ->
      let xs = (List.map (fun x -> fst x) xs) in
      let rec loop1 varlist accuenv = match varlist with
	| []   -> accuenv
	| a::b -> loop1 b (Env.bind_value a (Env.CVInt 0) accuenv)
      in let newenv = loop1 xs env 
      in let chg_val x e = Env.change_value x (eval_expression newenv e) newenv
      (**(mk_closure newenv x (Env.Expression e)) 
      let fact = fix fact. (\F \n ifz n then n else (F n (n+1))**)
      in List.iter2 chg_val xs es;
      eval_expression newenv e
  | Match (exp,cl) ->
    let eval = eval_expression env exp in
    let rec loop cl = 
    match cl with
      | []             -> interpretation_error "Unable to match expression.\n"
      | (patt,e)::next -> let newenv    = (compare_matching eval patt env) in
			  let isMatched = (check_match eval patt )         in 
			  if isMatched then (eval_expression newenv e) else (loop next)
    in loop cl
  | ArrayWrite (arr,pos,e) ->
    let eval   = eval_expression env arr in
    let posint = match(eval_expression env pos) with
      | Env.CVInt i -> i
      | _ -> interpretation_error "Array[write] pos can only be int.\n"
    in (
    match eval with 
      | Env.CVArray a -> a.(posint) <- (eval_expression env e); Env.CVArray a
      | _ -> interpretation_error "Only arrays can be written.\n" )
  | ArrayRead  (arr,pos) ->
    let eval   = eval_expression env arr in
    let posint = match(eval_expression env pos) with
      | Env.CVInt i -> i
      | _ -> interpretation_error "Array[read] pos can only be int.\n"
    in (
    match eval with 
      | Env.CVArray a -> a.(posint) 
      | _ -> interpretation_error "Only arrays can be read.\n" )
  | Data (const,elist) ->
      Env.CVData (const, List.map (fun x -> eval_expression env x) elist)
  | Float f ->
      Env.CVFloat f
  | Annot (e,ty) ->
       eval_expression env e

and apply_closure closure v = 
   match closure with
    | Env.CVClosure (env, x, Env.Expression e) -> 
        eval_expression (Env.bind_value x v env) e
    | Env.CVClosure (_, _, Env.PrimitiveCode e) ->
        e v
    | _ -> interpretation_error "Only closures can be applied."

and primitive =
  let mk_primitive f = 
    mk_closure Env.empty (HamletIdentifier.fresh ()) (Env.PrimitiveCode f)
  in
    function
      | Add -> mk_primitive (lift2 ( + ))
      | Sub -> mk_primitive (lift2 ( - ))
      | Mul -> mk_primitive (lift2 ( * ))
      | Div -> mk_primitive (lift2 ( / ))
      | Proj i ->
          mk_primitive 
            (fun t -> 
               match t with
                 | Env.CVTuple ts -> List.nth ts i
                 | _ -> interpretation_error "Projection only work on tuples.")
      | IfZ ->
          mk_primitive
            (fun t ->
               match t with
                 | Env.CVTuple [ cond; lhs; rhs ] ->
                     (match cond with
		       | Env.CVFloat 0. 
                       | Env.CVInt 0 -> 
                           apply_closure lhs (Env.CVInt 0)
		       | Env.CVFloat _ 
                       | Env.CVInt _ -> 
                           apply_closure rhs (Env.CVInt 0)
                       | _ -> interpretation_error 
                           "Tests only work on integers.")
                 | _ -> assert false (* By syntax. *))
      | AllocArray ->
	  (** app(x,app(y,arr(x,y))) **)
	  mk_primitive 
	    (fun t -> match t with
		| Env.CVInt size -> 
		    mk_primitive (fun e -> Env.CVArray (Array.make size e))
		| _ -> interpretation_error "Array size can only be evaluated as int.\n")
      | LessThanInt ->
	  mk_primitive (lift3 ( < ))
      | LessThanFloat ->
	  mk_primitive (lift3_float ( < ))
      | DivFloat ->
	  mk_primitive (lift2_float ( /. ))
      | MulFloat ->
	  mk_primitive (lift2_float ( *. ))
      | AddFloat ->
	  mk_primitive (lift2_float ( +. ))
      | SubFloat ->
	  mk_primitive (lift2_float ( -. ))
and lift2 f v = 
  match v with
    | Env.CVTuple [Env.CVInt x; Env.CVInt y] -> Env.CVInt (f x y)
    | _ -> interpretation_error 
        "Expecting two integers in arithmetic operation"
and lift2_float f v = 
  match v with
    | Env.CVTuple [Env.CVFloat x; Env.CVFloat y] -> Env.CVFloat (f x y)
    | _ -> interpretation_error 
        "Expecting two floats in arithmetic operation"
and lift3 f v = 
  match v with
    | Env.CVTuple [Env.CVInt x; Env.CVInt y] -> Env.CVInt (if (f x y) then 1 else 0)
    | _ -> interpretation_error 
        "Expecting two integers in arithmetic operation"
and lift3_float f v = 
  match v with
    | Env.CVTuple [Env.CVFloat x; Env.CVFloat y] -> Env.CVInt (if (f x y) then 1 else 0)
    | _ -> interpretation_error 
        "Expecting two floats in arithmetic operation"

let eval_declaration env (x, e) = 
  let v = eval_expression env e in
    (Env.bind_value x v env, v)

let eval =
  let env = ref Env.empty in
    fun (runtime : RuntimeSystem.t) e ->
      let env', v = eval_declaration !env e in
        env := env';
        runtime, [Env.show_closed_value v]
