open HamletAST

module Env = GenericEnvironment.Make (struct
                                           let raise_error x = raise Not_found
                                         end)

module DataEnv = GenericEnvironment.Make (struct
                                           let raise_error x = raise Not_found
                                         end)

let five_fst q = match q with | (a,b,c,d,e) -> a
let five_snd q = match q with | (a,b,c,d,e) -> b
let five_thr q = match q with | (a,b,c,d,e) -> c
let five_fou q = match q with | (a,b,c,d,e) -> d
let five_fiv q = match q with | (a,b,c,d,e) -> e

let typecheck_error =  Error.global_error "during type checking"

let lookup what lookup x env =
  try
    lookup env x
  with Not_found ->
    typecheck_error
      (Printf.sprintf "The %s `%s' is unbound."
         what
         (Identifier.as_string x)
      )

let lookup_var : var -> ty Env.t -> ty =
  lookup "var" Env.lookup

let lookup_data :
    var -> (tyvar * constructor_declaration list) DataEnv.t -> (tyvar * constructor_declaration list) =
  lookup "data" DataEnv.lookup

let rec show_typed_value = function
    | TyInt -> "int"
    | TyFloat -> "float"
    | TyArrow (t1, t2,_,_,_) -> (show_typed_value t1) ^ " -> " ^ (show_typed_value t2)
    | TyRecursive (t1, t2) -> (show_typed_value t1) ^ " -[rec]-> " ^ (show_typed_value t2)
    | TyArray arr -> "Array [ " ^ (show_typed_value arr) ^ " ]"
    | TyADT const -> (Identifier.as_string const)
    | TyObject -> "object"
    | TyTuple ts -> "(" ^ String.concat " * " (List.map show_typed_value ts) ^ ")"

let mismatch_error s t1 t2 = typecheck_error ("Type mismatch : [" ^ (show_typed_value t1) ^ "] & [" ^ (show_typed_value t2) ^ "] (source : "^s^")")
let function_error f       = typecheck_error ("Function type problem : " ^ (show_typed_value f))
let function_rec_error()   = typecheck_error  "Letrec bad format, only functions allowed with argument type given.\n"
let data_cons_error cons   = typecheck_error ("Constructor ["^(Identifier.as_string cons)^"] is unknown.")
let data_type_error typ    = typecheck_error ("Type {"^(Identifier.as_string typ)^"} is unknown.")
let array_alloc_error typ  = typecheck_error ( "Cannot allocate or access if type is not Array. ("^show_typed_value typ^")\n")
let matching_error ty cs   = typecheck_error ("Matching error, constructor ["^(Identifier.as_string cs)^"] is not a member of type ["^(show_typed_value ty)^"]")

let rec type_expression env dataenv e = 
  match e with
  | Var x -> 
      lookup_var x env
  | Int _ -> 
      TyInt
  | Float _ ->
      TyFloat
  | App(App (Prim p, x), v) ->
      type_primitives p (App (x,v)) env dataenv
  | App (Prim p, x) ->
      type_primitives p x env dataenv
  | App (f, x) ->
      let fun_type  = type_expression env dataenv f in
      let check     = ref false 		    in
      let fiv_types = (match fun_type with | TyArrow     (a,b,c,d,e) -> (a,b,c,d,e) 
					   | TyRecursive (a,b)       -> check := true; let dummy = HamletIdentifier.fresh() in (a,b,Var dummy,dummy,Env.empty)
					   | TyObject                -> check := true; let dummy = HamletIdentifier.fresh() in (TyObject,TyObject,Var dummy,dummy,Env.empty)
					   | _ 		             -> function_error fun_type) in
      let oldenv = five_fiv fiv_types in
      let arg_type  = check_type "App" (five_fst fiv_types) (type_expression env dataenv x) dataenv in
      if !check then let t = five_snd fiv_types in (*print_string ("T : "^(show_typed_value t)^"\n");*)t else begin
      let ret_type  = check_type "App" (five_snd fiv_types) (type_expression (Env.bind oldenv (five_fou fiv_types) arg_type) dataenv (five_thr fiv_types)) dataenv in
      ret_type end
  | Fun (x, e) ->
      begin
      match x with 
	 | (x,None)       -> TyArrow (TyObject,TyObject,e,x,env)
	 | (x,Some thing) -> TyArrow (thing,TyObject,e,x,env) 
      end
  | LetRec (xs, es, e) ->
      let newenv = List.fold_left2 (fun env b e -> let ret_type = ( match b with 
								      | (_,None)       -> TyObject
								      | (_,Some thing) -> thing) in
						   let arg_type = ( match e with
								      | Fun (v,e) -> (
								      match v with 
									| (_,Some thing) -> thing
									|  _             -> function_rec_error () )
								      | _ -> function_rec_error ()
						   ) in (Env.bind env (fst b) (TyRecursive(arg_type,ret_type)))) env xs es
      in  
      let fold_func_left = (fun env b e -> match e with | Fun (v,e) ->
				    let arg_type     = (match (snd v) with | Some x -> x | None -> function_rec_error ()) in
				    let env_with_arg = Env.bind env (fst v) arg_type in
				    let ret_type     = type_expression env_with_arg dataenv e in
				    let previous     = lookup_var (fst b) env in (
				    match previous with | TyRecursive (arg,ret) -> 
				    let ret_type = check_type "LetRec" ret ret_type dataenv in 
				    Env.bind env (fst b) (TyRecursive(arg,ret_type))
							| _ -> function_rec_error () )
				    | _ -> function_rec_error ()) in
      let fold_func_right = (fun b e env -> match e with | Fun (v,e) ->
				    let arg_type     = (match (snd v) with | Some x -> x | None -> function_rec_error ()) in
				    let env_with_arg = Env.bind env (fst v) arg_type in
				    let ret_type     = type_expression env_with_arg dataenv e in
				    let previous     = lookup_var (fst b) env in (
				    match previous with | TyRecursive (arg,ret) -> 
				    let ret_type = check_type "LetRec" ret ret_type dataenv in 
				    Env.bind env (fst b) (TyRecursive(arg,ret_type))
							| _ -> function_rec_error () )
				    | _ -> function_rec_error ()) in
      let newenv = List.fold_left2  fold_func_left  newenv xs es in
      let newenv = List.fold_right2 fold_func_right xs es newenv in
      type_expression newenv dataenv e
  | Fix (b, e) ->
      let ret_type  = ( match b with 
			| (_,None)       -> TyObject
			| (_,Some thing) -> thing ) in
      let arg_type  = ( match e with
			| Fun (v,e) -> (
			    match v with 
			    | (_,Some thing) -> thing
			    |  _             -> function_rec_error () )
			| _ -> function_rec_error ()
		       ) in
      let v = ( match e with | Fun (v,e) -> v | _ -> function_rec_error () ) in 
      let e = ( match e with | Fun (v,e) -> e | _ -> function_rec_error () ) in 
      let newenv = (Env.bind env (fst b) (TyRecursive(arg_type,ret_type))) in
      let env_with_arg = Env.bind newenv (fst v) arg_type in
      let return       = type_expression env_with_arg dataenv e in
      let _ = check_type "Fix" return ret_type dataenv in 
      TyRecursive(arg_type,return)
  | Prim p -> 
      (* Impossible, tout est catché *)
      Student.this_is_your_job ()
  | Tuple (es) ->
      TyTuple (List.map (fun x -> (type_expression env dataenv x)) es)
  | Data (cons, elist)->
      let typed_list = List.map (fun x -> (type_expression env dataenv x)) elist in
      let data_pair  = retrieve_datainfos cons dataenv  in
      let data_type  = TyADT (fst data_pair) in
      let _          = List.map2 (fun x y -> check_type "Data" x y dataenv) typed_list (snd data_pair)
      in data_type
  | Match (exp,clist) ->
      let exp_type  = (type_expression env dataenv exp) in
      let rec loop clist ty = match clist with 
	| [] -> ty
	| (patt,expr)::next -> let env = check_match patt exp_type env dataenv in
			       loop next (check_type "Match" ty (type_expression env dataenv expr) dataenv)
      in loop clist TyObject
  | ArrayWrite (arr,pos,value) ->
      let arrty = type_expression env dataenv arr in
      let _     = check_type "ArrayWrite" (type_expression env dataenv pos) TyInt dataenv in
      begin match arrty with 
	| TyArray ty -> let _ = check_type "ArrayWrite" ty (type_expression env dataenv value) dataenv in arrty
	| t ->  array_alloc_error t end
  | ArrayRead (arr,pos)  ->
      let arrty = type_expression env dataenv arr in
      let _     = check_type "ArrayRead" (type_expression env dataenv pos) TyInt dataenv in
      begin match arrty with 
	| TyArray ty -> ty
	| t -> array_alloc_error t end
  | Annot (e,ty) ->
      let type_got = type_expression env dataenv e in
      check_type "Annot" ty type_got dataenv
    
and check_type (source : string) (ty1 : ty) (ty2 : ty) dataenv = 
  (* print_string ("("^source^") "^(show_typed_value ty1) ^ " (COMPARE) " ^ (show_typed_value ty2)^ "\n"); *)
  if(ty1 = ty2) then ty2
  else match ty1 with 
    | TyObject -> verify_data ty2 dataenv; ty2
    | TyArrow (a,b,_,_,_) -> begin match ty2 with | TyArrow (a2,b2,_,_,_) -> let _ = check_type source a a2 dataenv in
									     let _ = check_type source b b2 dataenv in
									     ty2
						  | _ -> (mismatch_error source ty1 ty2) end
    | _ -> match ty2 with 
	   | TyObject -> verify_data ty1 dataenv; ty1
	   | _ -> (mismatch_error source ty1 ty2)
   
and check_match pattern p_type env dataenv = match pattern with
    | PVar v  -> Env.bind env v p_type
    | PData (datacons,patt_list) ->
		 let cons_list = get_conslist p_type dataenv in
		 let type_list = try (List.assoc datacons cons_list) with Not_found ->  (matching_error p_type datacons) in
		 List.fold_left2 (fun env p t -> check_match p t env dataenv) env patt_list type_list
    | PView _ -> typecheck_error "Views not supported."

(* Retrouve le type à partir du constructeur *)
and retrieve_datainfos cons denv = 
  let rec loop li = begin match li with
    | []               -> data_cons_error cons
    | (ty,clist)::next -> 
	let rec loop2 li = begin match li with
	    | [] 		 -> loop next
	    | (cs,tylist)::next2 -> if cs = cons then (ty,tylist)
				    else loop2 next2 end
	in loop2 clist end
  in loop denv

and get_conslist typ denv = 
  match typ with 
    | TyADT typ -> 
      let rec loop li = match li with
	| [] 	           -> data_type_error typ
	| (ty,clist)::next -> if ty = typ then clist else loop next
      in loop denv
    | _ -> typecheck_error "Error conslist."

and verify_data typ denv = 
  match typ with 
    | TyADT typ -> 
      let rec loop li = match li with
	| [] 	           -> data_type_error typ
	| (ty,clist)::next -> if ty = typ then () else loop next
      in loop denv
    | _ -> ()

and type_primitives prim arg env dataenv = match prim with 
  | LessThanFloat ->
    begin
    match arg with 
      | Tuple [e1; e2] -> let t = check_type "LessThanFloat" (type_expression env dataenv e1) (type_expression env dataenv e2) dataenv in
			  let _ = check_type "LessThanFloat" t TyFloat dataenv in
			  TyInt
      | _ -> typecheck_error "LessThan [float] problem."
    end
  | Add
  | Sub
  | Mul
  | Div 
  | LessThanInt -> 
    begin
    match arg with 
      | Tuple [e1; e2] -> let t = check_type "Binop(int)" (type_expression env dataenv e1) (type_expression env dataenv e2) dataenv in
			  check_type "Binop(int)" t TyInt dataenv
      | _ -> typecheck_error "Binop [int] problem."
    end
  | AddFloat
  | SubFloat
  | MulFloat
  | DivFloat ->
    begin
    match arg with 
      | Tuple [e1; e2] -> let t = check_type "Binop(float)" (type_expression env dataenv e1) (type_expression env dataenv e2) dataenv in
			  check_type "Binop(float)" t TyFloat dataenv
      | _ -> typecheck_error "Binop [float] problem."
    end
  | IfZ ->
    begin
    match arg with 
      | Tuple [cond; Fun ((_,_), lhs); Fun ((_,_), rhs)] -> 
			  let _ = check_type "IfZ" (type_expression env dataenv cond) TyInt dataenv in
			  let t = check_type "IfZ" (type_expression env dataenv lhs) (type_expression env dataenv rhs) dataenv in
			  t
      | _ -> typecheck_error "IFZ problem."
    end
  | Proj nth ->
    begin
      let arg = (type_expression env dataenv arg) in
      begin match arg with
	| TyTuple tylist ->
	  List.nth tylist nth
	| _ -> typecheck_error "Proj [tuples] problem."
      end
    end
  | AllocArray ->
    begin
      match arg with
	| App (length,value) ->
	  let _ = check_type "AllocArray" (type_expression env dataenv length) TyInt dataenv in
	  let v = (type_expression env dataenv value) in
	  TyArray v
        | _ -> typecheck_error "AllocArray problem."
    end

(*
let type_declaration env denv (x, e) = 
  let v = type_expression env denv e in
  v

let typecheck e dataenv =
  let env = Env.empty in
  let v = type_declaration env dataenv e in
  v
*)

let type_declaration env denv (x, e) = 
  let t = type_expression env denv e 
  in (Env.bind env x t)
(*print_string ("Declaration checked with type : " ^ (show_typed_value v) ^ "\n")*)


let typecheck =
  let env = ref Env.empty in
  fun dataenv e ->
    env := (type_declaration !env dataenv e)

let get_types dlist = 
  let rec loop dl = match dl with 
    | []
    | (DVar _ )::_ -> 
      []
    | (DType (tyvar, clist)) :: next-> 
      (tyvar,clist) :: (loop next)
  in loop dlist 