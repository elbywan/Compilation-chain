open WillowAST
module S = Identifier.ISet
module H = Identifier.IHashTable

let compilation_error = Error.global_error "during compilation"

module Env = GenericEnvironment.Make
  (struct
     let raise_error x =
       compilation_error (Printf.sprintf "%s is unbound."
                            (Identifier.as_string x))
   end)

let env_var = WillowIdentifier.fresh ()
(** Ajouts **)
let new_var () = WillowIdentifier.fresh ()
let new_fun () = WillowIdentifier.fresh_function ()

type index = int list

let show_env env =
  Env.fold (fun accu x v ->
                 Printf.sprintf "%s@%s "
                   (Identifier.as_string x)
                   (String.concat ":" (List.map string_of_int v)) ^ accu) env ""

let rec read where = function
  | [ i ] -> BlockRead (where, i)
  | i :: is -> BlockRead (read where is, i)
  | [] -> assert false

let rec write where what = function
  | [ i ] -> BlockWrite (where, i, what)
  | i :: is -> BlockWrite (read where is, i, what)
  | [] -> assert false
  
(** AJOUTS **)

(** ENVIRONNEMENT DATA **)

let data_bank : (int H.t) = (H.create 31)
let data_count = ref 0
let add_data const =
  try let _ = H.find data_bank const in ()
  with Not_found -> H.add data_bank const !data_count; data_count := !data_count + 1
let rec get_data const = 
  try H.find data_bank const
   with Not_found -> add_data const; get_data const

(* BLOCKS *)
let rec create_block ds fdefs env =
  let rec loop li fd lst = match li with
	| []       -> (fd,List.rev lst)
	| (v,e)::b -> let _pair = trad env fd e in
		      loop b (fst _pair) (snd _pair::lst)
  in loop ds fdefs []

and create_block_fix ds fdefs env xvar abstrvar = 
  let rec loop li fd lst = match li with
	| []       -> (fd,List.rev lst)
	| (v,e)::b -> if (v = xvar) 
		      then begin let _pair = (fdefs,WillowAST.Int 0) in
				 loop b (fst _pair) (snd _pair::lst) end
		      else begin let _pair = trad env fd e in
				 loop b (fst _pair) (snd _pair::lst) end
  in loop ds fdefs []

and write_block_fix ds xvar blockvar =
  let rec loop li nb = match li with 
      | [] -> WillowAST.Var xvar
      | (v,e)::b -> if (v = xvar) then begin 
		    let abstr_var = new_var () in
		    WillowAST.Let (abstr_var,(WillowAST.BlockWrite (WillowAST.Var blockvar,WillowAST.Int nb,WillowAST.Var xvar)),loop b (nb+1)) end
		    else loop b (nb+1)
  in loop ds 0

and create_block_rec xs fdefs env xvar expr blockvar =
  match expr with 
     | ClovisAST.Closure (ds, x, e) -> (
       let rec loop li fd lst = match li with
	| [] 	   -> (fd,List.rev lst)
	| (v,e)::b -> if (List.exists (fun x -> if x = v then true else false) xs)
		      then begin let _pair = (fdefs,WillowAST.Int 0) in
				 loop b (fst _pair) (snd _pair::lst) end
		      else begin let _pair = trad env fd e in
				 loop b (fst _pair) (snd _pair::lst) end
        in loop ds fdefs []
     )
     | _ -> compilation_error "LETREC not followed by a closure.\n" 

and get_env_rec es fdefs fvarl = (
  List.fold_left2 (fun accu e fresh_funid -> match e with
	| ClovisAST.Closure (ds, _argid, e) -> (
	  let rec loop li env nb = match li with
	    | []       -> env
	    | (v,e)::b -> loop b (Env.bind env v [nb]) (nb+1)
	  in let _env = loop ds Env.empty 0 in
	  let _body = (trad _env accu e) in
	  match _body with | (funlist,bodyexpr) -> 
	  let _fdecl = (fresh_funid,env_var,_argid,bodyexpr) in
	  _fdecl::funlist
	  )
        | _ -> compilation_error "LETREC not followed by a closure.\n") fdefs es fvarl)

and write_1block_rec ds xs blockvar = 
  let rec loop li nb = match li with 
      | [] -> []
      | (v,e)::b -> if (List.exists (fun x -> if x = v then true else false) xs) then begin 
		    (WillowAST.BlockWrite (WillowAST.Var blockvar,WillowAST.Int nb,WillowAST.Var v))::(loop b (nb+1)) end
		    else loop b (nb+1)
  in loop ds 0

and write_allblocks_rec xs es bvarlist = List.rev (List.fold_left2 
(fun accu e b -> match e with 
		 | ClovisAST.Closure (ds, _, _) -> 
		      (write_1block_rec ds xs b)@accu
		 | _ -> compilation_error "LETREC not followed by a closure.\n"
) [] es bvarlist)

(** PATTERN MATCHING **)
and make_plus i next =
   Call (Prim Add, WillowAST.BlockAlloc [], WillowAST.BlockAlloc (i::(next)::[]))

and make_minus i1 i2 = 
   Call (Prim Sub, WillowAST.BlockAlloc [], WillowAST.BlockAlloc ((WillowAST.Int i1)::(i2)::[]))

and make_let var exp next = 
  WillowAST.Let (var,exp,next)

and make_if cond _then _else =
  Call (Prim IfZ, WillowAST.BlockAlloc [], WillowAST.BlockAlloc (cond::_then::_else::[]))

and has_match = ref 0
and match_funvar () = new_fun ()
and match_argvar () = new_var ()
and match_funbody =  (WillowAST.Var env_var)
and match_ref     = ref (new_fun ())
and declare_matching fdefs =
   if !has_match = 0 then begin
      match_ref := (match_funvar ());
      (!match_ref,env_var,match_argvar (), match_funbody)::fdefs
   end else fdefs
and make_matchfun nb follow =
  let rec loop nb = if nb = 0 then follow
		    else (WillowAST.BlockAlloc ((WillowAST.FVar !match_ref)::(loop (nb-1))::[]))
  in loop nb

and make_closure env fdefs cls inject = ( match cls with
      | ClovisAST.Closure (ds, x, e) -> (
      let fresh_funid = new_fun ()                  in
      let fresh_envid = env_var                     in
      let      _argid = x                           in
      let rec loop li env nb = match li with
	| []       -> env
	| (v,e)::b -> loop b (Env.bind env v [nb]) (nb+1)
				                    in
      let      _env   = loop ds Env.empty 0         in
      let _pair_block = create_block ds fdefs env   in
      let 	fdefs = fst _pair_block             in
      let      _block = snd _pair_block             in
      let      _body  = (trad _env fdefs e)         in
      match _body with | (funlist,bodyexpr) -> 
      let      _fdecl = (fresh_funid,fresh_envid,_argid,(inject bodyexpr)) in
      let willow_blck = WillowAST.BlockAlloc ((WillowAST.FVar fresh_funid)::(WillowAST.BlockAlloc _block)::[]) in
      (_fdecl::funlist, willow_blck) )
      | _ -> compilation_error "Invalid compilation model.\n" )

and get_ifmatch cval pattern pos = match pattern with
  | ClovisAST.PVar var -> (WillowAST.Int 0)
  | ClovisAST.PData (const,plist) -> let rec get_ifmatch cval pattern pos = match pattern with
  | ClovisAST.PVar var -> (WillowAST.Int 0)
  | ClovisAST.PData (const,plist) -> let id_const   = get_data const in
				     let difference = make_minus id_const (WillowAST.BlockRead ((WillowAST.BlockRead (cval,WillowAST.Int pos),WillowAST.Int 0))) in 
				     let rec loop cval pos li = match li with
				      | [] -> (WillowAST.Int 0)
				      | p::next -> let res_patt = (get_ifmatch cval p pos) in
						   match res_patt with | WillowAST.Int 0 -> loop cval (pos+1) next
								       | _               -> make_plus (get_ifmatch cval p pos) (loop cval (pos+1) next)
				     in make_plus (difference) (loop (WillowAST.BlockRead (cval,WillowAST.Int pos)) 1 plist) 
  | ClovisAST.PView (_,_) -> compilation_error "Views not supported (yet...).\n" 
  in let id_const   = get_data const in
		      let difference = make_minus id_const (WillowAST.BlockRead (cval,WillowAST.Int pos)) in 
		      let rec loop cval pos li = match li with
		      | [] -> (WillowAST.Int 0)
		      | p::next -> let res_patt = (get_ifmatch cval p pos) in
				    match res_patt with | WillowAST.Int 0 -> loop cval (pos+1) next
							| _               -> make_plus (get_ifmatch cval p pos) (loop cval (pos+1) next)
		      in make_plus (difference) (loop cval 1 plist)
  | ClovisAST.PView (_,_) -> compilation_error "Views not supported (yet...).\n"

and get_letmatch cval pattern pos end_exp = match pattern with
  | ClovisAST.PVar var -> make_let var cval end_exp
  | ClovisAST.PData (_,plist) -> let rec get_letmatch cval pattern pos end_exp = match pattern with
  | ClovisAST.PVar var -> make_let var (WillowAST.BlockRead (cval,WillowAST.Int pos)) end_exp
  | ClovisAST.PData (_,plist) -> let rec loop cval pos li = match li with
				  | []      -> end_exp
				  | p::next -> get_letmatch cval p pos (loop cval (pos+1) next)
				 in loop (WillowAST.BlockRead (cval,WillowAST.Int pos)) 1 plist
  | ClovisAST.PView (_,_) -> compilation_error "Views not supported (yet...).\n"
  in let rec loop cval pos li = match li with
      | []      -> end_exp
      | p::next -> get_letmatch cval p pos (loop cval (pos+1) next)
      in loop cval 1 plist
  | ClovisAST.PView (_,_) -> compilation_error "Views not supported (yet...).\n"

(** FIN AJOUTS **)

and trad env fdefs : ClovisAST.expression -> WillowAST.program =
  fun e ->
    if !Settings.debug then 
      Printf.eprintf "[Willow-to-Clovis]: %s |- %s\n"
        (show_env env)
        (Misc.FormatUtils.as_string ClovisPrinter.expression e);
    match e with
  | ClovisAST.Var x ->
      (fdefs,WillowAST.Var x)
  | ClovisAST.Lookup x ->
      let pos = List.hd (Env.lookup env x)     in
      (fdefs,BlockRead(WillowAST.Var env_var, WillowAST.Int pos))
  | ClovisAST.Int i ->
      (fdefs,WillowAST.Int i)
  | ClovisAST.Let ((x, lhs), rhs) ->
      let eval1 = trad env fdefs lhs           in
      let fdefs = fst eval1 		       in
      let eval2 = trad env fdefs rhs           in
      let fdefs = fst eval2 		       in
      (fdefs,WillowAST.Let (x,(snd eval1), (snd eval2)))
  | ClovisAST.Closure (ds, x, e) -> (
      let fresh_funid = new_fun ()                  in
      let fresh_envid = env_var                     in
      let      _argid = x                           in
      let rec loop li env nb = match li with
	| []       -> env
	| (v,e)::b -> loop b (Env.bind env v [nb]) (nb+1)
				                    in
      let      _env   = loop ds Env.empty 0         in
      let _pair_block = create_block ds fdefs env   in
      let 	fdefs = fst _pair_block             in
      let      _block = snd _pair_block             in
      let      _body  = (trad _env fdefs e)         in
      match _body with | (funlist,bodyexpr) -> 
      let      _fdecl = (fresh_funid,fresh_envid,_argid,bodyexpr) in
      let willow_blck = WillowAST.BlockAlloc ((WillowAST.FVar fresh_funid)::(WillowAST.BlockAlloc _block)::[]) in
      (_fdecl::funlist, willow_blck) )
      (** RETURNS :
	([fvar * var * var * expression] list / [FVar of fvar,[FV BLOCK]]) 
      **)
  | ClovisAST.Fix (x, e) -> 
      (
      match e with 
	| ClovisAST.Closure (ds, _argid, e) -> (
	  let fresh_funid = new_fun ()                  in
	  let abstract_id = new_var ()			in
	  let rec loop li env nb = match li with
	    | []       -> env
	    | (v,e)::b -> loop b (Env.bind env v [nb]) (nb+1)
							in
	  let      _env   = loop ds Env.empty 0         in
	  let _pair_block = create_block_fix ds fdefs env x abstract_id in
	  let 	    fdefs = fst _pair_block             in
	  let      _block = snd _pair_block             in
	  let      _body  = (trad _env fdefs e)         in
	  match _body with | (funlist,bodyexpr) -> 
	  let      _fdecl = (fresh_funid,env_var,_argid,bodyexpr) in
	  let willow_blck = WillowAST.BlockAlloc ((WillowAST.FVar fresh_funid)::(WillowAST.Var abstract_id)::[]) in
	  let block_init  = WillowAST.BlockAlloc (_block) in
	  let write_instr = write_block_fix ds x abstract_id in
	  (_fdecl::funlist,
	  WillowAST.Let (abstract_id,block_init,
	  WillowAST.Let (x,willow_blck,
	  write_instr
	  )))
	  )
	| _ -> compilation_error "Fix not followed by a closure.\n" 
	)
  | ClovisAST.LetRec (xs, es, e) ->
      (** Schéma :
	  - Initialisation des blocs avec les var. récursives à 0. (les autres ont la bonne valeur)
	    1 Bloc / Var. Rec.
	  - Initialisation des seconds blocs, à 2 pos : 1ère pos pour Fvar / 2è pos qui pointe vers le 1er bloc respectif
	  - Série de WRITE qui remplace les 0 des premiers blocs par des pointeurs vers les seconds blocs.
	  - + Création d'une fonction par var. rec. (on récup. l'env, on traduit le corps, on crée un nouveau symbole pour la fonction et on ajoute au fenv.)
      **)
      let blockvar_list = ref [] in
      let funvar_list   = ref [] in
      (** 1) Premiers blocs + blockvars **)
      let blockdecl_list = (
	  List.fold_left2 (fun accu x e -> let bvar = new_var () in
					   blockvar_list := bvar::!blockvar_list;
					   let block = create_block_rec xs (fst accu) env x e bvar in
					   let fdefs = fst block in
					   let block = snd block in
					   (fdefs,(WillowAST.BlockAlloc block)::(snd accu))) (fdefs,[]) xs es) in
      let fdefs = fst blockdecl_list in
      let blockdecl_list = List.rev (snd blockdecl_list) in
      blockvar_list := (List.rev !blockvar_list);
      (** 2) Seconds blocs + funvars **)
      let blocksnd_list = List.rev (
	List.fold_left (fun accu b -> let fvar = new_fun () in
				      funvar_list := fvar::!funvar_list;
				      let block = WillowAST.BlockAlloc ((WillowAST.FVar fvar)::(WillowAST.Var b)::[]) in
				      block::accu) [] !blockvar_list
      ) in
      funvar_list := List.rev !funvar_list;
      (** 3) Création de fonctions + environnement **)
      let fdefs = get_env_rec es fdefs !funvar_list in
      let ref_fdefs = ref [] in
      (** 4) Création des writes **)
      let write_list = write_allblocks_rec xs es !blockvar_list in
      (** On applique avec des LET imbriqués **)
      let rec let_init li = match li with
	| [] -> let_init_snd (List.combine xs blocksnd_list)
	| (b,blc)::next ->  WillowAST.Let (b,blc,let_init next)
      and let_init_snd li = match li with
	| [] -> let_writes write_list
	| (x,blc)::next -> WillowAST.Let (x,blc,let_init_snd next)
      and let_writes li = match li with
	| [] -> let eval = (trad env fdefs e) in
		ref_fdefs := fst eval;
		snd eval
	| w::next -> WillowAST.Let (new_var (), w, let_writes next)
      in let final_res = let_init (List.combine !blockvar_list blockdecl_list)
      in (!ref_fdefs,final_res)
  | ClovisAST.Tuple es ->
       let rec loop li fd lst = match li with
	| []       -> (fd,List.rev lst)
	| e::b -> let _pair = trad env fd e in
		      loop b (fst _pair) (snd _pair::lst)
       in let result = loop es fdefs []
       in (fst result,WillowAST.BlockAlloc (snd result))
  | ClovisAST.Apply (ClovisAST.Apply (ClovisAST.Prim ClovisAST.AllocArray, size), expr) -> 
      let init_map  = trad env fdefs expr in
      let fdefs     = fst init_map        in
      let init_size = trad env fdefs size in
      let fdefs     = fst init_size       in (* OLD VERSION
      match snd init_size with | WillowAST.Int size ->
      let rec loop nb_of_loops = if nb_of_loops = 0 then [] else (snd init_map)::(loop (nb_of_loops - 1))
      in let init_map = loop size in
      (fdefs,WillowAST.BlockAlloc (init_map))
			       | _ -> compilation_error "Array size must be an integer.\n" *)
      (fdefs,WillowAST.Call (WillowAST.Prim WillowAST.AllocArray, BlockAlloc [], BlockAlloc [snd init_size; snd init_map]))
  | ClovisAST.Apply (ClovisAST.Prim (ClovisAST.Proj i), t) ->
       let tuple = trad env fdefs t in
       let fdefs = fst tuple 	    in
       (fdefs,WillowAST.BlockRead(snd tuple,WillowAST.Int i))
  | ClovisAST.Apply (ClovisAST.Prim p, t) ->
      let tuple = trad env fdefs t in
      let fdefs = fst tuple 	   in
      (fdefs,WillowAST.Call (WillowAST.Prim (primitive p),WillowAST.Int 0,snd tuple))
  | ClovisAST.Apply (f, x) -> 
      let f_pair = trad env fdefs f 	 	  in 
      let block  = snd f_pair			  in
      let arg    = trad env (fst f_pair) x  	  in 
      (fst arg, Call(BlockRead(block,WillowAST.Int 0),BlockRead(block,WillowAST.Int 1),(snd arg)))
  | ClovisAST.Prim p -> 
      (fdefs,WillowAST.Prim (primitive p))
  | ClovisAST.Float f ->
      (fdefs,WillowAST.Float f)
  | ClovisAST.ArrayRead (arr,pos) ->
      let arr_ptr = trad env fdefs arr in
      let fdefs   = fst arr_ptr        in
      let pos_exp = trad env fdefs pos in
      let fdefs   = fst pos_exp        in
      (fdefs,WillowAST.BlockRead (snd arr_ptr, snd pos_exp))
  | ClovisAST.ArrayWrite (arr,pos,exp) ->
      let arr_ptr = trad env fdefs arr in
      let fdefs   = fst arr_ptr        in
      let pos_exp = trad env fdefs pos in
      let fdefs   = fst pos_exp        in
      let affect  = trad env fdefs exp in
      let fdefs   = fst affect         in
      (fdefs,WillowAST.BlockWrite (snd arr_ptr, snd pos_exp, snd affect))
  | ClovisAST.Data (const,es) ->
      add_data const;
      let rec loop li fd lst = match li with
	| []       -> (fd,List.rev lst)
	| e::b -> let _pair = trad env fd e in
      loop b (fst _pair) (snd _pair::lst)
      in let result = loop es fdefs []
      in (fst result,WillowAST.BlockAlloc (WillowAST.Int (get_data const)::snd result))
  | ClovisAST.Match (exp,clist) -> 
      let fdefs = declare_matching fdefs     in
      let to_match_pair = trad env fdefs exp in
      let fdefs = fst to_match_pair          in
      let to_match = snd to_match_pair       in 
      let rec_fdefs = ref fdefs 	     in
      let rec loop clist nb = match clist with 
      | []             -> (WillowAST.Int 0) (* CAS OU LE MATCH ECHOUE *)
      | (patt,e)::next -> let get_if_content = (get_ifmatch  to_match patt 0) in 
			  let get_if_lets    = (get_letmatch to_match patt 0) in
			  let trad_as_closure_pair = trad env !rec_fdefs e    in
			  rec_fdefs     := fst trad_as_closure_pair;
			  let as_closure = snd trad_as_closure_pair           in
			  let as_true_closure = make_matchfun nb as_closure   in
			  make_if get_if_content (get_if_lets as_true_closure) (loop next (nb+1))
      in (!rec_fdefs,loop clist 0) 
        
and primitive = function
  | ClovisAST.Add -> Add
  | ClovisAST.Sub -> Sub
  | ClovisAST.Mul -> Mul
  | ClovisAST.Div -> Div
  | ClovisAST.Proj i -> assert false
  | ClovisAST.IfZ -> IfZ
  | ClovisAST.AddFloat -> AddFloat
  | ClovisAST.SubFloat -> SubFloat
  | ClovisAST.MulFloat -> MulFloat
  | ClovisAST.DivFloat -> DivFloat
  | ClovisAST.LessThanInt -> LessThanInt
  | ClovisAST.LessThanFloat -> LessThanFloat
  | ClovisAST.AllocArray -> failwith "Impossible." (** IMPOSSIBLE, CAPTURé PAR UN MOTIF DANS LE PATTERN MATCHING PRINCIPAL **)

let translate exp = trad Env.empty [] exp
