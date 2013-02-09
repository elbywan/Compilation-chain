module Make (S: sig 
               val raise_error : Identifier.t -> 'a 
             end) = struct

  include Identifier.IMap
    
  module Identifier = Identifier
    
  let bind e x v = add x v e
    
  let lookup e x = 
    try find x e with Not_found -> S.raise_error x
    
  let domain e = 
    fold (fun k _ s -> Identifier.ISet.add k s) e Identifier.ISet.empty

  (** Ajout exists **)
  let exists truc e = fold (fun k v accu -> if v = truc then accu+1 else accu) e 0

  let fold f accu m = fold (fun k v accu -> f accu k v) accu m
      
end
