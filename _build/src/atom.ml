module Make (S : Hashtbl.HashedType) =
struct

  type t = int * S.t

  module H = Hashtbl.Make (S)

  let table = H.create 13

  let mk =
    let r = ref 0 in
      fun s ->
        try
          H.find table s
        with Not_found ->
          incr r;
          let a = (!r, s) in
            H.add table s a;
            a

  let extract = snd

  let equal = ( = )

  let hash = fst 

  let compare (i, _) (j, _) = compare i j

end
