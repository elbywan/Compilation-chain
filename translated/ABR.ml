type abr = Vide | Feuille of float | Noeud of float * abr * abr 
let x13 = let rec
 ajout = (fun (a : abr) ->
  (fun nb ->
    ( match a with 
    | Vide  -> Feuille (nb) 
       | 
      Feuille (i) -> if (fun x -> if x = true then 1 else 0) (i < nb) = 0 then begin 
       (fun x2 -> Noeud (nb, Vide , Feuille (i))) ()  end else begin 
       (fun x1 -> Noeud (nb, Feuille (i), Vide )) () end 
       | 
      Noeud (i, g, d) -> if (fun x -> if x = true then 1 else 0) (i < nb) = 0 then begin 
       (fun x4 -> Noeud (i, ajout (g) (nb), d)) ()  end else begin 
       (fun x3 -> Noeud (i, g, ajout (d) (nb))) () end 
       )
))
 in 
let rec
 isIn = (fun (a : abr) ->
  (fun elem ->
    ( match a with 
    | Vide  -> 0 
       | 
      Feuille (i) -> if (fun x -> if x = true then 1 else 0) (0. < i -. elem) = 0 then begin 
       (fun x6 -> 1) ()  end else begin  (fun x5 -> 0) () end 
       | 
      Noeud (i, g, d) -> if (fun x -> if x = true then 1 else 0) (i < elem) = 0 then begin 
       (fun x10 -> isIn (g) (elem)) ()  end else begin 
       (fun x9 ->
        if (fun x -> if x = true then 1 else 0) (0. < i -. elem) = 0 then begin 
         (fun x8 -> 1) ()  end else begin  (fun x7 -> isIn (d) (elem)) () end) () end 
       )
))
 in 
let rec
 cree_arbre_equilibre = (fun (max : float) ->
  (fun moy ->
   (fun newabr ->
    let rec
     loop = (fun (moy : float) ->
      (fun a ->
       (fun count ->
        if (fun x -> if x = true then 1 else 0) (1. < count) = 0 then begin 
         (fun x12 -> a) ()  end else begin 
         (fun x11 ->
           ( match a with 
           | Feuille (i) -> (fun left ->
              (fun right ->
               Noeud (i, left, right)) (loop (moy /. 2.) (Feuille (i +. moy /. 2.)) (count /. 2.))) (loop (moy /. 2.) (Feuille (
             i -. moy /. 2.)) (count /. 2.)) 
              )
) () end)))
     in loop (moy) (newabr) (max)) (ajout (Vide ) (moy))) (max /. 2.))
 in cree_arbre_equilibre (10.)