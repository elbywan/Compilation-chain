type list = Nil | Cons of int * list 
let x9 = let rec
 length = (fun (l : list) ->
  let rec
   aux = (fun (l : list) ->
     ( match l with 
     | Nil  -> 0 
        | Cons (x, xs) -> 1 + aux (xs) 
        )
)
   in aux (l))
 in 
let rec
 concat = (fun (l : list) ->
  (fun l2 ->
   let rec
    aux = (fun (l : list) ->
      ( match l with 
      | Nil  -> l2 
         | Cons (x, xs) -> Cons (x, aux (xs)) 
         )
)
    in aux (l)))
 in 
let rec
 create_from = (fun (first : int) ->
  (fun last ->
   let rec
    loop = (fun (i : int) ->
     if last + 1 - i = 0 then begin  (fun x2 -> Nil ) ()  end else begin 
      (fun x1 -> Cons (i, loop (i + 1))) () end)
    in loop (first)))
 in 
let rec
 create_to = (fun (last : int) ->
  (fun first ->
   let rec
    loop = (fun (i : int) ->
     if first - 1 - i = 0 then begin  (fun x4 -> Nil ) ()  end else begin 
      (fun x3 -> Cons (i, loop (i - 1))) () end)
    in loop (last)))
 in 
let rec
 map = (fun (l : list) ->
  (fun funct ->
   let rec
    loop = (fun (l : list) ->
      ( match l with 
      | Nil  -> Nil  
         | Cons (x, xs) -> Cons (funct (x), loop (xs)) 
         )
)
    in loop (l)))
 in 
let rec
 fold_left = (fun (l : list) ->
  (fun funct ->
   (fun accu ->
    let rec
     loop = (fun (l : list) ->
      (fun acc ->
        ( match l with 
        | Nil  -> acc 
           | Cons (x, xs) -> loop (xs) (funct (x) (acc)) 
           )
))
     in loop (l) (accu))))
 in 
let rec
 quicksort = (fun (l : list) ->
  if (fun x -> if x = true then 1 else 0) (1 < length (l)) = 0 then begin 
   (fun x8 -> l) ()  end else begin 
   (fun x7 ->
    (fun pivot ->
     (fun l ->
      let rec
       loop = (fun (l : list) ->
        (fun less ->
         (fun more ->
           ( match l with 
           | Nil  -> (less, more) 
              | 
             Cons (x, xs) -> if (fun x -> if x = true then 1 else 0) (pivot < x) = 0 then begin 
              (fun x6 -> loop (xs) (Cons (x, less)) (more)) () 
              end else begin 
              (fun x5 -> loop (xs) (less) (Cons (x, more))) () end 
              )
)))
       in 
      (fun tuple ->
       concat (concat (quicksort (failwith ("Projections not supported in Ocaml.") (tuple))) (Cons (
       pivot, Nil ))) (quicksort (failwith ("Projections not supported in Ocaml.") (tuple)))) (loop (l) (Nil ) (Nil ))) ( ( match l with 
      | Nil  -> Nil  
         | Cons (x, xs) -> xs 
         )
)) ( ( match l with 
     | Nil  -> 0 - 1 
        | Cons (x, xs) -> x 
        )
)) () end)
 in quicksort (create_to (10) (0))