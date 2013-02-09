type list = Nil | Cons of int * list 
let x3 = let rec
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
(fun list1 ->
 (fun list2 ->
  (fun list3 ->
   (fun sum ->
    sum + length (list1)) (fold_left (create_from (0) (5)) ((fun x ->
    (fun y -> x + y))) (0))) (map (concat (list1) (list2)) ((fun x ->
   x + 1)))) (create_from (15) (20))) (create_from (5) (10))