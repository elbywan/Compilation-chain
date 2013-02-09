type t = T of int 
let x1 = (fun x ->
 (fun y -> x (T (2))) ((fun z ->
   ( match z with 
   | T (x) -> x 
      | a -> 3 
      )
))) ((fun z ->
  ( match z with 
  | T (x) -> x 
     | a -> 2 
     )
))