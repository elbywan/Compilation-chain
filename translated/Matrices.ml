let x13 = let rec
 init_matrice = (fun (height : int) ->
  (fun (width : int) ->
   (fun value ->
    (fun mat ->
     let rec
      loop = (fun (i : int) ->
       if height - i = 0 then begin  (fun x2 -> mat) ()  end else begin 
        (fun x1 ->
         (fun z ->
          loop (i + 1)) ((mat.(i) <- Array.make (width) (value); mat))) () end)
      in loop (0)) (Array.make (height) (Array.make (width) (value))))))
 in 
(fun add_matrice ->
 (fun mult_matrice ->
  (fun matrice1 ->
   (fun matrice2 ->
    (fun z ->
     mult_matrice (matrice1) (matrice2) (3) (2) (3)) ((matrice1.(1) .(0) <- 5.; matrice1.(1) ))) (init_matrice (2) (3) (2.))) (init_matrice (3) (2) (1.))) ((fun (m1 : float array array) ->
  (fun m2 ->
   (fun h1 ->
    (fun common ->
     (fun w2 ->
      (fun new_matrice ->
       let rec
        calc_case = (fun (pos : int) ->
         (fun accu ->
          (fun i ->
           (fun j ->
            if common - pos = 0 then begin  (fun x8 -> accu) () 
             end else begin 
             (fun x7 ->
              calc_case (pos + 1) (accu +. m1.(i) .(pos)  *. m2.(pos) .(j) ) (i) (j)) () end))))
        in 
       let rec
        loop1 = (fun (i : int) ->
         (fun j ->
          if h1 - i = 0 then begin  (fun x10 -> new_matrice) () 
           end else begin 
           (fun x9 ->
            (fun z ->
             loop1 (i + 1) (j)) ((new_matrice.(i) .(j) <- calc_case (0) (0.) (i) (j); new_matrice.(i) ))) () end))
        in 
       let rec
        loop2 = (fun (i : int) ->
         if w2 - i - 1 = 0 then begin  (fun x12 -> new_matrice) () 
          end else begin 
          (fun x11 -> (fun z -> loop2 (i + 1)) (loop1 (0) (i + 1))) () end)
        in loop2 (0 - 1)) (init_matrice (h1) (w2) (0.))))))))) ((fun (m : float array array) ->
 (fun m2 ->
  (fun height ->
   (fun width ->
    (fun new_matrice ->
     let rec
      loop1 = (fun (i : int) ->
       (fun j ->
        if width - i = 0 then begin  (fun x4 -> new_matrice) () 
         end else begin 
         (fun x3 ->
          (fun z ->
           loop1 (i + 1) (j)) ((new_matrice.(i) .(j) <- m.(j) .(i)  +. m2.(j) .(i) ; new_matrice.(i) ))) () end))
      in 
     let rec
      loop2 = (fun (i : int) ->
       if height - i - 1 = 0 then begin  (fun x6 -> new_matrice) () 
        end else begin 
        (fun x5 -> (fun z -> loop2 (i + 1)) (loop1 (0) (i + 1))) () end)
      in loop2 (0 - 1)) (init_matrice (height) (width) (0.)))))))