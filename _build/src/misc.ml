(* From J.-C. Filliatre. *)
(* FIXME: document ! *)
module Types = struct
  module MakeExistential (X : sig type 'a t end) : sig
    type t
    val pack : 'a X.t -> t
    type 'a user = { f : 'b. 'b X.t -> 'a }
    val use : 'a user -> t -> 'a
  end = struct
    type 'a user = { f : 'b. 'b X.t -> 'a }
    type t = { pack : 'a. 'a user -> 'a }
    let pack impl = { pack = fun user -> user.f impl }
    let use f p = p.pack f
  end

  module MakeExistentialPoly (X : sig type ('a, 'b) t end) : sig
    type 'a t
    val pack : ('a, 'b) X.t -> 'a t
    type ('a, 'c) user = { f : 'b. ('a, 'b) X.t -> 'c }
    val use : ('a, 'c) user -> 'a t -> 'c
  end = struct
    type ('a, 'c) user = { f : 'b. ('a, 'b) X.t -> 'c }
    type 'a t = { pack : 'c. ('a, 'c) user -> 'c }
    let pack impl = { pack = fun user -> user.f impl }
    let use f p = p.pack f
  end
end

module HashedString = struct
  type t = string
  let hash = Hashtbl.hash
  let equal s1 s2 = (String.compare s1 s2 = 0)
end

module ListUtils = struct

  let first =
    let rec loop = function
      | [] -> raise Not_found
      | finder :: others ->
          try
            finder ()
          with _ -> loop others
    in
      loop

  let rec first_success xs f = 
    match xs with
      | [] -> None
      | x :: xs -> match f x with
          | None -> first_success xs f
          | Some x -> Some x

  let rec last = function
    | [] -> raise Not_found
    | [ x ] -> x
    | _ :: xs -> last xs

  let foldmap f =
    let rec loop ys accu = function
      | [] -> (accu, List.rev ys)
      | x :: xs -> let accu, y = f accu x in loop (y :: ys) accu xs
    in
      loop []

end

module FormatUtils = struct

  let as_string printer x =
    Format.fprintf Format.str_formatter "%a" printer x;
    Format.flush_str_formatter ()

  let rec separated_list fmt sep print =
    let rec loop fmt = function
      | [] -> ()
      | [ x ] -> print fmt x
      | x :: xs -> Format.fprintf fmt "%a%s@,%a" print x sep loop xs
    in
      Format.fprintf fmt "@[%a@]" loop

   let rec vertical_list fmt print =
    let rec loop fmt = function
      | [] -> ()
      | [ x ] -> print fmt x
      | x :: xs -> Format.fprintf fmt "%a@,%a" print x loop xs
    in
      Format.fprintf fmt "@[<v>%a@]" loop

  let rec terminated_list fmt term print =
    let rec loop fmt = function
      | [] -> ()
      | x :: xs -> Format.fprintf fmt "%a%s@,%a" print x term loop xs
    in
      Format.fprintf fmt "@[%a@]" loop

end

module IntUtils = struct

  let interval start stop =
    let rec loop accu i =
      if i < start then accu else loop (i :: accu) (i - 1)
    in
      loop [] stop

end

module IOUtils = struct

  let contents filename =
    let f = open_in filename in
    let b = Buffer.create 13 in
    let rec loop () =
      try
        Buffer.add_channel b f 1;
        loop ()
      with End_of_file -> close_in f; Buffer.contents b
    in
      loop ()
      

  let write_contents filename s = 
    let f = open_out filename in 
    let b = Buffer.create 13 in
      Buffer.add_string b s;
      Buffer.output_buffer f b;
      close_out f

end

module LexingUtils = struct

  open Lexing

  let from_filename s =
    let cin = open_in s in
    let lexbuf = Lexing.from_channel cin in
      lexbuf.lex_curr_p <-
        {
          pos_fname = s;
          pos_lnum  = 1;
          pos_bol   = 0;
          pos_cnum  = 0
        };
      lexbuf

end

module type FQueueSig = sig

    type 'a t

    exception EmptyQueue

    val empty        : 'a t
    val push     : 'a t -> 'a -> 'a t
    val pop      : 'a t -> 'a * 'a t
    val inject   : 'a t -> 'a -> 'a t
    val eject    : 'a t -> 'a t * 'a
    val is_empty : 'a t -> bool
    val rsplit   : ('a -> bool) -> 'a t -> ('a t * 'a * 'a t)
    val fold     : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
    val elements : 'a t -> 'a list
  end


let uncurry f = fun (x, y) -> f x y

let apply_option f = function
  | None -> None
  | Some x -> Some (f x)

let switch (x, y) = (y, x)

module FQueue = struct

  type 'a t = int * ('a list * 'a list)

  let empty = (0, ([], []))

  let debug (_, x) = x

  exception EmptyQueue

  let elements (_, (p, s)) = p @ (List.rev s)

  let lsplit k l =
    let rec loop i accu l =
      if i = 0 then (List.rev accu, List.rev l)
      else match l with
        | [] -> assert false
        | x :: xs -> loop (i - 1) (x :: accu) xs
    in
      loop k [] l

  let push (k, (p, s)) x = (k + 1, (x :: p, s))

  let rec pop q =
    match q with
      | (k, (x :: xs, s)) -> x, (k - 1, (xs, s))
      | (k, ([], ((_ :: _) as xs))) -> pop (k, switch (lsplit (k / 2) xs))
      | (k, ([], [])) -> assert (k = 0); raise EmptyQueue

  let inject (k, (p, s)) x = (k + 1, (p, x :: s))

  let rec eject q =
    match q with
      | (k, (p, x :: xs)) -> (k - 1, (p, xs)), x
      | (k, (((_ :: _) as xs), [])) -> eject (k, lsplit (k / 2) xs)
      | (k, ([], [])) -> assert (k = 0); raise EmptyQueue

  let is_empty (k, _) = k = 0

  let fold f accu (k, (p, q)) =
    List.fold_left f (List.fold_left f accu p) (List.rev q)

  let rsplit f ((n, _) as q) : 'a t * 'a * 'a t =
    let rec loop_prefix k accu = function
      | [] -> raise Not_found
      | x :: xs ->
          if f x then
            ((n - k, lsplit ((n - k) / 2) xs), x,
             (k - 1, lsplit ((k - 1) / 2) accu))
          else
            loop_prefix (k + 1) (x :: accu) xs
    in
    let rec loop k accu = function
      | (_, ([], [])) -> raise Not_found
      | (i, (p, x :: xs)) ->
          if f x then
            ((n - k, (p, xs)), x,
             (k - 1, lsplit ((k - 1) / 2) accu))
          else
            loop (k + 1) (x :: accu) (i - 1, (p, xs))
      | (i, (p, [])) ->
          loop_prefix k accu (List.rev p)
    in
      loop 0 [] q

end

(* FIXME:
open FQueue;;
let e = empty;;
let f = List.fold_left inject e [1;2;3;4];;
let f = List.fold_left push f [5;6;7];;
let (p, _, q) = rsplit (fun x -> x = 7) f;;
elements p, elements q;;
elements f;;
*)
