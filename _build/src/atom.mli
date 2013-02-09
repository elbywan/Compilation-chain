module Make (S : Hashtbl.HashedType) :
sig
  type t
  val mk : S.t -> t
  val extract : t -> S.t
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end
