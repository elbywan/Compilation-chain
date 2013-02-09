(**

    FIXME: Document.

    From {{:http://en.wikipedia.org/wiki/Type_system#Existential_types}
    Wikipedia}:

    Existential types are frequently used to represent modules and
    abstract data types because of their ability to separate
    implementation from interface. For example, in C pseudocode, the
    type [T = exists X { X a; int f(X); }] describes a module interface
    that has a data member of type X and a function that takes a
    parameter of the same type X and returns an integer. This could be
    implemented in different ways; for example:

    * [intT { int a; int f(int); }]
    * [floatT { float a; int f(float); }]

    These types are both subtypes of the more general existential type
    T and correspond to concrete implementation types, so any value of
    one of these types is a value of type T. Given a value "t" of type
    "T", we know that "t.f(t.a)" is well-typed, regardless of what the
    abstract type X is. This gives flexibility for choosing types
    suited to a particular implementation while clients that use only
    values of the interface type - the existential type - are isolated
    from these choices.

*)

module Types : sig
  module MakeExistential (X : sig type 'a t end) : sig
    type t
    val pack : 'a X.t -> t
    type 'a user = { f : 'b. 'b X.t -> 'a }
    val use : 'a user -> t -> 'a
  end

  module MakeExistentialPoly (X : sig type ('a, 'b) t end) : sig
    type 'a t
    val pack : ('a, 'b) X.t -> 'a t
    type ('a, 'c) user = { f : 'b. ('a, 'b) X.t -> 'c }
    val use : ('a, 'c) user -> 'a t -> 'c
  end
end

module HashedString : Hashtbl.HashedType with type t = string

module ListUtils : sig

  val first : (unit -> 'a) list -> 'a

  val last : 'a list -> 'a

  val foldmap : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list

  val first_success : 'a list -> ('a -> 'b option) -> 'b option

end

module FormatUtils : sig

  open Format

  val as_string : (formatter -> 'a -> unit) -> 'a -> string

  val separated_list : 
    formatter -> string -> (formatter -> 'a -> unit) -> 'a list -> unit

  val vertical_list : 
    formatter -> (formatter -> 'a -> unit) -> 'a list -> unit

  val terminated_list : 
    formatter -> string -> (formatter -> 'a -> unit) -> 'a list -> unit

end

module IntUtils : sig

  val interval : int -> int -> int list

end

module IOUtils : sig

  val contents : string -> string

  val write_contents : string -> string -> unit

end

module LexingUtils : sig

  val from_filename : string -> Lexing.lexbuf

end

val uncurry : ('a -> 'b -> 'c) -> (('a * 'b) -> 'c)

val apply_option : ('a -> 'b) -> 'a option -> 'b option

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
  
module FQueue : FQueueSig
