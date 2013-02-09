module KindAtom = Atom.Make (Misc.HashedString)

type kind = KindAtom.t

let fresh_kind = KindAtom.mk

let string_of_kind = KindAtom.extract

let kind_equal = KindAtom.equal

type identifier = {
  kind : kind;
  raw  : string;
}

module IdentifierAtom = Atom.Make (struct
                                     type t    = identifier
                                     let hash  = Hashtbl.hash
                                     let equal = ( = )
                                   end)

type t = IdentifierAtom.t

let mk kind raw =
  IdentifierAtom.mk {
    kind  = kind;
    raw   = raw;
  }

let fresh =
  let r = ref 0 in
    fun ?(prefix="x") kind ->
      incr r;
      mk kind (prefix ^ string_of_int !r)

let kind identifier = (IdentifierAtom.extract identifier).kind

let as_string identifier = (IdentifierAtom.extract identifier).raw

let classify kind identifier = mk kind (as_string identifier)

let equal = IdentifierAtom.equal

let ( ^^ ) p i = mk (kind i) (as_string p ^ "/" ^ as_string i)

module OrderedIdentifier = struct
  type t = IdentifierAtom.t
  let compare = compare
end

module IMap : Map.S with type key = t 
= Map.Make (OrderedIdentifier)

module IHashTable : Hashtbl.S with type key = t 
= Hashtbl.Make (IdentifierAtom)

module ISet : Set.S with type elt = t 
= Set.Make (OrderedIdentifier)
