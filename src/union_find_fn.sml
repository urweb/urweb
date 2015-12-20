functor UnionFindFn(K : ORD_KEY) :> sig
    type unionFind
    val empty : unionFind
    val union : unionFind * K.ord_key * K.ord_key -> unionFind
    val union' : (K.ord_key * K.ord_key) * unionFind -> unionFind
    val together : unionFind * K.ord_key * K.ord_key -> bool
    val classes : unionFind -> K.ord_key list list
end = struct

structure M = BinaryMapFn(K)
structure S = BinarySetFn(K)

datatype entry =
         Set of S.set
       | Pointer of K.ord_key

(* First map is the union-find tree, second stores equivalence classes. *)
type unionFind = entry M.map ref * S.set M.map

val empty : unionFind = (ref M.empty, M.empty)

fun findPair (uf, x) =
    case M.find (!uf, x) of
        NONE => (S.singleton x, x)
      | SOME (Set set) => (set, x)
      | SOME (Pointer parent) =>
        let
            val (set, rep) = findPair (uf, parent)
        in
            uf := M.insert (!uf, x, Pointer rep);
            (set, rep)
        end

fun find ((uf, _), x) = (S.listItems o #1 o findPair) (uf, x)

fun classes (_, cs) = (map S.listItems o M.listItems) cs

fun together ((uf, _), x, y) = case K.compare (#2 (findPair (uf, x)), #2 (findPair (uf, y))) of
                                   EQUAL => true
                                 | _ => false

fun union ((uf, cs), x, y) =
    let
        val (xSet, xRep) = findPair (uf, x)
        val (ySet, yRep) = findPair (uf, y)
        val xySet = S.union (xSet, ySet)
    in
        (ref (M.insert (M.insert (!uf, yRep, Pointer xRep),
                        xRep, Set xySet)),
         M.insert (case M.find (cs, yRep) of
                       NONE => cs
                     | SOME _ => #1 (M.remove (cs, yRep)),
                   xRep, xySet))
    end

fun union' ((x, y), uf) = union (uf, x, y)

end
