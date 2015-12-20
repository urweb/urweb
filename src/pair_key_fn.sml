functor PairKeyFn (structure I : ORD_KEY
                     structure J : ORD_KEY)
        : ORD_KEY where type ord_key = I.ord_key * J.ord_key = struct

type ord_key = I.ord_key * J.ord_key

fun compare ((i1, j1), (i2, j2)) =
    case I.compare (i1, i2) of
        EQUAL => J.compare (j1, j2)
      | ord => ord

end
