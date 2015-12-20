functor TripleKeyFn (structure I : ORD_KEY
                     structure J : ORD_KEY
                     structure K : ORD_KEY)
        : ORD_KEY where type ord_key = I.ord_key * J.ord_key * K.ord_key = struct

type ord_key = I.ord_key * J.ord_key * K.ord_key

fun compare ((i1, j1, k1), (i2, j2, k2)) =
    case I.compare (i1, i2) of
        EQUAL => (case J.compare (j1, j2) of
                      EQUAL => K.compare (k1, k2)
                    | ord => ord)
      | ord => ord

end
