functor OptionKeyFn(K : ORD_KEY)
        : ORD_KEY where type ord_key = K.ord_key option = struct

type ord_key = K.ord_key option

val compare =
 fn (NONE, NONE) => EQUAL
  | (NONE, _) => LESS
  | (_, NONE) => GREATER
  | (SOME x, SOME y) => K.compare (x, y)

end
