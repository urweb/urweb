functor ListKeyFn(K : ORD_KEY)
        : ORD_KEY where type ord_key = K.ord_key list = struct

type ord_key = K.ord_key list

val rec compare =
 fn ([], []) => EQUAL
  | ([], _) => LESS
  | (_, []) => GREATER
  | (x::xs, y::ys) => case K.compare (x, y) of
                              EQUAL => compare (xs, ys)
                            | ord => ord

end
