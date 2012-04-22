fun f [r] [[Id, Foo] ~ r] (x : $(map (fn ts => $ts) r ++ [Id = int, Foo = string]))
    : $(map (fn ts => $ts) r ++ [Foo = string]) = x

val r = {}
val x : $(map (fn ts => $ts) _ ++ [Foo = string]) = f r
