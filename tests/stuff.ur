type c1 = t :: Type -> t
con c2 :: Type = t :: Type -> t
con c3 = fn t :: Type => c1
con c4 = c3 c1
con c5 = (fn t :: Type => c1) c1

con name = #MyName

con c6 = {A : c1, name : c2}
con c7 = [A = c1, name = c2]

con c8 = fn t :: Type => t

con c9 = {}
con c10 = ([]) :: {Type}

val v1 = fn t :: Type => fn x : t => x
val v2 = v1 [t :: Type -> t -> t] v1

val r = {X = v1, Y = v2}
val v1_again = r.X
val v2_again = r.Y

val r2 = {X = {}, Y = v2, Z = {}}
val r2_X = r2.X
val r2_Y = r2.Y
val r2_Z = r2.Z

val f = fn fs :: {Type} => fn x : $([X = {}] ++ fs) => x.X
val f2 = fn fs :: {Type} => fn x : $(fs ++ [X = {}]) => x.X
val f3 = fn fs :: {Type} => fn x : $([X = {}, Y = {Z : {}}] ++ fs) => x.X
val f4 = fn fs :: {Type} => fn x : $([X = {}, Y = {Z : {}}] ++ fs) => x.Y
val f5 = fn fs1 :: {Type} => fn fs2 :: {Type} => fn x : $(fs1 ++ [X = {}] ++ fs2) => x.X
val f6 = fn fs1 :: {Type} => fn fs2 :: {Type} => fn x : $(fs1 ++ [X = {}] ++ fs2 ++ [Y = {Z : {}}]) => x.X
val f7 = fn fs1 :: {Type} => fn fs2 :: {Type} => fn x : $(fs1 ++ [X = {}] ++ fs2 ++ [Y = {Z : {}}]) => x.Y

val test = f [[Y = t :: Type -> t -> t, Z = {}]] r2
val test = f7 [[Y = t :: Type -> t -> t]] [[Z = {}]] r2
