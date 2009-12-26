fun id [t :: Type] (x : t) = x
val id_self = id [t :: Type -> t -> t] id

fun idi [t ::: Type] (x : t) = x
val idi_self = idi @@idi

fun picker [na :: Name] [a ::: Type] [nb :: Name] [b ::: Type] [fs ::: {Type}] [[na] ~ [nb]] [[na, nb] ~ fs]
        (r : $([na = a, nb = b] ++ fs)) = {na = r.na, nb = r.nb}
val getem = picker [#A] [#C] {A = 0, B = 1.0, C = "hi", D = {}}
val getem2 = picker [#A] [_] {A = 0, B = 1.0, C = "hi", D = {}}
val getem3 = picker [#A] [_::Name] {A = 0, B = 1.0, C = "hi", D = {}}

fun picker_ohmy [na ::: Name] [a ::: Type] [nb ::: Name] [b ::: Type] [fs ::: {Type}] [[na] ~ [nb]] [[na, nb] ~ fs]
        (r : $([na = a, nb = b] ++ fs)) = {na = r.na, nb = r.nb}
val getem_ohmy = picker_ohmy {A = 0, B = 1.0, C = "hi", D = {}}

fun proj [fs] [t] [nm :: Name] [[nm] ~ fs] (r : $([nm = t] ++ fs)) = r.nm
val one = proj [#A] {A = 1, B = True}
