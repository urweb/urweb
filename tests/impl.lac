val id = fn t :: Type => fn x : t => x
val id_self = id [t :: Type -> t -> t] id

val idi = fn t ::: Type => fn x : t => x
val idi_self = idi idi

val picker = fn na :: Name => fn a ::: Type => fn nb :: Name => fn b ::: Type => fn fs ::: {Type} =>
        fn r : $([na = a, nb = b] ++ fs) => {na = r.na, nb = r.nb}
val getem = picker [#A] [#C] {A = 0, B = 1.0, C = "hi", D = {}}
val getem2 = picker [#A] [_] {A = 0, B = 1.0, C = "hi", D = {}}
val getem3 = picker [#A] [_::Name] {A = 0, B = 1.0, C = "hi", D = {}}

val picker_ohmy = fn na ::: Name => fn a ::: Type => fn nb ::: Name => fn b ::: Type => fn fs ::: {Type} =>
        fn r : $([na = a, nb = b] ++ fs) => {na = r.na, nb = r.nb}
val getem_ohmy = picker_ohmy {A = 0, B = 1.0, C = "hi", D = {}}
