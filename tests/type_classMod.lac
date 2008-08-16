structure M = struct
        structure N = struct
                class c t = t
                val string_c : c string = "Hi"
        end
end

val c : t :: Type -> M.N.c t -> t =
        fn t :: Type => fn pf : M.N.c t => pf
val hi = c [string] _

val bool_c : M.N.c bool = True
val true = c [bool] _
val hi = c [string] _

con c = M.N.c
val int_c : c int = 0
val zero = c [int] _
