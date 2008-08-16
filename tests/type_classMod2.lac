signature S = sig
        class c
        val default : t :: Type -> c t -> t

        val string_c : c string
        val int_c : c int
end

structure M : S = struct
        class c t = t
        val default = fn t :: Type => fn v : c t => v

        val int_c : c int = 0
        val string_c : c string = "Hi"
end

val hi = M.default [string] _
val zero = M.default [int] _
