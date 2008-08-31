extern structure Lib : sig
        type t
        type u
        val x : t
        val y : u
        val f0 : {} -> u
        val f1 : t -> t
        val f2 : t -> u -> t
end

type t' = Lib.t
val x' : t' = Lib.x
val f0' = Lib.f0
val f1' = Lib.f1
val f2' = Lib.f2

structure Lib' = Lib

type t'' = Lib'.t
val x'' : t'' = Lib'.x

val main = f2' (f1' x') (f0' {})
