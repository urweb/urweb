signature S1 = sig
        type t
        val zero : t
end
signature S2 = sig
        type t = int
        val zero : t
end
structure S = struct
        type t = int
        val zero = 0
end
structure S1 : S1 = S
structure S2 : S2 = S

type t = S1.t
val zero : t = S1.zero

type t = S2.t
val zero : int = S2.zero

structure T = S1
val main : S1.t = T.zero
