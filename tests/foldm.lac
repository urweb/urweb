con currier = fold (fn nm => fn t => fn acc => t -> acc) {}

signature S = sig
        type t
        val x : t

        con rs :: {Type}
        val create : currier rs -> t
end

functor Currier (M : sig con rs :: {Type} end) : S where con rs = M.rs = struct
        val currier : rs :: {Type} -> currier rs =
                fold [currier] (fn nm :: Name => fn t :: Type => fn rest :: {Type} => fn acc => fn x : t => acc) {}

        type t = currier M.rs
        val x = currier [M.rs]

        con rs = M.rs
        val create : t -> t = fn x => x
end

structure ChefsSpecial = Currier(struct
        con rs = [A = int, B = float]
end)

val main = ChefsSpecial.x
