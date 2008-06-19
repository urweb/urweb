signature S = sig
        type t
        val z : t
        val s : t -> t
end

signature T = sig
        type t
        val three : t
end

functor F (M : S) : T where type t = M.t = struct
        type t = M.t
        val three = M.s (M.s (M.s M.z))
end


structure O = F (struct
        type t = int
        val z = 0
        val s = fn x : t => x
end)
val three : int = O.three

structure S = struct
        type t = int
        val z = 0
        val s = fn x : t => x
end
structure SO = F (S)
val three : int = SO.three

structure SS : S = S
structure SSO = F (SS)
val three : SS.t = SSO.three

val main = three
