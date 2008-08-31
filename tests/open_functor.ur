signature S = sig
        type t
        val x : t
end

functor F (M : S) : S where type t = M.t = struct
        type t = M.t
        val x = M.x
end

structure M = F(struct
        type t = int
        val x = 0
end)

open M
