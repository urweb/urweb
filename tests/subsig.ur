structure M = struct
        signature S = sig
                type t
        end
end

structure N : M.S = struct
        type t = int
end

structure M' = struct
        type t = int
        val y = 42

        signature S = sig
                val x : t
        end
end

structure N' : M'.S = struct
        val x = 0
end

signature S = sig
        type t
        val y : t

        signature S = sig
                val x : t
        end
end

structure M'S : S = M'

structure V : M'S.S = struct
        val x = M'S.y
end

structure M'S' = M'S

structure V : M'S'.S = struct
        val x = M'S.y
end
