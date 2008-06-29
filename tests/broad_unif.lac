structure M = struct
        type t = int
        val f = fn x => x
        val y = f 0
end

signature S = sig
        type t
        val f : t -> t
end

structure M : S = struct
        type t = int
        val f = fn x => x
end
