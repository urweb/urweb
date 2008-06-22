signature S = sig
        type t
        val x : t
end

signature S' = sig
        include S
        val y : t
end

signature S'' = sig
        type u
        include S' where type t = int
        type v
end
