signature S = sig
    type t
    val x : t
end

signature T = sig
    structure M : S

    type u
    val y : u

    structure N : S
end

structure M : T = struct
    structure M = struct
        val x = True
    end

    val y = 0

    structure N = struct
        val x = "hi"
    end
end
