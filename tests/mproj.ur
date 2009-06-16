structure M : sig
    type t
    val x : t

    structure S : sig
        type u = t

        val eq : eq u
    end
end = struct
    type t = int
    val x = 0

    structure S = struct
        type u = t

        val eq = _
    end
end

val y = M.x = M.x
