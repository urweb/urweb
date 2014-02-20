functor F(M : sig end) = struct
    fun f () = f ()

    functor G(M : sig end) = struct
        fun g () = f ()
    end
end

structure M1 = F(struct end)
structure M2 = F(struct end)

structure N1 = M1.G(struct end)
structure N2 = M2.G(struct end)

fun main () : transaction page =
    return (N1.g ());
    return (N2.g ());
    return <xml/>
