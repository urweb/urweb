functor Make(M : sig end) = struct
    datatype t = A | B
end

structure A = Make(struct end)
structure B = Make(struct end)

fun main (x : A.t) (y : B.t) : transaction page =
    return <xml/>
