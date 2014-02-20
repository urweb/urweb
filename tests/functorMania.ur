functor F1(M : sig type t end) = struct
    type t = M.t
    fun g () : M.t = g ()
    fun f () = g ()
end
functor F2(M : sig type t end) = F1(M)
functor F3(M : sig type t end) = F2(M)

functor F4(M : sig end) = F1(struct type t = int end)
functor F5(M : sig end) = F2(struct type t = int end)
functor F6(M : sig end) = F3(struct type t = int end)

functor F7(M : sig end) = F1(struct type t = string end)
functor F8(M : sig end) = F2(struct type t = string end)
functor F9(M : sig end) = F3(struct type t = string end)

structure M1 = F1(struct type t = string end)
structure M2 = F2(struct type t = string end)
structure M3 = F3(struct type t = string end)

structure M4 = F4(struct  end)
structure M5 = F5(struct end)
structure M6 = F6(struct end)

structure M7 = F7(struct end)
structure M8 = F8(struct end)
structure M9 = F9(struct end)

fun loop x : unit = loop (M1.f (), M2.f (), M3.f (),
                          M4.f (), M5.f (), M6.f (),
                          M7.f (), M8.f (), M9.f ())

fun main () : transaction page =
    x <- error <xml/>;
    u <- return (loop x);
    return <xml/>
