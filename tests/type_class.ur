datatype pair a b = Pair of a * b

structure M : sig
    class default
    val get : t ::: Type -> default t -> t

    val string_default : default string
    val int_default : default int

    val option_default : t ::: Type -> default t -> default (option t)
    val pair_default : a ::: Type -> b ::: Type -> default a -> default b -> default (pair a b)

    (*val uh_oh : t ::: Type -> default t -> default t*)

    class awesome
    val awesome_default : t ::: Type -> awesome t -> default t

    val float_awesome : awesome float

    val oh_my : t ::: Type -> awesome (option t) -> awesome (option t)

    val awesome : t ::: Type -> awesome t -> t
end = struct
    class default t = t
    fun get (t ::: Type) (x : t) = x

    val string_default = "Hi"
    val int_default = 0

    fun option_default (t ::: Type) (x : t) = Some x
    fun pair_default (a ::: Type) (b ::: Type) (x : a) (y : b) = Pair (x, y)

    (*fun uh_oh (t ::: Type) (x : t) = x*)

    class awesome t = t
    fun awesome_default (t ::: Type) (x : t) = x

    val float_awesome = 1.23

    fun oh_my (t ::: Type) (x : option t) = x

    fun awesome (t ::: Type) (x : t) = x
end

open M

fun default (t ::: Type) (_ : default t) : t = get
val hi : string = default
val zero : int = default
val some_zero : option int = default
val hi_zero : pair string int = default
val ott : float = default

fun frob (t ::: Type) (_ : default t) : t = default
val hi_again : string = frob
val zero_again : int = frob

fun show_option (t ::: Type) (_ : show t) : show (option t) =
    mkShow (fn x =>
               case x of
                   None => "None"
                 | Some y => show y)

(*val x : option float = awesome*)

fun show_pair (a ::: Type) (b ::: Type) (_ : show a) (_ : show b) : show (pair a b) =
    mkShow (fn x =>
               case x of
                   Pair (y, z) => "(" ^ show y ^ "," ^ show z ^ ")")

fun main () : transaction page = return <xml><body>
  {[hi_again]}, {[zero_again]}, {[some_zero]}, {[hi_zero]}, {[ott]}
</body></xml>
