datatype pair a b = Pair of a * b

structure M : sig
    class default
    val get : t ::: Type -> default t -> t

    val string_default : default string
    val int_default : default int

    val option_default : t ::: Type -> default t -> default (option t)
    val pair_default : a ::: Type -> b ::: Type -> default a -> default b -> default (pair a b)
end = struct
    class default t = t
    fun get (t ::: Type) (x : t) = x

    val string_default = "Hi"
    val int_default = 0

    fun option_default (t ::: Type) (x : t) = Some x
    fun pair_default (a ::: Type) (b ::: Type) (x : a) (y : b) = Pair (x, y)
end

open M

fun default (t ::: Type) (_ : default t) : t = get
val hi : string = default
val zero : int = default
val some_zero : option int = default
val hi_zero : pair string int = default

fun frob (t ::: Type) (_ : default t) : t = default
val hi_again : string = frob
val zero_again : int = frob

fun show_option (t ::: Type) (_ : show t) : show (option t) =
    mkShow (fn x =>
               case x of
                   None => "None"
                 | Some y => show y)

fun show_pair (a ::: Type) (b ::: Type) (_ : show a) (_ : show b) : show (pair a b) =
    mkShow (fn x =>
               case x of
                   Pair (y, z) => "(" ^ show y ^ "," ^ show z ^ ")")

fun main () : transaction page = return <xml><body>
  {[hi_again]}, {[zero_again]}, {[some_zero]}, {[hi_zero]}
</body></xml>
