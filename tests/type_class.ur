class default t = t

val string_default : default string = "Hi"
val int_default : default int = 0

val default : t :: Type -> default t -> t =
        fn t :: Type => fn d : default t => d
val hi = default [string] _
val zero = default [int] _

val frob : t :: Type -> default t -> t =
        fn t :: Type => fn _ : default t => default [t] _
val hi_again = frob [string] _
val zero_again = frob [int] _

val main : unit -> page = fn () => <html><body>
        {cdata hi_again}
</body></html>
