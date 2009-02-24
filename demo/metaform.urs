functor Make (M : sig
                  con fs :: {Unit}
                  val fl : folder fs
                  val names : $(mapUT string fs)
              end) : sig
    val main : unit -> transaction page
end
