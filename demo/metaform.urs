functor Make (M : sig
                  con fs :: {Unit}
                  val names : $(mapUT string fs)
              end) : sig
    val main : unit -> transaction page
end
