functor Make (M : sig
                  con fs :: {Unit}
                  val fl : folder fs
                  val names : $(mapU string fs)
              end) : sig
    val main : unit -> transaction page
end
