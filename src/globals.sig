signature GLOBALS = sig

    (* When was the Ur/Web compiler started or reset? *)
    val setResetTime : unit -> unit
    val getResetTime : unit -> Time.time

end
