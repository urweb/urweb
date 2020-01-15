(*
  Dummy implementation. Threading is only supported in MLton.
  All other implementations just immediately run the background tasks
*)
structure BgThread:> BGTHREAD = struct
  fun queueBgTask filename f = f ()
  fun hasBgTasks () = false
  fun runBgTaskForABit () = ()
end
