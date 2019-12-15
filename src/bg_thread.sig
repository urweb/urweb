(* Notice: API is kinda bad. We only allow queuing a single task per file *)
(* This works for us because we only do elaboration in the background, nothing else *)
signature BGTHREAD = sig 
    val queueBgTask: string (* fileName *) -> (unit -> unit) -> unit
    val hasBgTasks: unit -> bool
    val runBgTaskForABit: unit -> unit
end
