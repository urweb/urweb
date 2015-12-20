structure Cache = struct

type cache =
     {(* Takes a query ID and parameters (and, for store, the value to
         store) and gives an FFI call that checks, stores, or flushes the
         relevant entry. The parameters are strings for check and store and
         optional strings for flush because some parameters might not be
         fixed. *)
      check : int * Mono.exp list -> Mono.exp',
      store : int * Mono.exp list * Mono.exp -> Mono.exp',
      flush : int * Mono.exp list -> Mono.exp',
      lock : int * bool (* true = write, false = read *) -> Mono.exp',
      (* Generates C needed for FFI calls in check, store, and flush. *)
      setupGlobal : Print.PD.pp_desc,
      setupQuery : {index : int, params : int} -> Print.PD.pp_desc}

end
