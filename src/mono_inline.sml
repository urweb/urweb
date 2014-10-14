structure MonoInline = struct

fun inlineFull file =
    let
        val oldInline = Settings.getMonoInline ()
        val oldFull = !MonoReduce.fullMode
    in
        (Settings.setMonoInline (case Int.maxInt of
                                     NONE => 1000000
                                   | SOME n => n);
         MonoReduce.fullMode := true;
         let
             val file = MonoReduce.reduce file
             val file = MonoOpt.optimize file
             val file = Fuse.fuse file
             val file = MonoOpt.optimize file
             val file = MonoShake.shake file
         in
             file
         end before
         (MonoReduce.fullMode := oldFull;
          Settings.setMonoInline oldInline))
        handle ex => (Settings.setMonoInline oldInline;
                      MonoReduce.fullMode := oldFull;
                      raise ex)
    end

end
