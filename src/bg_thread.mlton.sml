(* Notice: API is kinda bad. We only allow queuing a single task per file *)
(* This works for us because we only do elaboration in the background, nothing else *)

structure BgThread:> BGTHREAD = struct
      open Posix.Signal
      open MLton
      open Itimer Signal Thread

      val topLevel: Thread.Runnable.t option ref = ref NONE
      val currentRunningThreadIsForFileName: string ref = ref ""
      (* FIFO queue: Max one task per fileName *)
      val tasks: ((Thread.Runnable.t * string) list) ref = ref []
      fun hasBgTasks () = List.length (!tasks) > 0

      fun setItimer t =
         Itimer.set (Itimer.Real,
                     {value = t,
                      interval = t})


      fun done () =  Thread.atomically
                         (fn () =>
                             ( tasks := (List.filter (fn q => #2 q <> (!currentRunningThreadIsForFileName)) (!tasks))
                             ; case !tasks of
                                   [] => (setItimer Time.zeroTime
                                         ; currentRunningThreadIsForFileName := ""
                                         ; switch (fn _ => valOf (!topLevel)))
                                 | t :: rest => (currentRunningThreadIsForFileName := #2 t
                                                ; switch (fn  _ => #1 t))))

      fun queueBgTask fileName f = 
          let
              fun new (f: unit -> unit): Thread.Runnable.t =
                  Thread.prepare
                      (Thread.new (fn () => ((f () handle _ => done ())
                                            ; done ())),
                       ())
          in
              case List.find (fn t => #2 t = fileName) (!tasks) of
                  NONE => tasks := (new f, fileName) :: (!tasks)
                | SOME t =>
                  (* Move existing task to front of list *)
                  tasks := t :: List.filter (fn q => #2 q <> fileName) (!tasks)
          end

      fun replaceInList (l: 'a list) (f: 'a -> bool) (replacement: 'a) =
          List.map (fn a => if f a then replacement else a ) l
      fun runBgTaskForABit () =
          case !(tasks) of
              [] => ()
            | t :: rest =>
              (setHandler (alrm, Handler.handler (fn t => (setItimer Time.zeroTime
                                                          (* This might some not needed, but other wise you get "Dead thread" error *)
                                                          ; tasks := replaceInList
                                                                         (!tasks)
                                                                         (fn t => #2 t = (!currentRunningThreadIsForFileName))
                                                                         (t, (!currentRunningThreadIsForFileName))
                                                          ; currentRunningThreadIsForFileName := ""
                                                          ; valOf (!topLevel))))
              ; setItimer (Time.fromMilliseconds 200)
              ; currentRunningThreadIsForFileName := #2 t
              ; switch (fn top => (topLevel := SOME (Thread.prepare (top, ())); #1 t)) (* store top level thread and activate BG thread *)
              ; setItimer Time.zeroTime
              )
   end
