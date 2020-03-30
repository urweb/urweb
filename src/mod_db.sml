(* Copyright (c) 2012, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

(* Cache of module code, with dependency information *)

structure ModDb :> MOD_DB = struct

open Elab

structure SK = struct
type ord_key = string
val compare = String.compare
end

structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)
structure IM = IntBinaryMap

type oneMod = {Decl : decl,
               When : Time.time,
               Deps : SS.set,
               HasErrors: bool ref (* We're saving modules with errors so tooling can find them *)
              }

val byName = ref (SM.empty : oneMod SM.map)
val byId = ref (IM.empty : string IM.map)

fun reset () = (byName := SM.empty;
                byId := IM.empty)

(* For debug purposes *)
fun printByName (bn: oneMod SM.map): unit =
    (TextIO.print ("Contents of ModDb.byName: \n");
     List.app (fn tup =>
                  let
                      val name = #1 tup
                      val m = #2 tup
                      val renderedDeps =
                          String.concatWith ", " (SS.listItems (#Deps m))
                      val renderedMod =
                          "  " ^ name
                          ^ ". Stored at : " ^ Time.toString (#When m)
                          ^", HasErrors: " ^ Bool.toString (!(#HasErrors m))
                          ^". Deps: " ^ renderedDeps ^"\n"
                  in
                      TextIO.print renderedMod
                  end)
              (SM.listItemsi bn))

fun dContainsUndeterminedUnif d =
    ElabUtil.Decl.exists
        {kind = fn _ => false,
         con = fn _ => false,
         exp = fn e => case e of
                           EUnif (ref NONE) => true 
                         | _ => false,
         sgn_item = fn _ => false,
         sgn = fn _ => false,
         str = fn _ => false,
         decl = fn _ => false}
        d

fun insert (d, tm, hasErrors) =
    let
        val xn =
            case #1 d of
                DStr (x, n, _, _) => SOME (x, n)
              | DFfiStr (x, n, _) => SOME (x, n)
              | _ => NONE
    in
        case xn of
            NONE => ()
          | SOME (x, n) =>
            let
                (* Keep module when it's file didn't change and it was OK before *)
                val skipIt =
                    case SM.find (!byName, x) of
                        NONE => false
                      | SOME r => #When r = tm
                                  andalso not (!(#HasErrors r))
                                  (* We save results of error'd compiler passes *)
                                  (* so modules that still have undetermined unif variables *)
                                  (* should not be reused since those are unsuccessfully compiled *)
                                  andalso not (dContainsUndeterminedUnif (#Decl r))
            in
                if skipIt then
                    ()
                else
                    let
                        fun doMod (n', deps) =
                            case IM.find (!byId, n') of
                                NONE =>
                                (
                                  (* TextIO.print ("MISSED_DEP: " ^ Int.toString n' ^"\n"); *)
                                 deps)
                                (* raise Fail ("ModDb: Trying to make dep tree but couldn't find module " ^ Int.toString n') *)
                              (* I feel like this should throw, but the dependency searching algorithm *)
                              (* is not 100% precise. I encountered problems in json.urs: *)
                              (*   datatype r = Rec of M.t r *)
                              (* M is the structure passed to the Recursive functor, so this is not an external dependency *)
                              (* I'm just not sure how to filter these out yet *)
                              (* I still think this should throw: *)
                              (* Trying to add a dep for a module but can't find the dep... *)
                              (* That will always cause a hole in the dependency tree and cause problems down the line *)
                              | SOME x' =>
                                SS.union (deps,
                                          SS.add (case SM.find (!byName, x') of
                                                      NONE => SS.empty
                                                    | SOME {Deps = ds, ...} => ds, x'))

                        val deps = ElabUtil.Decl.fold {kind = #2,
                                                       con = fn (c, deps) =>
                                                                case c of
                                                                    CModProj (n', _, _) => doMod (n', deps)
                                                                  | _ => deps,
                                                       exp = fn (e, deps) =>
                                                                case e of
                                                                    EModProj (n', _, _) => doMod (n', deps)
                                                                  | _ => deps,
                                                       sgn_item = #2,
                                                       sgn = fn (sg, deps) =>
                                                                case sg of
                                                                    SgnProj (n', _, _) => doMod (n', deps)
                                                                  | _ => deps,
                                                       str = fn (st, deps) =>
                                                                case st of
                                                                    StrVar n' => doMod (n', deps)
                                                                  | _ => deps,
                                                       decl = fn (d, deps) =>
                                                                 case d of
                                                                     DDatatypeImp (_, _, n', _, _, _, _) => doMod (n', deps)
                                                                   | _ => deps}
                                                      SS.empty d
                    in
                        byName := SM.insert (SM.filter (fn r => if SS.member (#Deps r, x) then
                                                                    case #1 (#Decl r) of
                                                                        DStr (_, n', _, _) =>
                                                                        (byId := #1 (IM.remove (!byId, n'));
                                                                         false)
                                                                      | DFfiStr (_, n', _) =>
                                                                        (byId := #1 (IM.remove (!byId, n'));
                                                                         false)
                                                                      | _ => raise Fail "ModDb: Impossible decl"
                                                                else
                                                                    true) (!byName),
                                             x,
                                             {Decl = d,
                                              When = tm,
                                              Deps = deps,
                                              HasErrors = hasErrors
                                            });
                        byId := IM.insert (!byId, n, x)
                        (* printByName (!byName) *)
                    end
            end
    end

fun lookup (d : Source.decl) =
    case #1 d of
        Source.DStr (x, _, SOME tm, _, _) =>
        (case SM.find (!byName, x) of
             NONE => NONE
           | SOME r =>
             if tm = #When r andalso not (!(#HasErrors r)) andalso not (dContainsUndeterminedUnif (#Decl r)) then
                 SOME (#Decl r)
             else
                 NONE)
      | Source.DFfiStr (x, _, SOME tm) =>
        (case SM.find (!byName, x) of
             NONE => NONE
           | SOME r =>
             if tm = #When r andalso not (!(#HasErrors r)) andalso not (dContainsUndeterminedUnif (#Decl r)) then
                 SOME (#Decl r)
             else
                 NONE)
      | _ => NONE

fun lookupModAndDepsIncludingErrored name =
    case SM.find (!byName, name) of
        NONE => NONE
      | SOME m =>
        let
            val deps = SS.listItems (#Deps m)
            (* Clumsy way of adding Basis and Top without adding doubles *)
            val deps = List.filter (fn x => x <> "Basis" andalso x <> "Top") deps
            val deps = ["Basis", "Top"] @ deps
            val foundDepModules = List.mapPartial (fn d => SM.find (!byName, d)) deps
        in
            SOME (#Decl m, List.map (fn a => #Decl a) foundDepModules)
        end

val byNameBackup = ref (!byName)
val byIdBackup = ref (!byId)

fun snapshot () = (byNameBackup := !byName; byIdBackup := !byId)
fun revert () = (byName := !byNameBackup; byId := !byIdBackup)

end
