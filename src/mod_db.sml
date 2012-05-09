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
               Deps : SS.set}

val byName = ref (SM.empty : oneMod SM.map)
val byId = ref (IM.empty : string IM.map)

fun reset () = (byName := SM.empty;
                byId := IM.empty)

fun insert (d, tm) =
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
                val skipIt =
                    case SM.find (!byName, x) of
                        NONE => false
                      | SOME r => #When r = tm
            in
                if skipIt then
                    ()
                else
                    let
                        fun doMod (n', deps) =
                            case IM.find (!byId, n') of
                                NONE => deps
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
                                              Deps = deps});
                        byId := IM.insert (!byId, n, x)
                    end
            end
    end

fun lookup (d : Source.decl) =
    case #1 d of
        Source.DStr (x, _, SOME tm, _) =>
        (case SM.find (!byName, x) of
             NONE => NONE
           | SOME r =>
             if tm = #When r then
                 SOME (#Decl r)
             else
                 NONE)
      | Source.DFfiStr (x, _, SOME tm) =>
        (case SM.find (!byName, x) of
             NONE => NONE
           | SOME r =>
             if tm = #When r then
                 SOME (#Decl r)
             else
                 NONE)
      | _ => NONE

val byNameBackup = ref (!byName)
val byIdBackup = ref (!byId)

fun snapshot () = (byNameBackup := !byName; byIdBackup := !byId)
fun revert () = (byName := !byNameBackup; byId := !byIdBackup)

end
