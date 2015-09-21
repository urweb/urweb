(* Copyright (c) 2014, Adam Chlipala
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

structure DbModeCheck :> DB_MODE_CHECK = struct

open Mono

structure IM = IntBinaryMap

fun classify (ds, ps) =
    let
        fun mergeModes (m1, m2) =
            case (m1, m2) of
                (NoDb, _) => m2
              | (_, NoDb) => m1
              | _ => AnyDb

        fun modeOf modes =
            MonoUtil.Exp.fold {typ = fn (_, dbm) => dbm,
                               exp = fn (EQuery _, dbm) => mergeModes (OneQuery, dbm)
                                      | (EDml _, _) => AnyDb
                                      | (ENextval _, _) => AnyDb
                                      | (ESetval _, _) => AnyDb
                                      | (ENamed n, dbm) =>
                                        (case IM.find (modes, n) of
                                             NONE => dbm
                                           | SOME dbm' => mergeModes (dbm, dbm'))
                                      | (_, dbm) => dbm} NoDb

        fun decl ((d, _), modes) =
            case d of
                DVal (x, n, _, e, _) => IM.insert (modes, n, modeOf modes e)
              | DValRec xes =>
                let
                    val mode = foldl (fn ((_, _, _, e, _), mode) =>
                                         let
                                             val mode' = modeOf modes e
                                         in
                                             case mode' of
                                                 NoDb => mode
                                               | _ => AnyDb
                                         end) NoDb xes
                in
                    foldl (fn ((_, n, _, _, _), modes) => IM.insert (modes, n, mode)) modes xes
                end
              | _ => modes

        val modes = foldl decl IM.empty ds

        val (ps, modes) = ListUtil.foldlMap (fn ((n, side, _), modes) =>
                                                case IM.find (modes, n) of
                                                    NONE => ((n, side, AnyDb), modes)
                                                  | SOME mode => ((n, side, mode), #1 (IM.remove (modes, n))))
                                            modes ps

        val ps = IM.foldli (fn (n, mode, ps) => (n, ServerOnly, mode) :: ps) ps modes
    in
        (ds, ps)
    end

end

