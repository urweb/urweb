(* Copyright (c) 2013, Adam Chlipala
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

structure SigCheck :> SIG_CHECK = struct

open Mono

structure IS = IntBinarySet
structure E = ErrorMsg

fun check (ds, sl) =
    let
        fun isSiggy siggers =
            MonoUtil.Decl.exists {typ = fn _ => false,
                                  decl = fn _ => false,
                                  exp = fn e =>
                                        case e of
                                            ERel n => IS.member (siggers, n)
                                          | EFfiApp ("Basis", "sigString", _) => true
                                          | _ => false}

        fun sigify' sigdecs e =
            case e of
                ENamed n => if IS.member (sigdecs, n) then
                                (EApp ((e, E.dummySpan),
                                       (ERecord [], E.dummySpan)))
                            else
                                e
              | _ => e

        fun sigify sigdecs =
            MonoUtil.Decl.map {typ = fn x => x,
                               decl = fn d => d,
                               exp = sigify' sigdecs}

        fun sigifyE sigdecs =
            MonoUtil.Exp.map {typ = fn x => x,
                              exp = sigify' sigdecs}

        fun isFun (e, _) =
            case e of
                EAbs _ => true
              | _ => false

        fun doDecl (d : decl, (siggers, sigdecs)) =
            case #1 d of
                DVal (x, n, t, e, s) =>
                if isSiggy siggers d then
                    if isFun e then
                        (sigify sigdecs d, (IS.add (siggers, n), sigdecs))
                    else
                        ((DVal (x, n, (TFun ((TRecord [], #2 d), t), #2 d),
                                (EAbs ("_", (TRecord [], #2 d), t, sigifyE sigdecs e), #2 d),
                                s), #2 d),
                         (IS.add (siggers, n),
                          IS.add (sigdecs, n)))
                else
                    (sigify sigdecs d, (siggers, sigdecs))
              | DValRec vis =>
                if isSiggy siggers d then
                    (sigify sigdecs d,
                     (foldl IS.add' siggers (map #2 vis),
                      sigdecs))
                else
                    (sigify sigdecs d, (siggers, sigdecs))
              | _ => (sigify sigdecs d, (siggers, sigdecs))

        val (ds, _) = ListUtil.foldlMap doDecl (IS.empty, IS.empty) ds
    in
        (ds, sl)
    end

end
