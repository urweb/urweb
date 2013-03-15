(* Copyright (c) 2008, Adam Chlipala
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

structure Fuse :> FUSE = struct

open Mono
structure U = MonoUtil

structure IM = IntBinaryMap

fun returnsString (t, loc) =
    let
        fun rs (t, loc) =
            case t of
                TFfi ("Basis", "string") => SOME ([], (TRecord [], loc))
              | TFun (dom, ran) =>
                (case rs ran of
                     NONE => NONE
                   | SOME (args, ran') => SOME (dom :: args, (TFun (dom, ran'), loc)))
              | _ => NONE
    in
        case t of
            TFun (dom, ran) =>
            (case rs ran of
                 NONE => NONE
               | SOME (args, ran') => SOME (dom :: args, (TFun (dom, ran'), loc)))
          | _ => NONE
    end

fun fuse file =
    let
        fun doDecl (d as (_, loc), (funcs, maxName)) =
            let
                exception GetBody

                fun doVi ((x, n, t, e, s), funcs, maxName) =
                    case returnsString t of
                        NONE => (NONE, funcs, maxName)
                      | SOME (args, t') =>
                        let
                            fun getBody (e, args) =
                                case (#1 e, args) of
                                    (_, []) => (e, [])
                                  | (EAbs (x, t, _, e), _ :: args) =>
                                    let
                                        val (body, args') = getBody (e, args)
                                    in
                                        (body, (x, t) :: args')
                                    end
                                  | _ => raise GetBody

                            val (body, args) = getBody (e, args)
                            val body = MonoOpt.optExp (EWrite body, loc)
                            val (body, _) = foldr (fn ((x, dom), (body, ran)) =>
                                                      ((EAbs (x, dom, ran, body), loc),
                                                       (TFun (dom, ran), loc)))
                                                  (body, (TRecord [], loc)) args
                        in
                            (SOME (x, maxName, t', body, s),
                             IM.insert (funcs, n, maxName),
                             maxName + 1)
                        end
                        handle GetBody => (NONE, funcs, maxName)

                val (d, funcs, maxName) =
                    case #1 d of
                        DVal vi =>
                        let
                            val (vi', funcs, maxName) = doVi (vi, funcs, maxName)
                        in
                            (case vi' of
                                 NONE => d
                               | SOME vi' => (DValRec [vi, vi'], loc),
                             funcs, maxName)
                        end
                      | DValRec vis =>
                        let
                            val (vis', funcs, maxName) =
                                foldl (fn (vi, (vis', funcs, maxName)) =>
                                          let
                                              val (vi', funcs, maxName) = doVi (vi, funcs, maxName)
                                          in
                                              (case vi' of
                                                   NONE => vis'
                                                 | SOME vi' => vi' :: vis',
                                               funcs, maxName)
                                          end)
                                ([], funcs, maxName) vis
                        in
                            ((DValRec (vis @ vis'), loc), funcs, maxName)
                        end
                      | _ => (d, funcs, maxName)

                fun exp e =
                    case e of
                        EWrite e' =>
                        let
                            fun unravel (e, loc) =
                                case e of
                                    ENamed n =>
                                    (case IM.find (funcs, n) of
                                         NONE => NONE
                                       | SOME n' => SOME (ENamed n', loc))
                                  | EApp (e1, e2) =>
                                    (case unravel e1 of
                                         NONE => NONE
                                       | SOME e1 => SOME (EApp (e1, e2), loc))
                                  | _ => NONE
                        in
                            case unravel e' of
                                NONE => e
                              | SOME (e', _) => e'
                        end
                      | _ => e
            in
                (U.Decl.map {typ = fn x => x,
                             exp = exp,
                             decl = fn x => x} 
                            d,
                 (funcs, maxName))
            end

        val (ds, _) = ListUtil.foldlMap doDecl (IM.empty, U.File.maxName file + 1) (#1 file)
    in
        (ds, #2 file)
    end

end
