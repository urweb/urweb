(* Copyright (c) 2009, Adam Chlipala
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

structure Checknest :> CHECKNEST = struct

open Cjr

structure IS = IntBinarySet
structure IM = IntBinaryMap

fun expUses globals =
    let
        fun eu (e, _) =
            case e of
                EPrim _ => IS.empty
              | ERel _ => IS.empty
              | ENamed n => Option.getOpt (IM.find (globals, n), IS.empty)
              | ECon (_, _, NONE) => IS.empty
              | ECon (_, _, SOME e) => eu e
              | ENone _ => IS.empty
              | ESome (_, e) => eu e
              | EFfi _ => IS.empty
              | EFfiApp (_, _, es) => foldl IS.union IS.empty (map (eu o #1) es)
              | EApp (e, es) => foldl IS.union (eu e) (map eu es)

              | EUnop (_, e) => eu e
              | EBinop (_, e1, e2) => IS.union (eu e1, eu e2)

              | ERecord (_, xes) => foldl (fn ((_, e), s) => IS.union (eu e, s)) IS.empty xes
              | EField (e, _) => eu e

              | ECase (e, pes, _) => foldl (fn ((_, e), s) => IS.union (eu e, s)) (eu e) pes

              | EError (e, _) => eu e
              | EReturnBlob {blob = NONE, mimeType, ...} => eu mimeType
              | EReturnBlob {blob = SOME blob, mimeType, ...} => IS.union (eu blob, eu mimeType)
              | ERedirect (e, _) => eu e

              | EWrite e => eu e
              | ESeq (e1, e2) => IS.union (eu e1, eu e2)
              | ELet (_, _, e1, e2) => IS.union (eu e1, eu e2)

              | EQuery {query, body, initial, prepared, ...} =>
                let
                    val s = IS.union (eu query, IS.union (eu body, eu initial))
                in
                    case prepared of
                        SOME {id, ...} => IS.add (s, id)
                      | _ => s
                end
              | EDml {dml, prepared, ...} =>
                let
                    val s = eu dml
                in
                    case prepared of
                        SOME {id, ...} => IS.add (s, id)
                      | _ => s
                end
              | ENextval {seq, prepared, ...} =>
                let
                    val s = eu seq
                in
                    case prepared of
                        SOME {id, ...} => IS.add (s, id)
                      | _ => s
                end
              | ESetval {seq, count} => IS.union (eu seq, eu count)

              | EUnurlify (e, _, _) => eu e
    in
        eu
    end

fun annotateExp globals =
    let
        fun ae (e as (_, loc)) =
            case #1 e of
                EPrim _ => e
              | ERel _ => e
              | ENamed n => e
              | ECon (_, _, NONE) => e
              | ECon (dk, pc, SOME e) => (ECon (dk, pc, SOME (ae e)), loc)
              | ENone _ => e
              | ESome (t, e) => (ESome (t, ae e), loc)
              | EFfi _ => e
              | EFfiApp (m, f, es) => (EFfiApp (m, f, map (fn (e, t) => (ae e, t)) es), loc)
              | EApp (e, es) => (EApp (ae e, map ae es), loc)

              | EUnop (uo, e) => (EUnop (uo, ae e), loc)
              | EBinop (bo, e1, e2) => (EBinop (bo, ae e1, ae e2), loc)

              | ERecord (n, xes) => (ERecord (n, map (fn (x, e) => (x, ae e)) xes), loc)
              | EField (e, f) => (EField (ae e, f), loc)

              | ECase (e, pes, ts) => (ECase (ae e, map (fn (p, e) => (p, ae e)) pes, ts), loc)

              | EError (e, t) => (EError (ae e, t), loc)
              | EReturnBlob {blob = NONE, mimeType, t} => (EReturnBlob {blob = NONE, mimeType = ae mimeType, t = t}, loc)
              | EReturnBlob {blob = SOME blob, mimeType, t} => (EReturnBlob {blob = SOME (ae blob), mimeType = ae mimeType, t = t}, loc)
              | ERedirect (e, t) => (ERedirect (ae e, t), loc)

              | EWrite e => (EWrite (ae e), loc)
              | ESeq (e1, e2) => (ESeq (ae e1, ae e2), loc)
              | ELet (x, t, e1, e2) => (ELet (x, t, ae e1, ae e2), loc)

              | EQuery {exps, tables, rnum, state, query, body, initial, prepared} =>
                (EQuery {exps = exps,
                         tables = tables,
                         rnum = rnum,
                         state = state,
                         query = ae query,
                         body = ae body,
                         initial = ae initial,
                         prepared = case prepared of
                                        NONE => NONE
                                      | SOME {id, query, ...} => SOME {id = id, query = query,
                                                                       nested = IS.member (expUses globals body, id)}},
                 loc)
              | EDml {dml, prepared, mode} =>
                (EDml {dml = ae dml,
                       prepared = prepared,
                       mode = mode}, loc)

              | ENextval {seq, prepared} =>
                (ENextval {seq = ae seq,
                           prepared = prepared}, loc)
              | ESetval {seq, count} =>
                (ESetval {seq = ae seq,
                          count = ae count}, loc)

              | EUnurlify (e, t, b) => (EUnurlify (ae e, t, b), loc)
    in
        ae
    end

fun annotate (ds, syms) =
    let
        val globals =
            foldl (fn ((d, _), globals) =>
                      case d of
                          DVal (_, n, _, e) => IM.insert (globals, n, expUses globals e)
                        | DFun (_, n, _, _, e) => IM.insert (globals, n, expUses globals e)
                        | DFunRec fs =>
                          let
                              val s = foldl (fn ((_, _, _, _, e), s) => IS.union (expUses globals e, s)) IS.empty fs
                          in
                              foldl (fn ((_, n, _, _, _), globals) => IM.insert (globals, n, s)) globals fs
                          end
                        | _ => globals) IM.empty ds

        val ds =
            map (fn d as (_, loc) =>
                    case #1 d of
                        DVal (x, n, t, e) => (DVal (x, n, t, annotateExp globals e), loc)
                      | DFun (x, n, ts, t, e) => (DFun (x, n, ts, t, annotateExp globals e), loc)
                      | DFunRec fs => (DFunRec
                                           (map (fn (x, n, ts, t, e) => (x, n, ts, t, annotateExp globals e)) fs), loc)
                      | _ => d) ds
    in
        (ds, syms)
    end

end
