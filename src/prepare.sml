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

structure Prepare :> PREPARE = struct

open Cjr
open Settings

structure SM = BinaryMapFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

structure St :> sig
    type t
    val empty : t
    val nameOf : t * string -> t * int
    val list : t -> (string * int) list
    val count : t -> int
end = struct

type t = {map : int SM.map, list : (string * int) list, count : int}

val empty = {map = SM.empty, list = [], count = 0}

fun nameOf (t as {map, list, count}, s) =
    case SM.find (map, s) of
        NONE => ({map = SM.insert (map, s, count), list = (s, count) :: list, count = count + 1}, count)
      | SOME n => (t, n)

fun list (t : t) = rev (#list t)
fun count (t : t) = #count t

end

fun prepString (e, st) =
    let
        fun prepString' (e, ss, n) =
            let
                fun doOne t =
                    SOME (#p_blank (Settings.currentDbms ()) (n + 1, t) :: ss, n + 1)
            in
                case #1 e of
                    EPrim (Prim.String (_, s)) =>
                    SOME (s :: ss, n)
                  | EFfiApp ("Basis", "strcat", [(e1, _), (e2, _)]) =>
                    (case prepString' (e1, ss, n) of
                         NONE => NONE
                       | SOME (ss, n) => prepString' (e2, ss, n))
                  | EFfiApp ("Basis", "sqlifyInt", [_]) => doOne Int
                  | EFfiApp ("Basis", "sqlifyFloat", [_]) => doOne Float
                  | EFfiApp ("Basis", "sqlifyString", [_]) => doOne String
                  | EFfiApp ("Basis", "sqlifyBool", [_]) => doOne Bool
                  | EFfiApp ("Basis", "sqlifyTime", [_]) => doOne Time
                  | EFfiApp ("Basis", "sqlifyBlob", [_]) => doOne Blob
                  | EFfiApp ("Basis", "sqlifyChannel", [_]) => doOne Channel
                  | EFfiApp ("Basis", "sqlifyClient", [_]) => doOne Client

                  | ECase (e,
                           [((PNone _, _),
                             (EPrim (Prim.String (_, "NULL")), _)),
                            ((PSome (_, (PVar _, _)), _),
                             (EFfiApp (m, x, [((ERel 0, _), _)]), _))],
                           {disc = t, ...}) => prepString' ((EFfiApp (m, x, [(e, t)]), #2 e), ss, n)

                  | ECase (e,
                           [((PCon (_, PConFfi {mod = "Basis", con = "True", ...}, _), _),
                             (EPrim (Prim.String (_, "TRUE")), _)),
                            ((PCon (_, PConFfi {mod = "Basis", con = "False", ...}, _), _),
                             (EPrim (Prim.String (_, "FALSE")), _))],
                           _) => doOne Bool

                  | _ => NONE
            end
    in
        case prepString' (e, [], 0) of
            NONE => NONE
          | SOME (ss, n) =>
            let
                val s = String.concat (rev ss)
                val (st, id) = St.nameOf (st, s)
            in
                SOME (id, s, st)
            end
    end

fun prepExp (e as (_, loc), st) =
    case #1 e of
        EPrim _ => (e, st)
      | ERel _ => (e, st)
      | ENamed _ => (e, st)
      | ECon (_, _, NONE) => (e, st)
      | ECon (dk, pc, SOME e) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((ECon (dk, pc, SOME e), loc), st)
        end
      | ENone t => (e, st)
      | ESome (t, e) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((ESome (t, e), loc), st)
        end
      | EFfi _ => (e, st)
      | EFfiApp (m, x, es) =>
        let
            val (es, st) = ListUtil.foldlMap (fn ((e, t), st) =>
                                                 let
                                                     val (e, st) = prepExp (e, st)
                                                 in
                                                     ((e, t), st)
                                                 end) st es
        in
            ((EFfiApp (m, x, es), loc), st)
        end
      | EApp (e1, es) =>
        let
            val (e1, st) = prepExp (e1, st)
            val (es, st) = ListUtil.foldlMap prepExp st es
        in
            ((EApp (e1, es), loc), st)
        end

      | EUnop (s, e1) =>
        let
            val (e1, st) = prepExp (e1, st)
        in
            ((EUnop (s, e1), loc), st)
        end
      | EBinop (s, e1, e2) =>
        let
            val (e1, st) = prepExp (e1, st)
            val (e2, st) = prepExp (e2, st)
        in
            ((EBinop (s, e1, e2), loc), st)
        end

      | ERecord (rn, xes) =>
        let
            val (xes, st) = ListUtil.foldlMap (fn ((x, e), st) =>
                                                   let
                                                       val (e, st) = prepExp (e, st)
                                                   in
                                                       ((x, e), st)
                                                   end) st xes
        in
            ((ERecord (rn, xes), loc), st)
        end
      | EField (e, s) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((EField (e, s), loc), st)
        end

      | ECase (e, pes, ts) =>
        let
            val (e, st) = prepExp (e, st)
            val (pes, st) = ListUtil.foldlMap (fn ((p, e), st) =>
                                                   let
                                                       val (e, st) = prepExp (e, st)
                                                   in
                                                       ((p, e), st)
                                                   end) st pes
        in
            ((ECase (e, pes, ts), loc), st)
        end

      | EError (e, t) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((EError (e, t), loc), st)
        end

      | EReturnBlob {blob, mimeType, t} =>
        let
            val (blob, st) = case blob of
                                 NONE => (blob, st)
                               | SOME blob =>
                                 let
                                     val (b, st) = prepExp (blob, st)
                                 in
                                     (SOME b, st)
                                 end
            val (mimeType, st) = prepExp (mimeType, st)
        in
            ((EReturnBlob {blob = blob, mimeType = mimeType, t = t}, loc), st)
        end

      | ERedirect (e, t) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((ERedirect (e, t), loc), st)
        end

      | EWrite e =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((EWrite e, loc), st)
        end
      | ESeq (e1, e2) =>
        let
            val (e1, st) = prepExp (e1, st)
            val (e2, st) = prepExp (e2, st)
        in
            ((ESeq (e1, e2), loc), st)
        end
      | ELet (x, t, e1, e2) =>
        let
            val (e1, st) = prepExp (e1, st)
            val (e2, st) = prepExp (e2, st)
        in
            ((ELet (x, t, e1, e2), loc), st)
        end

      | EQuery {exps, tables, rnum, state, query, body, initial, ...} =>
        let
            val (body, st) = prepExp (body, st)
        in
            case prepString (query, st) of
                NONE =>
                ((EQuery {exps = exps, tables = tables, rnum = rnum,
                          state = state, query = query, body = body,
                          initial = initial, prepared = NONE}, loc),
                 st)
              | SOME (id, s, st) =>
                ((EQuery {exps = exps, tables = tables, rnum = rnum,
                          state = state, query = query, body = body,
                          initial = initial, prepared = SOME {id = id, query = s, nested = true}}, loc), st)
        end

      | EDml {dml, mode, ...} =>
        (case prepString (dml, st) of
             NONE => (e, st)
           | SOME (id, s, st) =>
             ((EDml {dml = dml, prepared = SOME {id = id, dml = s}, mode = mode}, loc), st))

      | ENextval {seq, ...} =>
        if #supportsNextval (Settings.currentDbms ()) then
            let
                val s = case seq of
                            (EPrim (Prim.String (_, s)), loc) =>
                            (EPrim (Prim.String (Prim.Normal, "SELECT NEXTVAL('" ^ s ^ "')")), loc)
                          | _ =>
                            let
                                val t = (TFfi ("Basis", "string"), loc)
                                val s' = (EFfiApp ("Basis", "strcat", [(seq, t), ((EPrim (Prim.String (Prim.Normal, "')")), loc), t)]), loc)
                            in
                                (EFfiApp ("Basis", "strcat", [((EPrim (Prim.String (Prim.Normal, "SELECT NEXTVAL('")), loc), t), (s', t)]), loc)
                            end
            in
                case prepString (s, st) of
                    NONE => (e, st)
                  | SOME (id, s, st) =>
                    ((ENextval {seq = seq, prepared = SOME {id = id, query = s}}, loc), st)
            end
        else
            (e, st)

      | ESetval {seq = e1, count = e2} =>
        let
            val (e1, st) = prepExp (e1, st)
            val (e2, st) = prepExp (e2, st)
        in
            ((ESetval {seq = e1, count = e2}, loc), st)
        end

      | EUnurlify (e, t, b) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((EUnurlify (e, t, b), loc), st)
        end

fun prepDecl (d as (_, loc), st) =
    case #1 d of
        DStruct _ => (d, st)
      | DDatatype _ => (d, st)
      | DDatatypeForward _ => (d, st)
      | DVal (x, n, t, e) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((DVal (x, n, t, e), loc), st)
        end
      | DFun (x, n, xts, t, e) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((DFun (x, n, xts, t, e), loc), st)
        end
      | DFunRec fs =>
        let
            val (fs, st) = ListUtil.foldlMap (fn ((x, n, xts, t, e), st) =>
                                                  let
                                                      val (e, st) = prepExp (e, st)
                                                  in
                                                      ((x, n, xts, t, e), st)
                                                  end) st fs
        in
            ((DFunRec fs, loc), st)
        end

      | DTable _ => (d, st)
      | DSequence _ => (d, st)
      | DView _ => (d, st)
      | DIndex _ => (d, st)
      | DDatabase _ => (d, st)
      | DPreparedStatements _ => (d, st)
      | DJavaScript _ => (d, st)
      | DCookie _ => (d, st)
      | DStyle _ => (d, st)
      | DTask (tk, x1, x2, e) =>
        let
            val (e, st) = prepExp (e, st)
        in
            ((DTask (tk, x1, x2, e), loc), st)
        end
      | DOnError _ => (d, st)

fun prepare (ds, ps) =
    let
        val (ds, st) = ListUtil.foldlMap prepDecl St.empty ds
    in
        ((DPreparedStatements (St.list st), ErrorMsg.dummySpan) :: ds, ps)
    end

end
