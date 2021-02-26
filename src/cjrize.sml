(* Copyright (c) 2008-2010, Adam Chlipala
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

structure Cjrize :> CJRIZE = struct

structure L = Mono
structure L' = Cjr

structure IM = IntBinaryMap

structure Sm :> sig
    type t

    val empty : t
    val find : t * (string * L.typ) list * (string * L'.typ) list -> t * int
    val findList : t * L.typ * L'.typ -> t * int

    val declares : t -> (int * (string * L'.typ) list) list
    val clearDeclares : t -> t
end = struct

structure FM = BinaryMapFn(struct
                           type ord_key = L.typ
                           val compare = MonoUtil.Typ.compare
                           end)

type t = {
     count : int,
     normal : int FM.map,
     lists : int FM.map,
     decls : (int * (string * L'.typ) list) list
}

val empty : t = {
    count = 1,
    normal = FM.insert (FM.empty, (L.TRecord [], ErrorMsg.dummySpan), 0),
    lists = FM.empty,
    decls = []
}

fun find (v as {count, normal, decls, lists}, xts, xts') =
    let
        val t = (L.TRecord xts, ErrorMsg.dummySpan)
    in
        case FM.find (normal, t) of
            SOME i => (v, i)
          | NONE => ({count = count+1,
                      normal = FM.insert (normal, t, count),
                      lists = lists,
                      decls = (count, xts') :: decls},
                     count)
    end

fun findList (v as {count, normal, decls, lists}, t, t') =
    case FM.find (lists, t) of
        SOME i => (v, i)
      | NONE =>
        let
            val xts = [("1", t), ("2", (L.TList t, #2 t))]
            val xts' = [("1", t'), ("2", (L'.TList (t', count), #2 t'))]
        in
            ({count = count+1,
              normal = FM.insert (normal, (L.TRecord xts, ErrorMsg.dummySpan), count),
              lists = FM.insert (lists, t, count),
              decls = (count, xts') :: decls},
             count)
    end

fun declares (v : t) = #decls v

fun clearDeclares (v : t) = {count = #count v,
                             normal = #normal v,
                             lists = #lists v,
                             decls = []}

end

fun cifyTyp x =
    let
        fun cify dtmap ((t, loc), sm) =
            case t of
                L.TFun (t1, t2) =>
                let
                    val (t1, sm) = cify dtmap (t1, sm)
                    val (t2, sm) = cify dtmap (t2, sm)
                in
                    ((L'.TFun (t1, t2), loc), sm)
                end
              | L.TRecord xts =>
                let
                    val xts = MonoUtil.Typ.sortFields xts
                    val old_xts = xts
                    val (xts, sm) = ListUtil.foldlMap (fn ((x, t), sm) =>
                                                          let
                                                              val (t, sm) = cify dtmap (t, sm)
                                                          in
                                                              ((x, t), sm)
                                                          end)
                                                      sm xts
                    val (sm, si) = Sm.find (sm, old_xts, xts)
                in
                    ((L'.TRecord si, loc), sm)
                end
              | L.TDatatype (n, ref (dk, xncs)) =>
                (case IM.find (dtmap, n) of
                     SOME r => ((L'.TDatatype (dk, n, r), loc), sm)
                   | NONE =>
                     let
                         val r = ref []
                         val dtmap = IM.insert (dtmap, n, r)

                         val (xncs, sm) = ListUtil.foldlMap (fn ((x, n, to), sm) =>
                                                                case to of
                                                                    NONE => ((x, n, NONE), sm)
                                                                  | SOME t =>
                                                                    let
                                                                        val (t, sm) = cify dtmap (t, sm)
                                                                    in
                                                                        ((x, n, SOME t), sm)
                                                                    end)
                                                            sm xncs
                     in
                         r := xncs;
                         ((L'.TDatatype (dk, n, r), loc), sm)
                     end)
              | L.TFfi mx => ((L'.TFfi mx, loc), sm)
              | L.TOption t =>
                let
                    val (t, sm) = cify dtmap (t, sm)
                in
                    ((L'.TOption t, loc), sm)
                end
              | L.TList t =>
                let
                    val (t', sm) = cify dtmap (t, sm)
                    val (sm, si) = Sm.findList (sm, t, t')
                in
                    ((L'.TList (t', si), loc), sm)
                end
              | L.TSource => ((L'.TFfi ("Basis", "source"), loc), sm)
              | L.TSignal _ => (ErrorMsg.errorAt loc "TSignal remains";
                                Print.epreface ("Full type", MonoPrint.p_typ MonoEnv.empty (#1 x));
                                ((L'.TFfi ("Basis", "bogus"), loc), sm))
    in
        cify IM.empty x
    end

val dummye = (L'.EPrim (Prim.Int 0), ErrorMsg.dummySpan)

fun cifyPatCon (pc, sm) =
    case pc of
        L.PConVar n => (L'.PConVar n, sm)
      | L.PConFfi {mod = m, datatyp, con, arg} =>
        let
            val (arg, sm) =
                case arg of
                    NONE => (NONE, sm)
                  | SOME t =>
                    let
                        val (t, sm) = cifyTyp (t, sm)
                    in
                        (SOME t, sm)
                    end
        in
            (L'.PConFfi {mod = m, datatyp = datatyp, con = con, arg = arg}, sm)
        end

fun cifyPat ((p, loc), sm) =
    case p of
        L.PVar (x, t) =>
        let
            val (t, sm) = cifyTyp (t, sm)
        in
            ((L'.PVar (x, t), loc), sm)
        end
      | L.PPrim p => ((L'.PPrim p, loc), sm)
      | L.PCon (dk, pc, NONE) =>
        let
            val (pc, sm) = cifyPatCon (pc, sm)
        in
            ((L'.PCon (dk, pc, NONE), loc), sm)
        end
      | L.PCon (dk, pc, SOME p) =>
        let
            val (pc, sm) = cifyPatCon (pc, sm)
            val (p, sm) = cifyPat (p, sm)
        in
            ((L'.PCon (dk, pc, SOME p), loc), sm)
        end
      | L.PRecord xps =>
        let
            val (xps, sm) = ListUtil.foldlMap (fn ((x, p, t), sm) =>
                                                  let
                                                      val (p, sm) = cifyPat (p, sm)
                                                      val (t, sm) = cifyTyp (t, sm)
                                                  in
                                                      ((x, p, t), sm)
                                                  end) sm xps
        in
            ((L'.PRecord xps, loc), sm)
        end
      | L.PNone t =>
        let
            val (t, sm) = cifyTyp (t, sm)
        in
            ((L'.PNone t, loc), sm)
        end
      | L.PSome (t, p) =>
        let
            val (t, sm) = cifyTyp (t, sm)
            val (p, sm) = cifyPat (p, sm)
        in
            ((L'.PSome (t, p), loc), sm)
        end

fun cifyExp (eAll as (e, loc), sm) =
    let
        fun fail msg =
            (ErrorMsg.errorAt loc msg;
             ((L'.EPrim (Prim.String (Prim.Normal, "")), loc), sm))
    in
        case e of
            L.EPrim p => ((L'.EPrim p, loc), sm)
          | L.ERel n => ((L'.ERel n, loc), sm)
          | L.ENamed n => ((L'.ENamed n, loc), sm)
          | L.ECon (dk, pc, eo) =>
            let
                val (eo, sm) =
                    case eo of
                        NONE => (NONE, sm)
                      | SOME e =>
                        let
                            val (e, sm) = cifyExp (e, sm)
                        in
                            (SOME e, sm)
                        end
                val (pc, sm) = cifyPatCon (pc, sm)
            in
                ((L'.ECon (dk, pc, eo), loc), sm)
            end
          | L.ENone t =>
            let
                val (t, sm) = cifyTyp (t, sm)
            in
                ((L'.ENone t, loc), sm)
            end
          | L.ESome (t, e) =>
            let
                val (t, sm) = cifyTyp (t, sm)
                val (e, sm) = cifyExp (e, sm)
            in
                ((L'.ESome (t, e), loc), sm)
            end
          | L.EFfi mx => ((L'.EFfi mx, loc), sm)
          | L.EFfiApp (m, x, es) =>
            let
                val (es, sm) = ListUtil.foldlMap (fn ((e, t), sm) =>
                                                     let
                                                         val (t, sm) = cifyTyp (t, sm)
                                                         val (e, sm) = cifyExp (e, sm)
                                                     in
                                                         ((e, t), sm)
                                                     end) sm es
            in
                ((L'.EFfiApp (m, x, es), loc), sm)
            end
          | L.EApp (e1, e2) =>
            let
                fun unravel (e, args) =
                    case e of
                        (L.EApp (e1, e2), _) => unravel (e1, e2 :: args)
                      | _ => (e, args)

                val (f, es) = unravel (e1, [e2])

                val (f, sm) = cifyExp (f, sm)
                val (es, sm) = ListUtil.foldlMap cifyExp sm es
            in
                ((L'.EApp (f, es), loc), sm)
            end
          | L.EAbs _ => (ErrorMsg.errorAt loc "Anonymous function remains at code generation";
                         Print.prefaces' [("Function", MonoPrint.p_exp MonoEnv.empty eAll)];
                         (dummye, sm))

          | L.EUnop (s, e1) =>
            let
                val (e1, sm) = cifyExp (e1, sm)
            in
                ((L'.EUnop (s, e1), loc), sm)
            end
          | L.EBinop (_, s, e1, e2) =>
            let
                val (e1, sm) = cifyExp (e1, sm)
                val (e2, sm) = cifyExp (e2, sm)
            in
                ((L'.EBinop (s, e1, e2), loc), sm)
            end

          | L.ERecord xes =>
            let
                val old_xts = map (fn (x, _, t) => (x, t)) xes

                val (xets, sm) = ListUtil.foldlMap (fn ((x, e, t), sm) =>
                                                       let
                                                           val (e, sm) = cifyExp (e, sm)
                                                           val (t, sm) = cifyTyp (t, sm)
                                                       in
                                                           ((x, e, t), sm)
                                                       end)
                                                   sm xes

                val (sm, si) = Sm.find (sm, old_xts, map (fn (x, _, t) => (x, t)) xets)

                val xes = map (fn (x, e, _) => (x, e)) xets
                val xes = ListMergeSort.sort (fn ((x1, _), (x2, _)) => String.compare (x1, x2) = GREATER) xes
            in
                ((L'.ERecord (si, xes), loc), sm)
            end
          | L.EField (e, x) =>
            let
                val (e, sm) = cifyExp (e, sm)
            in
                ((L'.EField (e, x), loc), sm)
            end

          | L.ECase (e, pes, {disc, result}) =>
            let
                val (e, sm) = cifyExp (e, sm)
                val (pes, sm) = ListUtil.foldlMap
                                    (fn ((p, e), sm) =>
                                        let
                                            val (e, sm) = cifyExp (e, sm)
                                            val (p, sm) = cifyPat (p, sm)
                                        in
                                            ((p, e), sm)
                                        end) sm pes
                val (disc, sm) = cifyTyp (disc, sm)
                val (result, sm) = cifyTyp (result, sm)
            in
                ((L'.ECase (e, pes, {disc = disc, result = result}), loc), sm)
            end

          | L.EError (e, t) =>
            let
                val (e, sm) = cifyExp (e, sm)
                val (t, sm) = cifyTyp (t, sm)
            in
                ((L'.EError (e, t), loc), sm)
            end
          | L.EReturnBlob {blob = NONE, mimeType, t} =>
            let
                val (mimeType, sm) = cifyExp (mimeType, sm)
                val (t, sm) = cifyTyp (t, sm)
            in
                ((L'.EReturnBlob {blob = NONE, mimeType = mimeType, t = t}, loc), sm)
            end
          | L.EReturnBlob {blob = SOME blob, mimeType, t} =>
            let
                val (blob, sm) = cifyExp (blob, sm)
                val (mimeType, sm) = cifyExp (mimeType, sm)
                val (t, sm) = cifyTyp (t, sm)
            in
                ((L'.EReturnBlob {blob = SOME blob, mimeType = mimeType, t = t}, loc), sm)
            end
          | L.ERedirect (e, t) =>
            let
                val (e, sm) = cifyExp (e, sm)
                val (t, sm) = cifyTyp (t, sm)
            in
                ((L'.ERedirect (e, t), loc), sm)
            end

          | L.EStrcat (e1, e2) =>
            let
                val (e1, sm) = cifyExp (e1, sm)
                val (e2, sm) = cifyExp (e2, sm)
                val s = (L'.TFfi ("Basis", "string"), loc)
            in
                ((L'.EFfiApp ("Basis", "strcat", [(e1, s), (e2, s)]), loc), sm)
            end

          | L.EWrite e =>
            let
                val (e, sm) = cifyExp (e, sm)
            in
                ((L'.EWrite e, loc), sm)
            end

          | L.ESeq (e1, e2) =>
            let
                val (e1, sm) = cifyExp (e1, sm)
                val (e2, sm) = cifyExp (e2, sm)
            in
                ((L'.ESeq (e1, e2), loc), sm)
            end

          | L.ELet (x, t, e1, e2) =>
            let
                val (t, sm) = cifyTyp (t, sm)
                val (e1, sm) = cifyExp (e1, sm)
                val (e2, sm) = cifyExp (e2, sm)
            in
                ((L'.ELet (x, t, e1, e2), loc), sm)
            end

          | L.EClosure _ => (ErrorMsg.errorAt loc "Nested closure remains in code generation";
                             (dummye, sm))

          | L.EQuery {exps, tables, state, query, body, initial} =>
            let
                val (exps', sm) = ListUtil.foldlMap (fn ((x, t), sm) =>
                                                        let
                                                            val (t, sm) = cifyTyp (t, sm)
                                                        in
                                                            ((x, t), sm)
                                                        end) sm exps
                val (tables', sm) = ListUtil.foldlMap (fn ((x, xts), sm) =>
                                                          let
                                                              val (xts, sm) = ListUtil.foldlMap
                                                                                  (fn ((x, t), sm) =>
                                                                                      let
                                                                                          val (t, sm) = cifyTyp (t, sm)
                                                                                      in
                                                                                          ((x, t), sm)
                                                                                      end) sm xts
                                                          in
                                                              ((x, xts), sm)
                                                          end) sm tables

                val row = exps @ map (fn (x, xts) => (x, (L.TRecord xts, loc))) tables
                val row = ListMergeSort.sort (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) row

                val (tableRows, sm) = ListUtil.foldlMap (fn (((x, xts), (_, xts')), sm) =>
                                                            let
                                                                val (sm, rnum) = Sm.find (sm, xts, xts')
                                                            in
                                                                ((x, rnum), sm)
                                                            end)
                                                        sm (ListPair.zip (tables, tables'))
                val row' = exps' @ map (fn (x, n) => (x, (L'.TRecord n, loc))) tableRows
                val row' = ListMergeSort.sort (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER) row'

                val (sm, rnum) = Sm.find (sm, row, row')

                val (state, sm) = cifyTyp (state, sm)
                val (query, sm) = cifyExp (query, sm)
                val (body, sm) = cifyExp (body, sm)
                val (initial, sm) = cifyExp (initial, sm)
            in
                ((L'.EQuery {exps = exps', tables = tables', rnum = rnum, state = state,
                             query = query, body = body, initial = initial, prepared = NONE}, loc), sm)
            end

          | L.EDml (e, mode) =>
            let
                val (e, sm) = cifyExp (e, sm)
            in
                ((L'.EDml {dml = e, prepared = NONE, mode = mode}, loc), sm)
            end

          | L.ENextval e =>
            let
                val (e, sm) = cifyExp (e, sm)
            in
                ((L'.ENextval {seq = e, prepared = NONE}, loc), sm)
            end
          | L.ESetval (e1, e2) =>
            let
                val (e1, sm) = cifyExp (e1, sm)
                val (e2, sm) = cifyExp (e2, sm)
            in
                ((L'.ESetval {seq = e1, count = e2}, loc), sm)
            end

          | L.EUnurlify (e, t, b) =>
            let
                val (e, sm) = cifyExp (e, sm)
                val (t, sm) = cifyTyp (t, sm)
            in
                ((L'.EUnurlify (e, t, b), loc), sm)
            end

          | L.EJavaScript _ => fail "Uncompilable JavaScript remains"

          | L.ESignalReturn _ => fail "Signal monad 'return' remains in server-side code"
          | L.ESignalBind _ => fail "Signal monad 'bind' remains in server-side code"
          | L.ESignalSource _ => fail "Signal monad 'source' remains in server-side code"

          | L.EServerCall _ => fail "RPC in server-side code"
          | L.ERecv _ => fail "Message receive in server-side code"
          | L.ESleep _ => fail "Sleep in server-side code"
          | L.ESpawn _ => fail "Thread spawn in server-side code"
    end

fun cifyDecl ((d, loc), sm) =
    case d of
        L.DDatatype dts =>
        let
            val (dts, sm) = ListUtil.foldlMap
                                (fn ((x, n, xncs), sm) =>
                                    let
                                        val dk = ElabUtil.classifyDatatype xncs
                                        val (xncs, sm) = ListUtil.foldlMap (fn ((x, n, to), sm) =>
                                                                               case to of
                                                                                   NONE => ((x, n, NONE), sm)
                                                                                 | SOME t =>
                                                                                   let
                                                                                       val (t, sm) = cifyTyp (t, sm)
                                                                                   in
                                                                                       ((x, n, SOME t), sm)
                                                                                   end) sm xncs
                                    in
                                        ((dk, x, n, xncs), sm)
                                    end)
                                sm dts
        in
            (SOME (L'.DDatatype dts, loc), NONE, sm)
        end

      | L.DVal (x, n, t, e, _) =>
        let
            val (t, sm) = cifyTyp (t, sm)

            val (d, sm) = case #1 t of
                              L'.TFun _ =>
                              let
                                  fun unravel (tAll as (t, _), eAll as (e, _)) =
                                      case (t, e) of
                                          (L'.TFun (dom, ran), L.EAbs (ax, _, _, e)) =>
                                          let
                                              val (args, t, e) = unravel (ran, e)
                                          in
                                              ((ax, dom) :: args, t, e)
                                          end
                                        | (L'.TFun (dom, ran), _) =>
                                          let
                                              val e = MonoEnv.liftExpInExp 0 eAll
                                              val e = (L.EApp (e, (L.ERel 0, loc)), loc)
                                              val (args, t, e) = unravel (ran, e)
                                          in
                                              (("x", dom) :: args, t, e)
                                          end
                                        | _ => ([], tAll, eAll)

                                  val (args, ran, e) = unravel (t, e)
                                  val (e, sm) = cifyExp (e, sm)
                              in
                                  (L'.DFun (x, n, args, ran, e), sm)
                              end

                            | _ =>
                              let
                                  val (e, sm) = cifyExp (e, sm)
                              in
                                  (L'.DVal (x, n, t, e), sm)
                              end
        in
            (SOME (d, loc), NONE, sm)
        end
      | L.DValRec vis =>
        let
            val (vis, sm) = ListUtil.foldlMap
                            (fn ((x, n, t, e, _), sm) =>
                                let
                                    val (t, sm) = cifyTyp (t, sm)

                                    fun unravel (tAll as (t, _), eAll as (e, _)) =
                                        case (t, e) of
                                            (L'.TFun (dom, ran), L.EAbs (ax, _, _, e)) =>
                                            let
                                                val (args, t, e) = unravel (ran, e)
                                            in
                                                ((ax, dom) :: args, t, e)
                                            end
                                          | (L'.TFun _, _) =>
                                            (ErrorMsg.errorAt loc "Function isn't explicit at code generation";
                                             ([], tAll, eAll))
                                          | _ => ([], tAll, eAll)

                                    val (args, ran, e) = unravel (t, e)
                                    val (e, sm) = cifyExp (e, sm)
                              in
                                  ((x, n, args, ran, e), sm)
                              end)
                            sm vis
        in
            (SOME (L'.DFunRec vis, loc), NONE, sm)
        end

      | L.DExport (ek, s, n, ts, t, b) =>
        let
            val (ts, sm) = ListUtil.foldlMap cifyTyp sm ts
            val (t, sm) = cifyTyp (t, sm)
        in
            (NONE, SOME (ek, "/" ^ s, n, ts, t, L'.ServerAndPullAndPush, b), sm)
        end

      | L.DTable (s, xts, pe, ce) =>
        let
            val (xts, sm) = ListUtil.foldlMap (fn ((x, t), sm) =>
                                                  let
                                                      val (t, sm) = cifyTyp (t, sm)
                                                  in
                                                      ((x, t), sm)
                                                  end) sm xts

            fun flatten e =
                case #1 e of
                    L.ERecord [] => []
                  | L.ERecord [(x, (L.EPrim (Prim.String (_, v)), _), _)] => [(x, v)]
                  | L.EStrcat (e1, e2) => flatten e1 @ flatten e2
                  | _ => (ErrorMsg.errorAt loc "Constraint has not been fully determined";
                          Print.prefaces "Undetermined constraint"
                                         [("e", MonoPrint.p_exp MonoEnv.empty e)];
                          [])

            val pe = case #1 pe of
                         L.EPrim (Prim.String (_, s)) => s
                       | _ => (ErrorMsg.errorAt loc "Primary key has not been fully determined";
                               Print.prefaces "Undetermined constraint"
                                              [("e", MonoPrint.p_exp MonoEnv.empty pe)];
                               "")
        in
            (SOME (L'.DTable (s, xts, pe, flatten ce), loc), NONE, sm)
        end
      | L.DSequence s =>
        (SOME (L'.DSequence s, loc), NONE, sm)
      | L.DView (s, xts, e) =>
        let
            val (xts, sm) = ListUtil.foldlMap (fn ((x, t), sm) =>
                                                  let
                                                      val (t, sm) = cifyTyp (t, sm)
                                                  in
                                                      ((x, t), sm)
                                                  end) sm xts

            fun flatten e =
                case #1 e of
                    L.ERecord [] => []
                  | L.ERecord [(x, (L.EPrim (Prim.String (_, v)), _), _)] => [(x, v)]
                  | L.EStrcat (e1, e2) => flatten e1 @ flatten e2
                  | _ => (ErrorMsg.errorAt loc "Constraint has not been fully determined";
                          Print.prefaces "Undetermined constraint"
                                         [("e", MonoPrint.p_exp MonoEnv.empty e)];
                          [])

            val e = case #1 e of
                        L.EPrim (Prim.String (_, s)) => s
                      | _ => (ErrorMsg.errorAt loc "VIEW query has not been fully determined";
                              Print.prefaces "Undetermined VIEW query"
                                             [("e", MonoPrint.p_exp MonoEnv.empty e)];
                              "")
        in
            (SOME (L'.DView (s, xts, e), loc), NONE, sm)
        end
      | L.DIndex vs => (SOME (L'.DIndex vs, loc), NONE, sm)
      | L.DDatabase s => (SOME (L'.DDatabase s, loc), NONE, sm)
      | L.DJavaScript s => (SOME (L'.DJavaScript s, loc), NONE, sm)
      | L.DCookie args => (SOME (L'.DCookie args, loc), NONE, sm)
      | L.DStyle args => (SOME (L'.DStyle args, loc), NONE, sm)
      | L.DTask (e1, e2) =>
        (case #1 e2 of
             L.EAbs (x1, _, _, (L.EAbs (x2, _, _, e), _)) =>
             let
                 val tk = case #1 e1 of
                              L.EFfi ("Basis", "initialize") => L'.Initialize
                            | L.EFfi ("Basis", "clientLeaves") => L'.ClientLeaves
                            | L.EFfiApp ("Basis", "periodic", [((L.EPrim (Prim.Int n), _), _)]) => L'.Periodic n
                            | _ => (ErrorMsg.errorAt loc "Task kind not fully determined";
                                    L'.Initialize)
                 val (e, sm) = cifyExp (e, sm)
             in
                 (SOME (L'.DTask (tk, x1, x2, e), loc), NONE, sm)
             end
           | _ => (ErrorMsg.errorAt loc "Initializer has not been fully determined";
                   (NONE, NONE, sm)))
      | L.DPolicy _ => (NONE, NONE, sm)
      | L.DOnError n => (SOME (L'.DOnError n, loc), NONE, sm)

fun cjrize (ds, sideInfo) =
    let
        val (dsF, ds, ps, sm) = foldl (fn (d, (dsF, ds, ps, sm)) =>
                                          let
                                              val (dop, pop, sm) = cifyDecl (d, sm)

                                              val dsF = case dop of
                                                            SOME (L'.DDatatype dts, loc) =>
                                                            map (fn (dk, x, n, _) =>
                                                                    (L'.DDatatypeForward (dk, x, n), loc)) dts @ dsF
                                                          | _ => dsF

                                              val dsF = map (fn v => (L'.DStruct v, ErrorMsg.dummySpan)) (Sm.declares sm)
                                                        @ dsF

                                              val (dsF, ds) = case dop of
                                                                  NONE => (dsF, ds)
                                                                | SOME (d as (L'.DDatatype _, loc)) =>
                                                                  (d :: dsF, ds)
                                                                | SOME d => (dsF, d :: ds)

                                              val ps = case pop of
                                                           NONE => ps
                                                         | SOME p => p :: ps
                                          in
                                              (dsF, ds, ps, Sm.clearDeclares sm)
                                          end)
                                      ([], [], [], Sm.empty) ds

        val sideInfo = foldl (fn ((n, mode, dbmode), mp) => IM.insert (mp, n, (mode, dbmode))) IM.empty sideInfo

        val ps = map (fn (ek, s, n, ts, t, _, b) =>
                         let
                             val (side, db) = getOpt (IM.find (sideInfo, n), (L'.ServerOnly, L'.AnyDb))
                         in
                             (ek, s, n, ts, t, side, db, b)
                         end) ps
    in
        (List.revAppend (dsF, rev ds),
         ps)
    end

end
