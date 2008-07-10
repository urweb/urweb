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

structure Cjrize :> CJRIZE = struct

structure L = Flat
structure L' = Cjr

structure Sm :> sig
    type t

    val empty : t
    val find : t * (string * L.typ) list * (string * L'.typ) list -> t * int

    val declares : t -> (int * (string * L'.typ) list) list
end = struct

structure FM = BinaryMapFn(struct
                           type ord_key = L.typ
                           val compare = FlatUtil.Typ.compare
                           end)

type t = int * int FM.map * (int * (string * L'.typ) list) list

val empty : t = (1, FM.insert (FM.empty, (L.TRecord [], ErrorMsg.dummySpan), 0), [])

fun find ((n, m, ds), xts, xts') =
    let
        val t = (L.TRecord xts, ErrorMsg.dummySpan)
    in
        case FM.find (m, t) of
            NONE => ((n+1, FM.insert (m, t, n), (n, xts') :: ds), n)
          | SOME i => ((n, m, ds), i)
    end

fun declares (_, _, ds) = ds

end

fun cifyTyp ((t, loc), sm) =
    case t of
        L.TTop => ((L'.TTop, loc), sm)
      | L.TFun (t1, t2) =>
        let
            val (_, sm) = cifyTyp (t1, sm)
            val (_, sm) = cifyTyp (t2, sm)
        in
            ((L'.TFun, loc), sm)
        end
      | L.TCode (t1, t2) =>
        let
            val (t1, sm) = cifyTyp (t1, sm)
            val (t2, sm) = cifyTyp (t2, sm)
        in
            ((L'.TCode (t1, t2), loc), sm)
        end
      | L.TRecord xts =>
        let
            val old_xts = xts
            val (xts, sm) = ListUtil.foldlMap (fn ((x, t), sm) =>
                                                  let
                                                      val (t, sm) = cifyTyp (t, sm)
                                                  in
                                                      ((x, t), sm)
                                                  end)
                                              sm xts
            val (sm, si) = Sm.find (sm, old_xts, xts)
        in
            ((L'.TRecord si, loc), sm)
        end
      | L.TNamed n => ((L'.TNamed n, loc), sm)
      | L.TFfi mx => ((L'.TFfi mx, loc), sm)

fun cifyExp ((e, loc), sm) =
    case e of
        L.EPrim p => ((L'.EPrim p, loc), sm)
      | L.ERel n => ((L'.ERel n, loc), sm)
      | L.ENamed n => ((L'.ENamed n, loc), sm)
      | L.EFfi mx => ((L'.EFfi mx, loc), sm)
      | L.EFfiApp (m, x, es) =>
        let
            val (es, sm) = ListUtil.foldlMap cifyExp sm es
        in
            ((L'.EFfiApp (m, x, es), loc), sm)
        end
      | L.ECode n => ((L'.ECode n, loc), sm)
      | L.EApp (e1, e2) =>
        let
            val (e1, sm) = cifyExp (e1, sm)
            val (e2, sm) = cifyExp (e2, sm)
        in
            ((L'.EApp (e1, e2), loc), sm)
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

      | L.ELet (xes, e) =>
        let
            val (xes, sm) = ListUtil.foldlMap (fn ((x, t, e), sm) =>
                                                  let
                                                      val (t, sm) = cifyTyp (t, sm)
                                                      val (e, sm) = cifyExp (e, sm)
                                                  in
                                                      ((x, t, e), sm)
                                                  end)
                            sm xes
            val (e, sm) = cifyExp (e, sm)
        in
            ((L'.ELet (xes, e), loc), sm)
        end

      | L.EStrcat _ => raise Fail "Cjrize EStrcat"

      | L.EWrite e =>
        let
            val (e, sm) = cifyExp (e, sm)
        in
            ((L'.EWrite e, loc), sm)
        end

fun cifyDecl ((d, loc), sm) =
    case d of
        L.DVal (x, n, t, e) =>
        let
            val (t, sm) = cifyTyp (t, sm)
            val (e, sm) = cifyExp (e, sm)
        in
            (SOME (L'.DVal (x, n, t, e), loc), NONE, sm)
        end
      | L.DFun (n, x, dom, ran, e) =>
        let
            val (dom, sm) = cifyTyp (dom, sm)
            val (ran, sm) = cifyTyp (ran, sm)
            val (e, sm) = cifyExp (e, sm)
        in
            (SOME (L'.DFun (n, x, dom, ran, e), loc), NONE, sm)
        end
      | L.DPage (xts, e) =>
        let
            val (xts, sm) = ListUtil.foldlMap (fn ((x, t), sm) =>
                                                  let
                                                      val (t, sm) = cifyTyp (t, sm)
                                                  in
                                                      ((x, t), sm)
                                                  end)
                                              sm xts
            val (e, sm) = cifyExp (e, sm)
        in
            (NONE, SOME (xts, e), sm)
        end

fun cjrize ds =
    let
        val (ds, ps, sm) = foldl (fn (d, (ds, ps, sm)) =>
                                     let
                                         val (dop, pop, sm) = cifyDecl (d, sm)
                                         val ds = case dop of
                                                      NONE => ds
                                                    | SOME d => d :: ds
                                         val ps = case pop of
                                                      NONE => ps
                                                    | SOME p => p :: ps 
                                     in
                                         (ds, ps, sm)
                                     end)
                           ([], [], Sm.empty) ds
    in
        (List.revAppend (map (fn v => (L'.DStruct v, ErrorMsg.dummySpan)) (Sm.declares sm),
                         rev ds),
         ps)
    end

end
