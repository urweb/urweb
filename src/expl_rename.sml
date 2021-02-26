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

structure ExplRename :> EXPL_RENAME = struct

open Expl
structure E = ExplEnv

structure IM = IntBinaryMap

structure St :> sig
    type t

    val create : int -> t
    val next : t -> int

    val bind : t * int -> t * int
    val lookup: t * int -> int option
end = struct

type t = {next : int,
          renaming : int IM.map}

fun create next = {next = next,
                   renaming = IM.empty}

fun next (t : t) = #next t

fun bind ({next, renaming}, n) =
    ({next = next + 1,
      renaming = IM.insert (renaming, n, next)}, next)

fun lookup ({next, renaming}, n) =
    IM.find (renaming, n)

end

fun renameCon st (all as (c, loc)) =
    case c of
        TFun (c1, c2) => (TFun (renameCon st c1, renameCon st c2), loc)
      | TCFun (x, k, c) => (TCFun (x, k, renameCon st c), loc)
      | TRecord c => (TRecord (renameCon st c), loc)
      | CRel _ => all
      | CNamed n =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (CNamed n', loc))
      | CModProj (n, ms, x) =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (CModProj (n', ms, x), loc))
      | CApp (c1, c2) => (CApp (renameCon st c1, renameCon st c2), loc)
      | CAbs (x, k, c) => (CAbs (x, k, renameCon st c), loc)
      | CKAbs (x, c) => (CKAbs (x, renameCon st c), loc)
      | CKApp (c, k) => (CKApp (renameCon st c, k), loc)
      | TKFun (x, c) => (TKFun (x, renameCon st c), loc)
      | CName _ => all
      | CRecord (k, xcs) => (CRecord (k, map (fn (x, c) => (renameCon st x, renameCon st c)) xcs), loc)
      | CConcat (c1, c2) => (CConcat (renameCon st c1, renameCon st c2), loc)
      | CMap _ => all
      | CUnit => all
      | CTuple cs => (CTuple (map (renameCon st) cs), loc)
      | CProj (c, n) => (CProj (renameCon st c, n), loc)

fun renamePatCon st pc =
    case pc of
        PConVar n =>
        (case St.lookup (st, n) of
             NONE => pc
           | SOME n' => PConVar n')
      | PConProj (n, ms, x) =>
        (case St.lookup (st, n) of
             NONE => pc
           | SOME n' => PConProj (n', ms, x))

fun renamePat st (all as (p, loc)) =
    case p of
        PVar (x, c) => (PVar (x, renameCon st c), loc)
      | PPrim _ => all
      | PCon (dk, pc, cs, po) => (PCon (dk, renamePatCon st pc,
                                        map (renameCon st) cs,
                                        Option.map (renamePat st) po), loc)
      | PRecord xpcs => (PRecord (map (fn (x, p, c) =>
                                          (x, renamePat st p, renameCon st c)) xpcs), loc)

fun renameExp st (all as (e, loc)) =
    case e of
        EPrim _ => all
      | ERel _ => all
      | ENamed n =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (ENamed n', loc))
      | EModProj (n, ms, x) =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (EModProj (n', ms, x), loc))
      | EApp (e1, e2) => (EApp (renameExp st e1, renameExp st e2), loc)
      | EAbs (x, dom, ran, e) => (EAbs (x, renameCon st dom, renameCon st ran, renameExp st e), loc)
      | ECApp (e, c) => (ECApp (renameExp st e, renameCon st c), loc)
      | ECAbs (x, k, e) => (ECAbs (x, k, renameExp st e), loc)
      | EKAbs (x, e) => (EKAbs (x, renameExp st e), loc)
      | EKApp (e, k) => (EKApp (renameExp st e, k), loc)
      | ERecord xecs => (ERecord (map (fn (x, e, c) => (renameCon st x,
                                                        renameExp st e,
                                                        renameCon st c)) xecs), loc)
      | EField (e, c, {field, rest}) => (EField (renameExp st e,
                                                 renameCon st c,
                                                 {field = renameCon st field,
                                                  rest = renameCon st rest}), loc)
      | EConcat (e1, c1, e2, c2) => (EConcat (renameExp st e1,
                                              renameCon st c1,
                                              renameExp st e2,
                                              renameCon st c2), loc)
      | ECut (e, c, {field, rest}) => (ECut (renameExp st e,
                                             renameCon st c,
                                             {field = renameCon st field,
                                              rest = renameCon st rest}), loc)
      | ECutMulti (e, c, {rest}) => (ECutMulti (renameExp st e,
                                                renameCon st c,
                                                {rest = renameCon st rest}), loc)
      | ECase (e, pes, {disc, result}) => (ECase (renameExp st e,
                                                  map (fn (p, e) => (renamePat st p, renameExp st e)) pes,
                                                  {disc = renameCon st disc,
                                                   result = renameCon st result}), loc)
      | EWrite e => (EWrite (renameExp st e), loc)
      | ELet (x, c1, e1, e2) => (ELet (x, renameCon st c1,
                                      renameExp st e1,
                                      renameExp st e2), loc)

fun renameSitem st (all as (si, loc)) =
    case si of
        SgiConAbs _ => all
      | SgiCon (x, n, k, c) => (SgiCon (x, n, k, renameCon st c), loc)
      | SgiDatatype dts => (SgiDatatype (map (fn (x, n, xs, cns) =>
                                                 (x, n, xs,
                                                  map (fn (x, n, co) =>
                                                          (x, n, Option.map (renameCon st) co)) cns)) dts),
                            loc)
      | SgiDatatypeImp (x, n, n', xs, x', xs', cns) =>
        (SgiDatatypeImp (x, n, n', xs, x', xs',
                         map (fn (x, n, co) =>
                                 (x, n, Option.map (renameCon st) co)) cns), loc)
      | SgiVal (x, n, c) => (SgiVal (x, n, renameCon st c), loc)
      | SgiSgn (x, n, sg) => (SgiSgn (x, n, renameSgn st sg), loc)
      | SgiStr (x, n, sg) => (SgiStr (x, n, renameSgn st sg), loc)

and renameSgn st (all as (sg, loc)) =
    case sg of
        SgnConst sis => (SgnConst (map (renameSitem st) sis), loc)
      | SgnVar n =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (SgnVar n', loc))
      | SgnFun (x, n, dom, ran) => (SgnFun (x, n, renameSgn st dom, renameSgn st ran), loc)
      | SgnWhere (sg, xs, s, c) => (SgnWhere (renameSgn st sg, xs, s, renameCon st c), loc)
      | SgnProj (n, ms, x) =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (SgnProj (n', ms, x), loc))

fun renameDecl st (all as (d, loc)) =
    case d of
        DCon (x, n, k, c) => (DCon (x, n, k, renameCon st c), loc)
      | DDatatype dts => (DDatatype (map (fn (x, n, xs, cns) =>
                                             (x, n, xs,
                                              map (fn (x, n, co) =>
                                                      (x, n, Option.map (renameCon st) co)) cns)) dts),
                          loc)
      | DDatatypeImp (x, n, n', xs, x', xs', cns) =>
        (DDatatypeImp (x, n, n', xs, x', xs',
                       map (fn (x, n, co) =>
                               (x, n, Option.map (renameCon st) co)) cns), loc)
      | DVal (x, n, c, e) => (DVal (x, n, renameCon st c, renameExp st e), loc)
      | DValRec vis => (DValRec (map (fn (x, n, c, e) => (x, n, renameCon st c, renameExp st e)) vis), loc)
      | DSgn (x, n, sg) => (DSgn (x, n, renameSgn st sg), loc)
      | DStr (x, n, sg, str) => (DStr (x, n, renameSgn st sg, renameStr st str), loc)
      | DFfiStr (x, n, sg) => (DFfiStr (x, n, renameSgn st sg), loc)
      | DExport (n, sg, str) =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (DExport (n', renameSgn st sg, renameStr st str), loc))
      | DTable (n, x, m, c1, e1, c2, e2, c3) =>
        (DTable (n, x, m, renameCon st c1, renameExp st e1, renameCon st c2,
                 renameExp st e2, renameCon st c3), loc)
      | DSequence _ => all
      | DView (n, x, n', e, c) => (DView (n, x, n', renameExp st e, renameCon st c), loc)
      | DIndex (e1, e2) => (DIndex (renameExp st e1, renameExp st e2), loc)
      | DDatabase _ => all
      | DCookie (n, x, n', c) => (DCookie (n, x, n', renameCon st c), loc)
      | DStyle _ => all
      | DTask (e1, e2) => (DTask (renameExp st e1, renameExp st e2), loc)
      | DPolicy e => (DPolicy (renameExp st e), loc)
      | DOnError (n, xs, x) =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (DOnError (n', xs, x), loc))
      | DFfi (x, n, modes, t) => (DFfi (x, n, modes, renameCon st t), loc)

and renameStr st (all as (str, loc)) =
    case str of
        StrConst ds => (StrConst (map (renameDecl st) ds), loc)
      | StrVar n =>
        (case St.lookup (st, n) of
             NONE => all
           | SOME n' => (StrVar n', loc))
      | StrProj (str, x) => (StrProj (renameStr st str, x), loc)
      | StrFun (x, n, dom, ran, str) => (StrFun (x, n, renameSgn st dom,
                                                 renameSgn st ran,
                                                 renameStr st str), loc)
      | StrApp (str1, str2) => (StrApp (renameStr st str1, renameStr st str2), loc)



fun fromArity (n, loc) =
    case n of
        0 => (KType, loc)
      | _ => (KArrow ((KType, loc), fromArity (n - 1, loc)), loc)

fun dupDecl (all as (d, loc), st) =
    case d of
        DCon (x, n, k, c) =>
        let
            val (st, n') = St.bind (st, n)
        in
            ([(DCon (x, n, k, renameCon st c), loc),
              (DCon (x, n', k, (CNamed n, loc)), loc)],
             st)
        end
      | DDatatype dts =>
        let
            val d = (DDatatype (map (fn (x, n, xs, cns) =>
                                        (x, n, xs,
                                         map (fn (x, n, co) =>
                                                 (x, n, Option.map (renameCon st) co)) cns)) dts),
                     loc)

            val (dts', st) = ListUtil.foldlMap (fn ((x, n, xs, cns), st) =>
                                                   let
                                                       val (st, n') = St.bind (st, n)

                                                       val (cns', st) = ListUtil.foldlMap
                                                                            (fn ((x, n, _), st) =>
                                                                                let
                                                                                    val (st, n') =
                                                                                        St.bind (st, n)
                                                                                in
                                                                                    ((x, n, n'), st)
                                                                                end) st cns
                                                   in
                                                       ((x, n, length xs, n', cns'), st)
                                                   end) st dts

            val env = E.declBinds E.empty d
        in
            (d
             :: map (fn (x, n, arity, n', _) =>
                        (DCon (x, n', fromArity (arity, loc), (CNamed n, loc)), loc)) dts'
             @ ListUtil.mapConcat (fn (_, _, _, _, cns') =>
                                      map (fn (x, n, n') =>
                                              (DVal (x, n', #2 (E.lookupENamed env n), (ENamed n, loc)),
                                               loc)) cns') dts',
             st)
        end
      | DDatatypeImp (x, n, n', xs, x', xs', cns) =>
        let
            val d = (DDatatypeImp (x, n, n', xs, x', xs',
                                   map (fn (x, n, co) =>
                                           (x, n, Option.map (renameCon st) co)) cns), loc)

            val (cns', st) = ListUtil.foldlMap
                                 (fn ((x, n, _), st) =>
                                     let
                                         val (st, n') =
                                             St.bind (st, n)
                                     in
                                         ((x, n, n'), st)
                                     end) st cns

            val (st, n') = St.bind (st, n)

            val env = E.declBinds E.empty d
        in
            (d
             :: (DCon (x, n', fromArity (length xs, loc), (CNamed n, loc)), loc)
             :: map (fn (x, n, n') =>
                        (DVal (x, n', #2 (E.lookupENamed env n), (ENamed n, loc)),
                         loc)) cns',
             st)
        end
      | DVal (x, n, c, e) =>
        let
            val (st, n') = St.bind (st, n)
            val c' = renameCon st c
        in
            ([(DVal (x, n, c', renameExp st e), loc),
              (DVal (x, n', c', (ENamed n, loc)), loc)],
             st)
        end
      | DValRec vis =>
        let
            val d = (DValRec (map (fn (x, n, c, e) => (x, n, renameCon st c, renameExp st e)) vis), loc)

            val (vis', st) = ListUtil.foldlMap (fn ((x, n, _, _), st) =>
                                                   let
                                                       val (st, n') = St.bind (st, n)
                                                   in
                                                       ((x, n, n'), st)
                                                   end) st vis

            val env = E.declBinds E.empty d
        in
            (d
             :: map (fn (x, n, n') => (DVal (x, n', #2 (E.lookupENamed env n), (ENamed n, loc)), loc)) vis',
             st)
        end
      | DSgn (x, n, sg) =>
        let
            val (st, n') = St.bind (st, n)
        in
            ([(DSgn (x, n, renameSgn st sg), loc),
              (DSgn (x, n', (SgnVar n, loc)), loc)],
             st)
        end
      | DStr (x, n, sg, str) =>
        let
            val (st, n') = St.bind (st, n)
            val sg' = renameSgn st sg
        in
            ([(DStr (x, n, sg', renameStr st str), loc),
              (DStr (x, n', sg', (StrVar n, loc)), loc)],
             st)
        end
      | DFfiStr (x, n, sg) => ([(DFfiStr (x, n, renameSgn st sg), loc)], st)
      | DExport (n, sg, str) =>
        (case St.lookup (st, n) of
             NONE => ([all], st)
           | SOME n' => ([(DExport (n', renameSgn st sg, renameStr st str), loc)], st))
      | DTable (n, x, m, c1, e1, c2, e2, c3) =>
        let
            val (st, m') = St.bind (st, m)

            val d = (DTable (n, x, m, renameCon st c1, renameExp st e1, renameCon st c2,
                             renameExp st e2, renameCon st c3), loc)

            val env = E.declBinds E.empty d
        in
            ([d, (DVal (x, m', #2 (E.lookupENamed env m), (ENamed m, loc)), loc)], st)
        end
      | DSequence (n, x, m) =>
        let
            val (st, m') = St.bind (st, m)

            val env = E.declBinds E.empty all
        in
            ([all, (DVal (x, m', #2 (E.lookupENamed env m), (ENamed m, loc)), loc)], st)
        end
      | DView (n, x, m, e, c) =>
        let
            val (st, m') = St.bind (st, m)

            val d = (DView (n, x, m, renameExp st e, renameCon st c), loc)

            val env = E.declBinds E.empty d
        in
            ([d, (DVal (x, m', #2 (E.lookupENamed env m), (ENamed m, loc)), loc)], st)
        end
      | DIndex (e1, e2) =>
        let
            val d = (DIndex (renameExp st e1, renameExp st e2), loc)

        in
            ([d], st)
        end
      | DDatabase _ => ([all], st)
      | DCookie (n, x, m, c) =>
        let
            val (st, m') = St.bind (st, m)

            val d = (DCookie (n, x, m, renameCon st c), loc)

            val env = E.declBinds E.empty d
        in
            ([d, (DVal (x, m', #2 (E.lookupENamed env m), (ENamed m, loc)), loc)], st)
        end
      | DStyle (n, x, m) =>
        let
            val (st, m') = St.bind (st, m)

            val env = E.declBinds E.empty all
        in
            ([all, (DVal (x, m', #2 (E.lookupENamed env m), (ENamed m, loc)), loc)], st)
        end
      | DTask (e1, e2) => ([(DTask (renameExp st e1, renameExp st e2), loc)], st)
      | DPolicy e => ([(DPolicy (renameExp st e), loc)], st)
      | DOnError (n, xs, x) =>
        (case St.lookup (st, n) of
             NONE => ([all], st)
           | SOME n' => ([(DOnError (n', xs, x), loc)], st))
      | DFfi (x, n, modes, t) =>
        let
            val (st, n') = St.bind (st, n)
            val t' = renameCon st t
        in
            ([(DFfi (x, n, modes, t'), loc),
              (DVal (x, n', t', (ENamed n, loc)), loc)],
             st)
        end

fun rename {NextId, FormalName, FormalId, Body = all as (str, loc)} =
    case str of
        StrConst ds =>
        let
            val st = St.create NextId
            val (st, n) = St.bind (st, FormalId)
                     
            val (ds, st) = ListUtil.foldlMapConcat dupDecl st ds

            (* Revenge of the functor parameter renamer!
             * See comment in elaborate.sml for the start of the saga.
             * We need to alpha-rename the argument to allow sufficient shadowing in the body. *)

            fun mungeName m =
                if List.exists (fn (DStr (x, _, _, _), _) => x = m
                                 | _ => false) ds then
                    mungeName ("?" ^ m)
                else
                    m

            val FormalName = mungeName FormalName

            val ds = (DStr (FormalName, n, (SgnConst [], loc), (StrVar FormalId, loc)), loc) :: ds
        in
            (St.next st, (StrConst ds, loc))
        end
      | _ => (NextId, all)

end
