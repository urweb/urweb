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

structure ElabEnv :> ELAB_ENV = struct

open Elab

structure U = ElabUtil

structure IM = IntBinaryMap
structure SM = BinaryMapFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

exception UnboundRel of int
exception UnboundNamed of int


(* AST utility functions *)

exception SynUnif

val liftConInCon =
    U.Con.mapB {kind = fn k => k,
                con = fn bound => fn c =>
                                     case c of
                                         CRel xn =>
                                         if xn < bound then
                                             c
                                         else
                                             CRel (xn + 1)
                                       (*| CUnif _ => raise SynUnif*)
                                       | _ => c,
                bind = fn (bound, U.Con.Rel _) => bound + 1
                        | (bound, _) => bound}

val lift = liftConInCon 0


(* Back to environments *)

datatype 'a var' =
         Rel' of int * 'a
       | Named' of int * 'a

datatype 'a var =
         NotBound
       | Rel of int * 'a
       | Named of int * 'a

type datatyp = (string * con option) IM.map

type env = {
     renameC : kind var' SM.map,
     relC : (string * kind) list,
     namedC : (string * kind * con option) IM.map,

     datatypes : datatyp IM.map,
     constructors : (datatype_kind * int * con option * int) SM.map,

     renameE : con var' SM.map,
     relE : (string * con) list,
     namedE : (string * con) IM.map,

     renameSgn : (int * sgn) SM.map,
     sgn : (string * sgn) IM.map,

     renameStr : (int * sgn) SM.map,
     str : (string * sgn) IM.map
}

val namedCounter = ref 0

fun newNamed () =
    let
        val r = !namedCounter
    in
        namedCounter := r + 1;
        r
    end

val empty = {
    renameC = SM.empty,
    relC = [],
    namedC = IM.empty,

    datatypes = IM.empty,
    constructors = SM.empty,

    renameE = SM.empty,
    relE = [],
    namedE = IM.empty,

    renameSgn = SM.empty,
    sgn = IM.empty,

    renameStr = SM.empty,
    str = IM.empty
}

fun pushCRel (env : env) x k =
    let
        val renameC = SM.map (fn Rel' (n, k) => Rel' (n+1, k)
                               | x => x) (#renameC env)
    in
        {renameC = SM.insert (renameC, x, Rel' (0, k)),
         relC = (x, k) :: #relC env,
         namedC = IM.map (fn (x, k, co) => (x, k, Option.map lift co)) (#namedC env),

         datatypes = #datatypes env,
         constructors = #constructors env,

         renameE = #renameE env,
         relE = map (fn (x, c) => (x, lift c)) (#relE env),
         namedE = IM.map (fn (x, c) => (x, lift c)) (#namedE env),

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env
        }
    end

fun lookupCRel (env : env) n =
    (List.nth (#relC env, n))
    handle Subscript => raise UnboundRel n

fun pushCNamedAs (env : env) x n k co =
    {renameC = SM.insert (#renameC env, x, Named' (n, k)),
     relC = #relC env,
     namedC = IM.insert (#namedC env, n, (x, k, co)),

     datatypes = #datatypes env,
     constructors = #constructors env,

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = #renameSgn env,
     sgn = #sgn env,
     
     renameStr = #renameStr env,
     str = #str env}

fun pushCNamed env x k co =
    let
        val n = !namedCounter
    in
        namedCounter := n + 1;
        (pushCNamedAs env x n k co, n)
    end

fun lookupCNamed (env : env) n =
    case IM.find (#namedC env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupC (env : env) x =
    case SM.find (#renameC env, x) of
        NONE => NotBound
      | SOME (Rel' x) => Rel x
      | SOME (Named' x) => Named x

fun pushDatatype (env : env) n xncs =
    let
        val dk = U.classifyDatatype xncs
    in
        {renameC = #renameC env,
         relC = #relC env,
         namedC = #namedC env,

         datatypes = IM.insert (#datatypes env, n,
                                foldl (fn ((x, n, to), cons) =>
                                          IM.insert (cons, n, (x, to))) IM.empty xncs),
         constructors = foldl (fn ((x, n', to), cmap) =>
                                  SM.insert (cmap, x, (dk, n', to, n)))
                              (#constructors env) xncs,

         renameE = #renameE env,
         relE = #relE env,
         namedE = #namedE env,

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env}
    end

fun lookupDatatype (env : env) n =
    case IM.find (#datatypes env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupDatatypeConstructor dt n =
    case IM.find (dt, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupConstructor (env : env) s = SM.find (#constructors env, s)

fun constructors dt = IM.foldri (fn (n, (x, to), ls) => (x, n, to) :: ls) [] dt

fun pushERel (env : env) x t =
    let
        val renameE = SM.map (fn Rel' (n, t) => Rel' (n+1, t)
                               | x => x) (#renameE env)
    in
        {renameC = #renameC env,
         relC = #relC env,
         namedC = #namedC env,

         datatypes = #datatypes env,
         constructors = #constructors env,

         renameE = SM.insert (renameE, x, Rel' (0, t)),
         relE = (x, t) :: #relE env,
         namedE = #namedE env,

         renameSgn = #renameSgn env,
         sgn = #sgn env,

         renameStr = #renameStr env,
         str = #str env}
    end

fun lookupERel (env : env) n =
    (List.nth (#relE env, n))
    handle Subscript => raise UnboundRel n

fun pushENamedAs (env : env) x n t =
    {renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     datatypes = #datatypes env,
     constructors = #constructors env,

     renameE = SM.insert (#renameE env, x, Named' (n, t)),
     relE = #relE env,
     namedE = IM.insert (#namedE env, n, (x, t)),

     renameSgn = #renameSgn env,
     sgn = #sgn env,
     
     renameStr = #renameStr env,
     str = #str env}

fun pushENamed env x t =
    let
        val n = !namedCounter
    in
        namedCounter := n + 1;
        (pushENamedAs env x n t, n)
    end

fun lookupENamed (env : env) n =
    case IM.find (#namedE env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupE (env : env) x =
    case SM.find (#renameE env, x) of
        NONE => NotBound
      | SOME (Rel' x) => Rel x
      | SOME (Named' x) => Named x

fun pushSgnNamedAs (env : env) x n sgis =
    {renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     datatypes = #datatypes env,
     constructors = #constructors env,

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = SM.insert (#renameSgn env, x, (n, sgis)),
     sgn = IM.insert (#sgn env, n, (x, sgis)),
     
     renameStr = #renameStr env,
     str = #str env}

fun pushSgnNamed env x sgis =
    let
        val n = !namedCounter
    in
        namedCounter := n + 1;
        (pushSgnNamedAs env x n sgis, n)
    end

fun lookupSgnNamed (env : env) n =
    case IM.find (#sgn env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupSgn (env : env) x = SM.find (#renameSgn env, x)

fun pushStrNamedAs (env : env) x n sgis =
    {renameC = #renameC env,
     relC = #relC env,
     namedC = #namedC env,

     datatypes = #datatypes env,
     constructors = #constructors env,

     renameE = #renameE env,
     relE = #relE env,
     namedE = #namedE env,

     renameSgn = #renameSgn env,
     sgn = #sgn env,

     renameStr = SM.insert (#renameStr env, x, (n, sgis)),
     str = IM.insert (#str env, n, (x, sgis))}

fun pushStrNamed env x sgis =
    let
        val n = !namedCounter
    in
        namedCounter := n + 1;
        (pushStrNamedAs env x n sgis, n)
    end

fun lookupStrNamed (env : env) n =
    case IM.find (#str env, n) of
        NONE => raise UnboundNamed n
      | SOME x => x

fun lookupStr (env : env) x = SM.find (#renameStr env, x)

fun sgiBinds env (sgi, loc) =
    case sgi of
        SgiConAbs (x, n, k) => pushCNamedAs env x n k NONE
      | SgiCon (x, n, k, c) => pushCNamedAs env x n k (SOME c)
      | SgiDatatype (x, n, xncs) =>
        let
            val env = pushCNamedAs env x n (KType, loc) NONE
        in
            foldl (fn ((x', n', NONE), env) => pushENamedAs env x' n' (CNamed n, loc)
                    | ((x', n', SOME t), env) => pushENamedAs env x' n' (TFun (t, (CNamed n, loc)), loc))
            env xncs
        end
      | SgiDatatypeImp (x, n, m1, ms, x', xncs) =>
        let
            val env = pushCNamedAs env x n (KType, loc) (SOME (CModProj (m1, ms, x'), loc))
        in
            foldl (fn ((x', n', NONE), env) => pushENamedAs env x' n' (CNamed n, loc)
                    | ((x', n', SOME t), env) => pushENamedAs env x' n' (TFun (t, (CNamed n, loc)), loc))
            env xncs
        end
      | SgiVal (x, n, t) => pushENamedAs env x n t
      | SgiStr (x, n, sgn) => pushStrNamedAs env x n sgn
      | SgiSgn (x, n, sgn) => pushSgnNamedAs env x n sgn
      | SgiConstraint _ => env

fun sgnSeek f sgis =
    let
        fun seek (sgis, sgns, strs, cons) =
            case sgis of
                [] => NONE
              | (sgi, _) :: sgis =>
                case f sgi of
                    SOME v =>
                    let
                        val cons =
                            case sgi of
                                SgiDatatype (x, n, _) => IM.insert (cons, n, x)
                              | SgiDatatypeImp (x, n, _, _, _, _) => IM.insert (cons, n, x)
                              | _ => cons
                    in
                        SOME (v, (sgns, strs, cons))
                    end
                  | NONE =>
                    case sgi of
                        SgiConAbs (x, n, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x))
                      | SgiCon (x, n, _, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x))
                      | SgiDatatype (x, n, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x))
                      | SgiDatatypeImp (x, n, _, _, _, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x))
                      | SgiVal _ => seek (sgis, sgns, strs, cons)
                      | SgiSgn (x, n, _) => seek (sgis, IM.insert (sgns, n, x), strs, cons)
                      | SgiStr (x, n, _) => seek (sgis, sgns, IM.insert (strs, n, x), cons)
                      | SgiConstraint _ => seek (sgis, sgns, strs, cons)
    in
        seek (sgis, IM.empty, IM.empty, IM.empty)
    end

fun id x = x

fun unravelStr (str, _) =
    case str of
        StrVar x => (x, [])
      | StrProj (str, m) =>
        let
            val (x, ms) = unravelStr str
        in
            (x, ms @ [m])
        end
      | _ => raise Fail "unravelStr"

fun sgnS_con (str, (sgns, strs, cons)) c =
    case c of
        CModProj (m1, ms, x) =>
        (case IM.find (strs, m1) of
             NONE => c
           | SOME m1x =>
             let
                 val (m1, ms') = unravelStr str
             in
                 CModProj (m1, ms' @ m1x :: ms, x)
             end)
      | CNamed n =>
        (case IM.find (cons, n) of
             NONE => c
           | SOME nx =>
             let
                 val (m1, ms) = unravelStr str
             in
                 CModProj (m1, ms, nx)
             end)
      | _ => c

fun sgnS_sgn (str, (sgns, strs, cons)) sgn =
    case sgn of
        SgnProj (m1, ms, x) =>
        (case IM.find (strs, m1) of
             NONE => sgn
           | SOME m1x =>
             let
                 val (m1, ms') = unravelStr str
             in
                 SgnProj (m1, ms' @ m1x :: ms, x)
             end)
      | SgnVar n =>
        (case IM.find (sgns, n) of
             NONE => sgn
           | SOME nx =>
             let
                 val (m1, ms) = unravelStr str
             in
                 SgnProj (m1, ms, nx)
             end)
      | _ => sgn

fun sgnSubCon x =
    ElabUtil.Con.map {kind = id,
                      con = sgnS_con x}

fun sgnSubSgn x =
    ElabUtil.Sgn.map {kind = id,
                      con = sgnS_con x,
                      sgn_item = id,
                      sgn = sgnS_sgn x}

fun hnormSgn env (all as (sgn, loc)) =
    case sgn of
        SgnError => all
      | SgnVar n => hnormSgn env (#2 (lookupSgnNamed env n))
      | SgnConst _ => all
      | SgnFun _ => all
      | SgnProj (m, ms, x) =>
        let
            val (_, sgn) = lookupStrNamed env m
        in
            case projectSgn env {str = foldl (fn (m, str) => (StrProj (str, m), loc)) (StrVar m, loc) ms,
                                 sgn = sgn,
                                 field = x} of
                NONE => raise Fail "ElabEnv.hnormSgn: projectSgn failed"
              | SOME sgn => sgn
        end
      | SgnWhere (sgn, x, c) =>
        case #1 (hnormSgn env sgn) of
            SgnError => (SgnError, loc)
          | SgnConst sgis =>
            let
                fun traverse (pre, post) =
                    case post of
                        [] => raise Fail "ElabEnv.hnormSgn: Can't reduce 'where' [1]"
                      | (sgi as (SgiConAbs (x', n, k), loc)) :: rest =>
                        if x = x' then
                            List.revAppend (pre, (SgiCon (x', n, k, c), loc) :: rest)
                        else
                            traverse (sgi :: pre, rest)
                      | sgi :: rest => traverse (sgi :: pre, rest)

                val sgis = traverse ([], sgis)
            in
                (SgnConst sgis, loc)
            end
          | _ => raise Fail "ElabEnv.hnormSgn: Can't reduce 'where' [2]"

and projectSgn env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        (case sgnSeek (fn SgiSgn (x, _, sgn) => if x = field then SOME sgn else NONE | _ => NONE) sgis of
             NONE => NONE
           | SOME (sgn, subs) => SOME (sgnSubSgn (str, subs) sgn))
      | SgnError => SOME (SgnError, ErrorMsg.dummySpan)
      | _ => NONE

fun projectStr env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        (case sgnSeek (fn SgiStr (x, _, sgn) => if x = field then SOME sgn else NONE | _ => NONE) sgis of
             NONE => NONE
           | SOME (sgn, subs) => SOME (sgnSubSgn (str, subs) sgn))
      | SgnError => SOME (SgnError, ErrorMsg.dummySpan)
      | _ => NONE

fun chaseMpath env (n, ms) =
    let
        val (_, sgn) = lookupStrNamed env n
    in
        foldl (fn (m, (str, sgn)) =>
                                   case projectStr env {sgn = sgn, str = str, field = m} of
                                       NONE => raise Fail "kindof: Unknown substructure"
                                     | SOME sgn => ((StrProj (str, m), #2 sgn), sgn))
                               ((StrVar n, #2 sgn), sgn) ms
    end

fun projectCon env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        (case sgnSeek (fn SgiConAbs (x, _, k) => if x = field then SOME (k, NONE) else NONE
                        | SgiCon (x, _, k, c) => if x = field then SOME (k, SOME c) else NONE
                        | SgiDatatype (x, _, _) => if x = field then SOME ((KType, #2 sgn), NONE) else NONE
                        | SgiDatatypeImp (x, _, m1, ms, x', _) =>
                          if x = field then
                              SOME ((KType, #2 sgn), SOME (CModProj (m1, ms, x'), #2 sgn))
                          else
                              NONE
                        | _ => NONE) sgis of
             NONE => NONE
           | SOME ((k, co), subs) => SOME (k, Option.map (sgnSubCon (str, subs)) co))
      | SgnError => SOME ((KError, ErrorMsg.dummySpan), SOME (CError, ErrorMsg.dummySpan))
      | _ => NONE

fun projectDatatype env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        (case sgnSeek (fn SgiDatatype (x, _, xncs) => if x = field then SOME xncs else NONE
                        | SgiDatatypeImp (x, _, _, _, _, xncs) => if x = field then SOME xncs else NONE
                        | _ => NONE) sgis of
             NONE => NONE
           | SOME (xncs, subs) => SOME (map (fn (x, n, to) => (x, n, Option.map (sgnSubCon (str, subs)) to)) xncs))
      | _ => NONE

fun projectConstructor env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        let
            fun consider (n, xncs) =
                ListUtil.search (fn (x, n', to) =>
                                    if x <> field then
                                        NONE
                                    else
                                        SOME (U.classifyDatatype xncs, n', to, (CNamed n, #2 str))) xncs
        in
            case sgnSeek (fn SgiDatatype (_, n, xncs) => consider (n, xncs)
                           | SgiDatatypeImp (_, n, _, _, _, xncs) => consider (n, xncs)
                           | _ => NONE) sgis of
                NONE => NONE
              | SOME ((dk, n, to, t), subs) => SOME (dk, n, Option.map (sgnSubCon (str, subs)) to,
                                                     sgnSubCon (str, subs) t)
        end
      | _ => NONE

fun projectVal env {sgn, str, field} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis =>
        let
            fun seek (n, xncs) =
                ListUtil.search (fn (x, _, to) =>
                                    if x = field then
                                        SOME (case to of
                                                  NONE => (CNamed n, #2 sgn)
                                                | SOME t => (TFun (t, (CNamed n, #2 sgn)), #2 sgn))
                                    else
                                        NONE) xncs
        in
            case sgnSeek (fn SgiVal (x, _, c) => if x = field then SOME c else NONE
                           | SgiDatatype (_, n, xncs) => seek (n, xncs)
                           | SgiDatatypeImp (_, n, _, _, _, xncs) => seek (n, xncs)
                           | _ => NONE) sgis of
                NONE => NONE
              | SOME (c, subs) => SOME (sgnSubCon (str, subs) c)
        end
      | SgnError => SOME (CError, ErrorMsg.dummySpan)
      | _ => NONE

fun sgnSeekConstraints (str, sgis) =
    let
        fun seek (sgis, sgns, strs, cons, acc) =
            case sgis of
                [] => acc
              | (sgi, _) :: sgis =>
                case sgi of
                    SgiConstraint (c1, c2) =>
                    let
                        val sub = sgnSubCon (str, (sgns, strs, cons))
                    in
                        seek (sgis, sgns, strs, cons, (sub c1, sub c2) :: acc)
                    end
                  | SgiConAbs (x, n, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
                  | SgiCon (x, n, _, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
                  | SgiDatatype (x, n, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
                  | SgiDatatypeImp (x, n, _, _, _, _) => seek (sgis, sgns, strs, IM.insert (cons, n, x), acc)
                  | SgiVal _ => seek (sgis, sgns, strs, cons, acc)
                  | SgiSgn (x, n, _) => seek (sgis, IM.insert (sgns, n, x), strs, cons, acc)
                  | SgiStr (x, n, _) => seek (sgis, sgns, IM.insert (strs, n, x), cons, acc)
    in
        seek (sgis, IM.empty, IM.empty, IM.empty, [])
    end

fun projectConstraints env {sgn, str} =
    case #1 (hnormSgn env sgn) of
        SgnConst sgis => SOME (sgnSeekConstraints (str, sgis))
      | SgnError => SOME []
      | _ => NONE

fun declBinds env (d, loc) =
    case d of
        DCon (x, n, k, c) => pushCNamedAs env x n k (SOME c)
      | DDatatype (x, n, xncs) =>
        let
            val env = pushCNamedAs env x n (KType, loc) NONE
            val env = pushDatatype env n xncs
        in
            foldl (fn ((x', n', NONE), env) => pushENamedAs env x' n' (CNamed n, loc)
                    | ((x', n', SOME t), env) => pushENamedAs env x' n' (TFun (t, (CNamed n, loc)), loc))
            env xncs
        end
      | DDatatypeImp (x, n, m, ms, x', xncs) =>
        let
            val t = (CModProj (m, ms, x'), loc)
            val env = pushCNamedAs env x n (KType, loc) (SOME t)
            val env = pushDatatype env n xncs

            val t = (CNamed n, loc)
        in
            foldl (fn ((x', n', NONE), env) => pushENamedAs env x' n' t
                    | ((x', n', SOME t'), env) => pushENamedAs env x' n' (TFun (t', t), loc))
            env xncs
        end
      | DVal (x, n, t, _) => pushENamedAs env x n t
      | DValRec vis => foldl (fn ((x, n, t, _), env) => pushENamedAs env x n t) env vis
      | DSgn (x, n, sgn) => pushSgnNamedAs env x n sgn
      | DStr (x, n, sgn, _) => pushStrNamedAs env x n sgn
      | DFfiStr (x, n, sgn) => pushStrNamedAs env x n sgn
      | DConstraint _ => env
      | DExport _ => env

end
