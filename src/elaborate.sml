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

structure Elaborate :> ELABORATE = struct

structure P = Prim
structure L = Source
structure L' = Elab
structure E = ElabEnv
structure U = ElabUtil

open Print
open ElabPrint

fun elabKind (k, loc) =
    case k of
        L.KType => (L'.KType, loc)
      | L.KArrow (k1, k2) => (L'.KArrow (elabKind k1, elabKind k2), loc)
      | L.KName => (L'.KName, loc)
      | L.KRecord k => (L'.KRecord (elabKind k), loc)

fun elabExplicitness e =
    case e of
        L.Explicit => L'.Explicit
      | L.Implicit => L'.Implicit

fun occursKind r =
    U.Kind.exists (fn L'.KUnif (_, r') => r = r'
                    | _ => false)

datatype kunify_error =
         KOccursCheckFailed of L'.kind * L'.kind
       | KIncompatible of L'.kind * L'.kind

exception KUnify' of kunify_error

fun kunifyError err =
    case err of
        KOccursCheckFailed (k1, k2) =>
        eprefaces "Kind occurs check failed"
        [("Kind 1", p_kind k1),
         ("Kind 2", p_kind k2)]
      | KIncompatible (k1, k2) =>
        eprefaces "Incompatible kinds"
        [("Kind 1", p_kind k1),
         ("Kind 2", p_kind k2)]

fun unifyKinds' (k1All as (k1, _)) (k2All as (k2, _)) =
    let
        fun err f = raise KUnify' (f (k1All, k2All))
    in
        case (k1, k2) of
            (L'.KType, L'.KType) => ()
          | (L'.KArrow (d1, r1), L'.KArrow (d2, r2)) =>
            (unifyKinds' d1 d2;
             unifyKinds' r1 r2)
          | (L'.KName, L'.KName) => ()
          | (L'.KRecord k1, L'.KRecord k2) => unifyKinds' k1 k2

          | (L'.KError, _) => ()
          | (_, L'.KError) => ()

          | (L'.KUnif (_, ref (SOME k1All)), _) => unifyKinds' k1All k2All
          | (_, L'.KUnif (_, ref (SOME k2All))) => unifyKinds' k1All k2All

          | (L'.KUnif (_, r1), L'.KUnif (_, r2)) =>
            if r1 = r2 then
                ()
            else
                r1 := SOME k2All

          | (L'.KUnif (_, r), _) =>
            if occursKind r k2All then
                err KOccursCheckFailed
            else
                r := SOME k2All
          | (_, L'.KUnif (_, r)) =>
            if occursKind r k1All then
                err KOccursCheckFailed
            else
                r := SOME k1All

          | _ => err KIncompatible
    end

exception KUnify of L'.kind * L'.kind * kunify_error

fun unifyKinds k1 k2 =
    unifyKinds' k1 k2
    handle KUnify' err => raise KUnify (k1, k2, err)

datatype con_error =
         UnboundCon of ErrorMsg.span * string
       | WrongKind of L'.con * L'.kind * L'.kind * kunify_error

fun conError env err =
    case err of
        UnboundCon (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound constructor variable " ^ s)
      | WrongKind (c, k1, k2, kerr) =>
        (ErrorMsg.errorAt (#2 c) "Wrong kind";
         eprefaces' [("Constructor", p_con env c),
                     ("Have kind", p_kind k1),
                     ("Need kind", p_kind k2)];
         kunifyError kerr)

fun checkKind env c k1 k2 =
    unifyKinds k1 k2
    handle KUnify (k1, k2, err) =>
           conError env (WrongKind (c, k1, k2, err))

val dummy = ErrorMsg.dummySpan

val ktype = (L'.KType, dummy)
val kname = (L'.KName, dummy)
val ktype_record = (L'.KRecord ktype, dummy)

val cerror = (L'.CError, dummy)
val kerror = (L'.KError, dummy)
val eerror = (L'.EError, dummy)

local
    val count = ref 0
in

fun resetKunif () = count := 0

fun kunif () =
    let
        val n = !count
        val s = if n <= 26 then
                    str (chr (ord #"A" + n))
                else
                    "U" ^ Int.toString (n - 26)
    in
        count := n + 1;
        (L'.KUnif (s, ref NONE), dummy)
    end

end

local
    val count = ref 0
in

fun resetCunif () = count := 0

fun cunif k =
    let
        val n = !count
        val s = if n <= 26 then
                    str (chr (ord #"A" + n))
                else
                    "U" ^ Int.toString (n - 26)
    in
        count := n + 1;
        (L'.CUnif (k, s, ref NONE), dummy)
    end

end

fun elabCon env (c, loc) =
    case c of
        L.CAnnot (c, k) =>
        let
            val k' = elabKind k
            val (c', ck) = elabCon env c
        in
            checkKind env c' ck k';
            (c', k')
        end

      | L.TFun (t1, t2) =>
        let
            val (t1', k1) = elabCon env t1
            val (t2', k2) = elabCon env t2
        in
            checkKind env t1' k1 ktype;
            checkKind env t2' k2 ktype;
            ((L'.TFun (t1', t2'), loc), ktype)
        end
      | L.TCFun (e, x, k, t) =>
        let
            val e' = elabExplicitness e
            val k' = elabKind k
            val env' = E.pushCRel env x k'
            val (t', tk) = elabCon env' t
        in
            checkKind env t' tk ktype;
            ((L'.TCFun (e', x, k', t'), loc), ktype)
        end
      | L.TRecord c =>
        let
            val (c', ck) = elabCon env c
            val k = (L'.KRecord ktype, loc)
        in
            checkKind env c' ck k;
            ((L'.TRecord c', loc), ktype)
        end

      | L.CVar s =>
        (case E.lookupC env s of
             E.NotBound =>
             (conError env (UnboundCon (loc, s));
              (cerror, kerror))
           | E.Rel (n, k) =>
             ((L'.CRel n, loc), k)
           | E.Named (n, k) =>
             ((L'.CNamed n, loc), k))
      | L.CApp (c1, c2) =>
        let
            val (c1', k1) = elabCon env c1
            val (c2', k2) = elabCon env c2
            val dom = kunif ()
            val ran = kunif ()
        in
            checkKind env c1' k1 (L'.KArrow (dom, ran), loc);
            checkKind env c2' k2 dom;
            ((L'.CApp (c1', c2'), loc), ran)
        end
      | L.CAbs (x, k, t) =>
        let
            val k' = elabKind k
            val env' = E.pushCRel env x k'
            val (t', tk) = elabCon env' t
        in
            ((L'.CAbs (x, k', t'), loc),
             (L'.KArrow (k', tk), loc))
        end

      | L.CName s =>
        ((L'.CName s, loc), kname)

      | L.CRecord xcs =>
        let
            val k = kunif ()

            val xcs' = map (fn (x, c) =>
                               let
                                   val (x', xk) = elabCon env x
                                   val (c', ck) = elabCon env c
                               in
                                   checkKind env x' xk kname;
                                   checkKind env c' ck k;
                                   (x', c')
                               end) xcs
        in
            ((L'.CRecord (k, xcs'), loc), (L'.KRecord k, loc))
        end
      | L.CConcat (c1, c2) =>
        let
            val (c1', k1) = elabCon env c1
            val (c2', k2) = elabCon env c2
            val ku = kunif ()
            val k = (L'.KRecord ku, loc)
        in
            checkKind env c1' k1 k;
            checkKind env c2' k2 k;
            ((L'.CConcat (c1', c2'), loc), k)
        end

fun kunifsRemain k =
    case k of
        L'.KUnif (_, ref NONE) => true
      | _ => false
fun cunifsRemain c =
    case c of
        L'.CUnif (_, _, ref NONE) => true
      | _ => false

val kunifsInKind = U.Kind.exists kunifsRemain
val kunifsInCon = U.Con.exists {kind = kunifsRemain,
                                con = fn _ => false}
val kunifsInExp = U.Exp.exists {kind = kunifsRemain,
                                con = fn _ => false,
                                exp = fn _ => false}

val cunifsInCon = U.Con.exists {kind = fn _ => false,
                                con = cunifsRemain}
val cunifsInExp = U.Exp.exists {kind = fn _ => false,
                                con = cunifsRemain,
                                exp = fn _ => false}

fun occursCon r =
    U.Con.exists {kind = fn _ => false,
                  con = fn L'.CUnif (_, _, r') => r = r'
                         | _ => false}

datatype cunify_error =
         CKind of L'.kind * L'.kind * kunify_error
       | COccursCheckFailed of L'.con * L'.con
       | CIncompatible of L'.con * L'.con
       | CExplicitness of L'.con * L'.con
       | CKindof of L'.con
       | CRecordFailure

exception CUnify' of cunify_error

fun cunifyError env err =
    case err of
        CKind (k1, k2, kerr) =>
        (eprefaces "Kind unification failure"
                   [("Kind 1", p_kind k1),
                    ("Kind 2", p_kind k2)];
         kunifyError kerr)
      | COccursCheckFailed (c1, c2) =>
        eprefaces "Constructor occurs check failed"
                  [("Con 1", p_con env c1),
                   ("Con 2", p_con env c2)]
      | CIncompatible (c1, c2) =>
        eprefaces "Incompatible constructors"
                  [("Con 1", p_con env c1),
                   ("Con 2", p_con env c2)]
      | CExplicitness (c1, c2) =>
        eprefaces "Differing constructor function explicitness"
                  [("Con 1", p_con env c1),
                   ("Con 2", p_con env c2)]
      | CKindof c =>
        eprefaces "Kind unification variable blocks kindof calculation"
                  [("Con", p_con env c)]
      | CRecordFailure =>
        eprefaces "Can't unify record constructors" []

exception SynUnif = E.SynUnif
val liftConInCon = E.liftConInCon

val subConInCon =
    U.Con.mapB {kind = fn k => k,
                con = fn (xn, rep) => fn c =>
                                  case c of
                                      L'.CRel xn' =>
                                      if xn = xn' then
                                          #1 rep
                                      else
                                          c
                                    (*| L'.CUnif _ => raise SynUnif*)
                                    | _ => c,
                bind = fn ((xn, rep), U.Con.Rel _) => (xn+1, liftConInCon 0 rep)
                        | (ctx, _) => ctx}

type record_summary = {
     fields : (L'.con * L'.con) list,
     unifs : (L'.con * L'.con option ref) list,
     others : L'.con list
}

fun summaryToCon {fields, unifs, others} =
    let
        val c = (L'.CRecord (ktype, []), dummy)
        val c = List.foldr (fn (c', c) => (L'.CConcat (c', c), dummy)) c others
        val c = List.foldr (fn ((c', _), c) => (L'.CConcat (c', c), dummy)) c unifs
    in
        (L'.CConcat ((L'.CRecord (ktype, fields), dummy), c), dummy)
    end

fun p_summary env s = p_con env (summaryToCon s)

exception CUnify of L'.con * L'.con * cunify_error

fun hnormKind (kAll as (k, _)) =
    case k of
        L'.KUnif (_, ref (SOME k)) => hnormKind k
      | _ => kAll

fun kindof env (c, loc) =
    case c of
        L'.TFun _ => ktype
      | L'.TCFun _ => ktype
      | L'.TRecord _ => ktype

      | L'.CRel xn => #2 (E.lookupCRel env xn)
      | L'.CNamed xn => #2 (E.lookupCNamed env xn)
      | L'.CApp (c, _) =>
        (case #1 (hnormKind (kindof env c)) of
             L'.KArrow (_, k) => k
           | L'.KError => kerror
           | _ => raise CUnify' (CKindof c))
      | L'.CAbs (x, k, c) => (L'.KArrow (k, kindof (E.pushCRel env x k) c), loc)

      | L'.CName _ => kname

      | L'.CRecord (k, _) => (L'.KRecord k, loc)
      | L'.CConcat (c, _) => kindof env c

      | L'.CError => kerror
      | L'.CUnif (k, _, _) => k

fun unifyRecordCons env (c1, c2) =
    let
        val k1 = kindof env c1
        val k2 = kindof env c2
    in
        unifyKinds k1 k2;
        unifySummaries env (k1, recordSummary env c1, recordSummary env c2)
    end

and recordSummary env c : record_summary =
    case hnormCon env c of
        (L'.CRecord (_, xcs), _) => {fields = xcs, unifs = [], others = []}
      | (L'.CConcat (c1, c2), _) =>
        let
            val s1 = recordSummary env c1
            val s2 = recordSummary env c2
        in
            {fields = #fields s1 @ #fields s2,
             unifs = #unifs s1 @ #unifs s2,
             others = #others s1 @ #others s2}
        end
      | (L'.CUnif (_, _, ref (SOME c)), _) => recordSummary env c
      | c' as (L'.CUnif (_, _, r), _) => {fields = [], unifs = [(c', r)], others = []}
      | c' => {fields = [], unifs = [], others = [c']}

and consEq env (c1, c2) =
    (unifyCons env c1 c2;
     true)
    handle CUnify _ => false

and unifySummaries env (k, s1 : record_summary, s2 : record_summary) =
    let
        (*val () = eprefaces "Summaries" [("#1", p_summary env s1),
                                          ("#2", p_summary env s2)]*)

        fun eatMatching p (ls1, ls2) =
            let
                fun em (ls1, ls2, passed1) =
                    case ls1 of
                        [] => (rev passed1, ls2)
                      | h1 :: t1 =>
                        let
                            fun search (ls2', passed2) =
                                case ls2' of
                                    [] => em (t1, ls2, h1 :: passed1)
                                  | h2 :: t2 =>
                                    if p (h1, h2) then
                                        em (t1, List.revAppend (passed2, t2), passed1)
                                    else
                                        search (t2, h2 :: passed2)
                        in
                            search (ls2, [])
                        end
            in
                em (ls1, ls2, [])
            end

        val (fs1, fs2) = eatMatching (fn ((x1, c1), (x2, c2)) =>
                                         if consEq env (x1, x2) then
                                             (unifyCons env c1 c2;
                                              true)
                                         else
                                             false) (#fields s1, #fields s2)
        (*val () = eprefaces "Summaries2" [("#1", p_summary env {fields = fs1, unifs = #unifs s1, others = #others s1}),
                                           ("#2", p_summary env {fields = fs2, unifs = #unifs s2, others = #others s2})]*)
        val (unifs1, unifs2) = eatMatching (fn ((_, r1), (_, r2)) => r1 = r2) (#unifs s1, #unifs s2)
        val (others1, others2) = eatMatching (consEq env) (#others s1, #others s2)

        fun unifFields (fs, others, unifs) =
            case (fs, others, unifs) of
                ([], [], _) => ([], [], unifs)
              | (_, _, []) => (fs, others, [])
              | (_, _, (_, r) :: rest) =>
                let
                    val r' = ref NONE
                    val cr' = (L'.CUnif (k, "recd", r'), dummy)

                    val prefix = case (fs, others) of
                                     ([], other :: others) =>
                                     List.foldl (fn (other, c) =>
                                                    (L'.CConcat (c, other), dummy))
                                                other others
                                   | (fs, []) =>
                                     (L'.CRecord (k, fs), dummy)
                                   | (fs, others) =>
                                     List.foldl (fn (other, c) =>
                                                    (L'.CConcat (c, other), dummy))
                                                (L'.CRecord (k, fs), dummy) others
                in
                    r := SOME (L'.CConcat (prefix, cr'), dummy);
                    ([], [], (cr', r') :: rest)
                end

        val (fs1, others1, unifs2) = unifFields (fs1, others1, unifs2)
        val (fs2, others2, unifs1) = unifFields (fs2, others2, unifs1)

        val clear1 = case (fs1, others1) of
                         ([], []) => true
                       | _ => false
        val clear2 = case (fs2, others2) of
                         ([], []) => true
                       | _ => false
        val empty = (L'.CRecord (k, []), dummy)
        fun pairOffUnifs (unifs1, unifs2) =
            case (unifs1, unifs2) of
                ([], _) =>
                if clear1 then
                    List.app (fn (_, r) => r := SOME empty) unifs2
                else
                    raise CUnify' CRecordFailure
              | (_, []) =>
                if clear2 then
                    List.app (fn (_, r) => r := SOME empty) unifs1
                else
                    raise CUnify' CRecordFailure
              | ((c1, _) :: rest1, (_, r2) :: rest2) =>
                (r2 := SOME c1;
                 pairOffUnifs (rest1, rest2))
    in
        pairOffUnifs (unifs1, unifs2)
    end

and hnormCon env (cAll as (c, _)) =
    case c of
        L'.CUnif (_, _, ref (SOME c)) => hnormCon env c

      | L'.CNamed xn =>
        (case E.lookupCNamed env xn of
             (_, _, SOME c') => hnormCon env c'
           | _ => cAll)

      | L'.CApp (c1, c2) =>
        (case hnormCon env c1 of
             (L'.CAbs (_, _, cb), _) =>
             ((hnormCon env (subConInCon (0, c2) cb))
              handle SynUnif => cAll)
           | _ => cAll)

      | L'.CConcat (c1, c2) =>
        (case (hnormCon env c1, hnormCon env c2) of
             ((L'.CRecord (k, xcs1), loc), (L'.CRecord (_, xcs2), _)) =>
             (L'.CRecord (k, xcs1 @ xcs2), loc)
           | _ => cAll)

      | _ => cAll

and unifyCons' env c1 c2 =
    unifyCons'' env (hnormCon env c1) (hnormCon env c2)
    
and unifyCons'' env (c1All as (c1, _)) (c2All as (c2, _)) =
    let
        fun err f = raise CUnify' (f (c1All, c2All))

        fun isRecord () = unifyRecordCons env (c1All, c2All)
    in
        case (c1, c2) of
            (L'.TFun (d1, r1), L'.TFun (d2, r2)) =>
            (unifyCons' env d1 d2;
             unifyCons' env r1 r2)
          | (L'.TCFun (expl1, x1, d1, r1), L'.TCFun (expl2, _, d2, r2)) =>
            if expl1 <> expl2 then
                err CExplicitness
            else
                (unifyKinds d1 d2;
                 unifyCons' (E.pushCRel env x1 d1) r1 r2)
          | (L'.TRecord r1, L'.TRecord r2) => unifyCons' env r1 r2

          | (L'.CRel n1, L'.CRel n2) =>
            if n1 = n2 then
                ()
            else
                err CIncompatible
          | (L'.CNamed n1, L'.CNamed n2) =>
            if n1 = n2 then
                ()
            else
                err CIncompatible

          | (L'.CApp (d1, r1), L'.CApp (d2, r2)) =>
            (unifyCons' env d1 d2;
             unifyCons' env r1 r2)
          | (L'.CAbs (x1, k1, c1), L'.CAbs (_, k2, c2)) =>
            (unifyKinds k1 k2;
             unifyCons' (E.pushCRel env x1 k1) c1 c2)

          | (L'.CName n1, L'.CName n2) =>
            if n1 = n2 then
                ()
            else
                err CIncompatible

          | (L'.CError, _) => ()
          | (_, L'.CError) => ()

          | (L'.CUnif (_, _, ref (SOME c1All)), _) => unifyCons' env c1All c2All
          | (_, L'.CUnif (_, _, ref (SOME c2All))) => unifyCons' env c1All c2All

          | (L'.CUnif (k1, _, r1), L'.CUnif (k2, _, r2)) =>
            if r1 = r2 then
                ()
            else
                (unifyKinds k1 k2;
                 r1 := SOME c2All)

          | (L'.CUnif (_, _, r), _) =>
            if occursCon r c2All then
                err COccursCheckFailed
            else
                r := SOME c2All
          | (_, L'.CUnif (_, _, r)) =>
            if occursCon r c1All then
                err COccursCheckFailed
            else
                r := SOME c1All

          | (L'.CRecord _, _) => isRecord ()
          | (_, L'.CRecord _) => isRecord ()
          | (L'.CConcat _, _) => isRecord ()
          | (_, L'.CConcat _) => isRecord ()

          | _ => err CIncompatible
    end

and unifyCons env c1 c2 =
    unifyCons' env c1 c2
    handle CUnify' err => raise CUnify (c1, c2, err)
         | KUnify args => raise CUnify (c1, c2, CKind args)

datatype exp_error =
       UnboundExp of ErrorMsg.span * string
     | Unify of L'.exp * L'.con * L'.con * cunify_error
     | Unif of string * L'.con
     | WrongForm of string * L'.exp * L'.con

fun expError env err =
    case err of
        UnboundExp (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound expression variable " ^ s)
      | Unify (e, c1, c2, uerr) =>
        (ErrorMsg.errorAt (#2 e) "Unification failure";
         eprefaces' [("Expression", p_exp env e),
                     ("Have con", p_con env c1),
                     ("Need con", p_con env c2)];
         cunifyError env uerr)
      | Unif (action, c) =>
        (ErrorMsg.errorAt (#2 c) ("Unification variable blocks " ^ action);
         eprefaces' [("Con", p_con env c)])
      | WrongForm (variety, e, t) =>
        (ErrorMsg.errorAt (#2 e) ("Expression is not a " ^ variety);
         eprefaces' [("Expression", p_exp env e),
                     ("Type", p_con env t)])

fun checkCon env e c1 c2 =
    unifyCons env c1 c2
    handle CUnify (c1, c2, err) =>
           expError env (Unify (e, c1, c2, err))

fun primType env p =
    let
        val s = case p of
                    P.Int _ => "int"
                  | P.Float _ => "float"
                  | P.String _ => "string"
    in
        case E.lookupC env s of
            E.NotBound => raise Fail ("Primitive type " ^ s ^ " unbound")
          | E.Rel _ => raise Fail ("Primitive type " ^ s ^ " bound as relative")
          | E.Named (n, (L'.KType, _)) => L'.CNamed n
          | E.Named _ => raise Fail ("Primitive type " ^ s ^ " bound at non-Type kind")
    end

fun typeof env (e, loc) =
    case e of
        L'.EPrim p => (primType env p, loc)
      | L'.ERel n => #2 (E.lookupERel env n)
      | L'.ENamed n => #2 (E.lookupENamed env n)
      | L'.EApp (e1, _) =>
        (case #1 (typeof env e1) of
             L'.TFun (_, c) => c
           | _ => raise Fail "typeof: Bad EApp")
      | L'.EAbs (x, t, e1) => (L'.TFun (t, typeof (E.pushERel env x t) e1), loc)
      | L'.ECApp (e1, c) =>
        (case #1 (typeof env e1) of
             L'.TCFun (_, _, _, c1) => subConInCon (0, c) c1
           | _ => raise Fail "typeof: Bad ECApp")
      | L'.ECAbs (expl, x, k, e1) => (L'.TCFun (expl, x, k, typeof (E.pushCRel env x k) e1), loc)

      | L'.ERecord xes => (L'.TRecord (L'.CRecord (ktype, map (fn (x, e) => (x, typeof env e)) xes), loc), loc)
      | L'.EField (_, _, {field, ...}) => field

      | L'.EError => cerror

fun elabHead env (e as (_, loc)) t =
    let
        fun unravel (t, e) =
            case hnormCon env t of
                (L'.TCFun (L'.Implicit, x, k, t'), _) =>
                let
                    val u = cunif k
                in
                    unravel (subConInCon (0, u) t',
                             (L'.ECApp (e, u), loc))
                end
              | _ => (e, t)
    in
        unravel (t, e)
    end

fun elabExp env (e, loc) =
    case e of
        L.EAnnot (e, t) =>
        let
            val (e', et) = elabExp env e
            val (t', _) = elabCon env t
        in
            checkCon env e' et t';
            (e', t')
        end

      | L.EPrim p => ((L'.EPrim p, loc), (primType env p, loc))
      | L.EVar s =>
        (case E.lookupE env s of
             E.NotBound =>
             (expError env (UnboundExp (loc, s));
              (eerror, cerror))
           | E.Rel (n, t) => ((L'.ERel n, loc), t)
           | E.Named (n, t) => ((L'.ENamed n, loc), t))
      | L.EApp (e1, e2) =>
        let
            val (e1', t1) = elabExp env e1
            val (e1', t1) = elabHead env e1' t1
            val (e2', t2) = elabExp env e2

            val dom = cunif ktype
            val ran = cunif ktype
            val t = (L'.TFun (dom, ran), dummy)
        in
            checkCon env e1' t1 t;
            checkCon env e2' t2 dom;
            ((L'.EApp (e1', e2'), loc), ran)
        end
      | L.EAbs (x, to, e) =>
        let
            val t' = case to of
                         NONE => cunif ktype
                       | SOME t =>
                         let
                             val (t', tk) = elabCon env t
                         in
                             checkKind env t' tk ktype;
                             t'
                         end
            val (e', et) = elabExp (E.pushERel env x t') e
        in
            ((L'.EAbs (x, t', e'), loc),
             (L'.TFun (t', et), loc))
        end
      | L.ECApp (e, c) =>
        let
            val (e', et) = elabExp env e
            val (e', et) = elabHead env e' et
            val (c', ck) = elabCon env c
        in
            case #1 (hnormCon env et) of
                L'.CError => (eerror, cerror)
              | L'.TCFun (_, _, k, eb) =>
                let
                    val () = checkKind env c' ck k
                    val eb' = subConInCon (0, c') eb
                              handle SynUnif => (expError env (Unif ("substitution", eb));
                                                 cerror)
                in
                    ((L'.ECApp (e', c'), loc), eb')
                end

              | L'.CUnif _ =>
                (expError env (Unif ("application", et));
                 (eerror, cerror))

              | _ =>
                (expError env (WrongForm ("constructor function", e', et));
                 (eerror, cerror))
        end
      | L.ECAbs (expl, x, k, e) =>
        let
            val expl' = elabExplicitness expl
            val k' = elabKind k
            val (e', et) = elabExp (E.pushCRel env x k') e
        in
            ((L'.ECAbs (expl', x, k', e'), loc),
             (L'.TCFun (expl', x, k', et), loc))
        end

      | L.ERecord xes =>
        let
            val xes' = map (fn (x, e) =>
                               let
                                   val (x', xk) = elabCon env x
                                   val (e', et) = elabExp env e
                               in
                                   checkKind env x' xk kname;
                                   (x', e', et)
                               end) xes
        in
            ((L'.ERecord (map (fn (x', e', _) => (x', e')) xes'), loc),
             (L'.TRecord (L'.CRecord (ktype, map (fn (x', _, et) => (x', et)) xes'), loc), loc))
        end

      | L.EField (e, c) =>
        let
            val (e', et) = elabExp env e
            val (c', ck) = elabCon env c

            val ft = cunif ktype
            val rest = cunif ktype_record
        in
            checkKind env c' ck kname;
            checkCon env e' et (L'.TRecord (L'.CConcat ((L'.CRecord (ktype, [(c', ft)]), loc), rest), loc), loc);
            ((L'.EField (e', c', {field = ft, rest = rest}), loc), ft)
        end
            

datatype decl_error =
         KunifsRemainKind of ErrorMsg.span * L'.kind
       | KunifsRemainCon of ErrorMsg.span * L'.con
       | KunifsRemainExp of ErrorMsg.span * L'.exp
       | CunifsRemainCon of ErrorMsg.span * L'.con
       | CunifsRemainExp of ErrorMsg.span * L'.exp

fun declError env err =
    case err of
        KunifsRemainKind (loc, k) =>
        (ErrorMsg.errorAt loc "Some kind unification variables are undetermined in kind";
         eprefaces' [("Kind", p_kind k)])
      | KunifsRemainCon (loc, c) =>
        (ErrorMsg.errorAt loc "Some kind unification variables are undetermined in constructor";
         eprefaces' [("Constructor", p_con env c)])
      | KunifsRemainExp (loc, e) =>
        (ErrorMsg.errorAt loc "Some kind unification variables are undetermined in expression";
         eprefaces' [("Expression", p_exp env e)])
      | CunifsRemainCon (loc, c) =>
        (ErrorMsg.errorAt loc "Some constructor unification variables are undetermined in constructor";
         eprefaces' [("Constructor", p_con env c)])
      | CunifsRemainExp (loc, e) =>
        (ErrorMsg.errorAt loc "Some constructor unification variables are undetermined in expression";
         eprefaces' [("Expression", p_exp env e)])

fun elabDecl env (d, loc) =
    (resetKunif ();
     resetCunif ();
     case d of
         L.DCon (x, ko, c) =>
         let
             val k' = case ko of
                          NONE => kunif ()
                        | SOME k => elabKind k

             val (c', ck) = elabCon env c
             val (env', n) = E.pushCNamed env x k' (SOME c')
         in
             checkKind env c' ck k';

             if ErrorMsg.anyErrors () then
                 ()
             else (
                 if kunifsInKind k' then
                     declError env (KunifsRemainKind (loc, k'))
                 else
                     ();

                 if kunifsInCon c' then
                     declError env (KunifsRemainCon (loc, c'))
                 else
                     ()
                 );

             (env',
              (L'.DCon (x, n, k', c'), loc))
         end
       | L.DVal (x, co, e) =>
         let
             val (c', ck) = case co of
                                NONE => (cunif ktype, ktype)
                              | SOME c => elabCon env c

             val (e', et) = elabExp env e
             val (env', n) = E.pushENamed env x c'
         in
             checkCon env e' et c';

             if ErrorMsg.anyErrors () then
                 ()
             else (
                 if kunifsInCon c' then
                     declError env (KunifsRemainCon (loc, c'))
                 else
                     ();

                 if cunifsInCon c' then
                     declError env (CunifsRemainCon (loc, c'))
                 else
                     ();

                 if kunifsInExp e' then
                     declError env (KunifsRemainExp (loc, e'))
                 else
                     ();

                 if cunifsInExp e' then
                     declError env (CunifsRemainExp (loc, e'))
                 else
                     ());

             (env',
              (L'.DVal (x, n, c', e'), loc))
         end)

fun elabFile env ds =
    ListUtil.mapfoldl (fn (d, env) => elabDecl env d) env ds

end
