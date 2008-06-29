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

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

fun elabExplicitness e =
    case e of
        L.Explicit => L'.Explicit
      | L.Implicit => L'.Implicit

fun occursKind r =
    U.Kind.exists (fn L'.KUnif (_, _, r') => r = r'
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

          | (L'.KUnif (_, _, ref (SOME k1All)), _) => unifyKinds' k1All k2All
          | (_, L'.KUnif (_, _, ref (SOME k2All))) => unifyKinds' k1All k2All

          | (L'.KUnif (_, _, r1), L'.KUnif (_, _, r2)) =>
            if r1 = r2 then
                ()
            else
                r1 := SOME k2All

          | (L'.KUnif (_, _, r), _) =>
            if occursKind r k2All then
                err KOccursCheckFailed
            else
                r := SOME k2All
          | (_, L'.KUnif (_, _, r)) =>
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
       | UnboundStrInCon of ErrorMsg.span * string
       | WrongKind of L'.con * L'.kind * L'.kind * kunify_error
       | DuplicateField of ErrorMsg.span * string

fun conError env err =
    case err of
        UnboundCon (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound constructor variable " ^ s)
      | UnboundStrInCon (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound structure " ^ s)
      | WrongKind (c, k1, k2, kerr) =>
        (ErrorMsg.errorAt (#2 c) "Wrong kind";
         eprefaces' [("Constructor", p_con env c),
                     ("Have kind", p_kind k1),
                     ("Need kind", p_kind k2)];
         kunifyError kerr)
      | DuplicateField (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate record field " ^ s)

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
val sgnerror = (L'.SgnError, dummy)
val strerror = (L'.StrError, dummy)

val int = ref cerror
val float = ref cerror
val string = ref cerror

local
    val count = ref 0
in

fun resetKunif () = count := 0

fun kunif loc =
    let
        val n = !count
        val s = if n <= 26 then
                    str (chr (ord #"A" + n))
                else
                    "U" ^ Int.toString (n - 26)
    in
        count := n + 1;
        (L'.KUnif (loc, s, ref NONE), dummy)
    end

end

local
    val count = ref 0
in

fun resetCunif () = count := 0

fun cunif (loc, k) =
    let
        val n = !count
        val s = if n <= 26 then
                    str (chr (ord #"A" + n))
                else
                    "U" ^ Int.toString (n - 26)
    in
        count := n + 1;
        (L'.CUnif (loc, k, s, ref NONE), dummy)
    end

end

fun elabKind (k, loc) =
    case k of
        L.KType => (L'.KType, loc)
      | L.KArrow (k1, k2) => (L'.KArrow (elabKind k1, elabKind k2), loc)
      | L.KName => (L'.KName, loc)
      | L.KRecord k => (L'.KRecord (elabKind k), loc)
      | L.KWild => kunif loc

fun foldKind (dom, ran, loc)=
    (L'.KArrow ((L'.KArrow ((L'.KName, loc),
                            (L'.KArrow (dom,
                                        (L'.KArrow (ran, ran), loc)), loc)), loc),
                (L'.KArrow (ran,
                            (L'.KArrow ((L'.KRecord dom, loc),
                                        ran), loc)), loc)), loc)

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

      | L.CVar ([], s) =>
        (case E.lookupC env s of
             E.NotBound =>
             (conError env (UnboundCon (loc, s));
              (cerror, kerror))
           | E.Rel (n, k) =>
             ((L'.CRel n, loc), k)
           | E.Named (n, k) =>
             ((L'.CNamed n, loc), k))
      | L.CVar (m1 :: ms, s) =>
        (case E.lookupStr env m1 of
             NONE => (conError env (UnboundStrInCon (loc, m1));
                      (cerror, kerror))
           | SOME (n, sgn) =>
             let
                 val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                     case E.projectStr env {sgn = sgn, str = str, field = m} of
                                         NONE => (conError env (UnboundStrInCon (loc, m));
                                                  (strerror, sgnerror))
                                       | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                  ((L'.StrVar n, loc), sgn) ms

                 val k = case E.projectCon env {sgn = sgn, str = str, field = s} of
                             NONE => (conError env (UnboundCon (loc, s));
                                      kerror)
                           | SOME (k, _) => k
             in
                 ((L'.CModProj (n, ms, s), loc), k)
             end)
                                                                       
      | L.CApp (c1, c2) =>
        let
            val (c1', k1) = elabCon env c1
            val (c2', k2) = elabCon env c2
            val dom = kunif loc
            val ran = kunif loc
        in
            checkKind env c1' k1 (L'.KArrow (dom, ran), loc);
            checkKind env c2' k2 dom;
            ((L'.CApp (c1', c2'), loc), ran)
        end
      | L.CAbs (x, ko, t) =>
        let
            val k' = case ko of
                         NONE => kunif loc
                       | SOME k => elabKind k
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
            val k = kunif loc

            val xcs' = map (fn (x, c) =>
                               let
                                   val (x', xk) = elabCon env x
                                   val (c', ck) = elabCon env c
                               in
                                   checkKind env x' xk kname;
                                   checkKind env c' ck k;
                                   (x', c')
                               end) xcs

            val rc = (L'.CRecord (k, xcs'), loc)
            (* Add duplicate field checking later. *)
        in
            (rc, (L'.KRecord k, loc))
        end
      | L.CConcat (c1, c2) =>
        let
            val (c1', k1) = elabCon env c1
            val (c2', k2) = elabCon env c2
            val ku = kunif loc
            val k = (L'.KRecord ku, loc)
        in
            checkKind env c1' k1 k;
            checkKind env c2' k2 k;
            ((L'.CConcat (c1', c2'), loc), k)
        end
      | L.CFold =>
        let
            val dom = kunif loc
            val ran = kunif loc
        in
            ((L'.CFold (dom, ran), loc),
             foldKind (dom, ran, loc))
        end

      | L.CWild k =>
        let
            val k' = elabKind k
        in
            (cunif (loc, k'), k')
        end

fun kunifsRemain k =
    case k of
        L'.KUnif (_, _, ref NONE) => true
      | _ => false
fun cunifsRemain c =
    case c of
        L'.CUnif (loc, _, _, ref NONE) => SOME loc
      | _ => NONE

val kunifsInDecl = U.Decl.exists {kind = kunifsRemain,
                                  con = fn _ => false,
                                  exp = fn _ => false,
                                  sgn_item = fn _ => false,
                                  sgn = fn _ => false,
                                  str = fn _ => false,
                                  decl = fn _ => false}

val cunifsInDecl = U.Decl.search {kind = fn _ => NONE,
                                  con = cunifsRemain,
                                  exp = fn _ => NONE,
                                  sgn_item = fn _ => NONE,
                                  sgn = fn _ => NONE,
                                  str = fn _ => NONE,
                                  decl = fn _ => NONE}

fun occursCon r =
    U.Con.exists {kind = fn _ => false,
                  con = fn L'.CUnif (_, _, _, r') => r = r'
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
                                      (case Int.compare (xn', xn) of
                                           EQUAL => #1 rep
                                         | GREATER => L'.CRel (xn' - 1)
                                         | LESS => c)
                                    (*| L'.CUnif _ => raise SynUnif*)
                                    | _ => c,
                bind = fn ((xn, rep), U.Con.Rel _) => (xn+1, liftConInCon 0 rep)
                        | (ctx, _) => ctx}

fun subStrInSgn (m1, m2) =
    U.Sgn.map {kind = fn k => k,
               con = fn c as L'.CModProj (m1', ms, x) =>
                        if m1 = m1' then
                            L'.CModProj (m2, ms, x)
                        else
                            c
                      | c => c,
               sgn_item = fn sgi => sgi,
               sgn = fn sgn => sgn}

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
        L'.KUnif (_, _, ref (SOME k)) => hnormKind k
      | _ => kAll

fun kindof env (c, loc) =
    case c of
        L'.TFun _ => ktype
      | L'.TCFun _ => ktype
      | L'.TRecord _ => ktype

      | L'.CRel xn => #2 (E.lookupCRel env xn)
      | L'.CNamed xn => #2 (E.lookupCNamed env xn)
      | L'.CModProj (n, ms, x) =>
        let
            val (_, sgn) = E.lookupStrNamed env n
            val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                       case E.projectStr env {sgn = sgn, str = str, field = m} of
                                           NONE => raise Fail "kindof: Unknown substructure"
                                         | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                             ((L'.StrVar n, loc), sgn) ms
        in
            case E.projectCon env {sgn = sgn, str = str, field = x} of
                NONE => raise Fail "kindof: Unknown con in structure"
              | SOME (k, _) => k
        end

      | L'.CApp (c, _) =>
        (case #1 (hnormKind (kindof env c)) of
             L'.KArrow (_, k) => k
           | L'.KError => kerror
           | _ => raise CUnify' (CKindof c))
      | L'.CAbs (x, k, c) => (L'.KArrow (k, kindof (E.pushCRel env x k) c), loc)

      | L'.CName _ => kname

      | L'.CRecord (k, _) => (L'.KRecord k, loc)
      | L'.CConcat (c, _) => kindof env c
      | L'.CFold (dom, ran) => foldKind (dom, ran, loc)

      | L'.CError => kerror
      | L'.CUnif (_, k, _, _) => k

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
      | (L'.CUnif (_, _, _, ref (SOME c)), _) => recordSummary env c
      | c' as (L'.CUnif (_, _, _, r), _) => {fields = [], unifs = [(c', r)], others = []}
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
                                         consEq env (c1, c2) andalso consEq env (x1, x2))
                                     (#fields s1, #fields s2)
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
                    val cr' = (L'.CUnif (dummy, k, "recd", r'), dummy)

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

and hnormCon env (cAll as (c, loc)) =
    case c of
        L'.CUnif (_, _, _, ref (SOME c)) => hnormCon env c

      | L'.CNamed xn =>
        (case E.lookupCNamed env xn of
             (_, _, SOME c') => hnormCon env c'
           | _ => cAll)

      | L'.CModProj (n, ms, x) =>
        let
            val (_, sgn) = E.lookupStrNamed env n
            val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                       case E.projectStr env {sgn = sgn, str = str, field = m} of
                                           NONE => raise Fail "hnormCon: Unknown substructure"
                                         | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                             ((L'.StrVar n, loc), sgn) ms
        in
            case E.projectCon env {sgn = sgn, str = str, field = x} of
                NONE => raise Fail "kindof: Unknown con in structure"
              | SOME (_, NONE) => cAll
              | SOME (_, SOME c) => hnormCon env c
        end

      | L'.CApp (c1, c2) =>
        (case #1 (hnormCon env c1) of
             L'.CAbs (x, k, cb) =>
             let
                 val sc = (hnormCon env (subConInCon (0, c2) cb))
                     handle SynUnif => cAll
                 (*val env' = E.pushCRel env x k*)
             in
                 (*eprefaces "Subst" [("x", Print.PD.string x),
                                    ("cb", p_con env' cb),
                                    ("c2", p_con env c2),
                                    ("sc", p_con env sc)];*)
                 sc
             end
           | L'.CApp (c', i) =>
             (case #1 (hnormCon env c') of
                  L'.CApp (c', f) =>
                  (case #1 (hnormCon env c') of
                       L'.CFold ks =>
                       (case #1 (hnormCon env c2) of
                            L'.CRecord (_, []) => hnormCon env i
                          | L'.CRecord (k, (x, c) :: rest) =>
                            hnormCon env
                                     (L'.CApp ((L'.CApp ((L'.CApp (f, x), loc), c), loc),
                                               (L'.CApp ((L'.CApp ((L'.CApp ((L'.CFold ks, loc), f), loc), i), loc),
                                                         (L'.CRecord (k, rest), loc)), loc)), loc)
                          | L'.CConcat ((L'.CRecord (k, (x, c) :: rest), _), rest') =>
                            let
                                val rest'' = (L'.CConcat ((L'.CRecord (k, rest), loc), rest'), loc)

                                (*val ccc = (L'.CApp ((L'.CApp ((L'.CApp (f, x), loc), c), loc),
                                                   (L'.CApp ((L'.CApp ((L'.CApp ((L'.CFold ks, loc), f), loc), i), loc),
                                                             rest''), loc)), loc)*)
                            in
                                (*eprefaces "Red to" [("ccc", p_con env ccc), ("ccc'", p_con env (hnormCon env ccc))];*)
                                hnormCon env
                                         (L'.CApp ((L'.CApp ((L'.CApp (f, x), loc), c), loc),
                                                   (L'.CApp ((L'.CApp ((L'.CApp ((L'.CFold ks, loc), f), loc), i), loc),
                                                             rest''), loc)), loc)
                            end
                          | _ => cAll)
                     | _ => cAll)
                | _ => cAll)
           | _ => cAll)

      | L'.CConcat (c1, c2) =>
        (case (hnormCon env c1, hnormCon env c2) of
             ((L'.CRecord (k, xcs1), loc), (L'.CRecord (_, xcs2), _)) =>
             (L'.CRecord (k, xcs1 @ xcs2), loc)
           | ((L'.CRecord (_, []), _), c2') => c2'
           | ((L'.CConcat (c11, c12), loc), c2') =>
             hnormCon env (L'.CConcat (c11, (L'.CConcat (c12, c2'), loc)), loc)
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

          | (L'.CModProj (n1, ms1, x1), L'.CModProj (n2, ms2, x2)) =>
            if n1 = n2 andalso ms1 = ms2 andalso x1 = x2 then
                ()
            else
                err CIncompatible

          | (L'.CError, _) => ()
          | (_, L'.CError) => ()

          | (L'.CUnif (_, _, _, ref (SOME c1All)), _) => unifyCons' env c1All c2All
          | (_, L'.CUnif (_, _, _, ref (SOME c2All))) => unifyCons' env c1All c2All

          | (L'.CUnif (_, k1, _, r1), L'.CUnif (_, k2, _, r2)) =>
            if r1 = r2 then
                ()
            else
                (unifyKinds k1 k2;
                 r1 := SOME c2All)

          | (L'.CUnif (_, _, _, r), _) =>
            if occursCon r c2All then
                err COccursCheckFailed
            else
                r := SOME c2All
          | (_, L'.CUnif (_, _, _, r)) =>
            if occursCon r c1All then
                err COccursCheckFailed
            else
                r := SOME c1All

          | (L'.CRecord _, _) => isRecord ()
          | (_, L'.CRecord _) => isRecord ()
          | (L'.CConcat _, _) => isRecord ()
          | (_, L'.CConcat _) => isRecord ()

          | (L'.CFold (dom1, ran1), L'.CFold (dom2, ran2)) =>
            (unifyKinds dom1 dom2;
             unifyKinds ran1 ran2)

          | _ => err CIncompatible
    end

and unifyCons env c1 c2 =
    unifyCons' env c1 c2
    handle CUnify' err => raise CUnify (c1, c2, err)
         | KUnify args => raise CUnify (c1, c2, CKind args)

datatype exp_error =
       UnboundExp of ErrorMsg.span * string
     | UnboundStrInExp of ErrorMsg.span * string
     | Unify of L'.exp * L'.con * L'.con * cunify_error
     | Unif of string * L'.con
     | WrongForm of string * L'.exp * L'.con

fun expError env err =
    case err of
        UnboundExp (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound expression variable " ^ s)
      | UnboundStrInExp (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound structure " ^ s)
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
    case p of
        P.Int _ => !int
      | P.Float _ => !float
      | P.String _ => !string

fun recCons (k, nm, v, rest, loc) =
    (L'.CConcat ((L'.CRecord (k, [(nm, v)]), loc),
                 rest), loc)

fun foldType (dom, loc) =
    (L'.TCFun (L'.Explicit, "ran", (L'.KArrow ((L'.KRecord dom, loc), (L'.KType, loc)), loc),
               (L'.TFun ((L'.TCFun (L'.Explicit, "nm", (L'.KName, loc),
                                    (L'.TCFun (L'.Explicit, "v", dom,
                                               (L'.TCFun (L'.Explicit, "rest", (L'.KRecord dom, loc),
                                                          (L'.TFun ((L'.CApp ((L'.CRel 3, loc), (L'.CRel 0, loc)), loc),
                                                                    (L'.CApp ((L'.CRel 3, loc),
                                                                              recCons (dom,
                                                                                       (L'.CRel 2, loc),
                                                                                       (L'.CRel 1, loc),
                                                                                       (L'.CRel 0, loc),
                                                                                       loc)), loc)), loc)),
                                                loc)), loc)), loc),
                         (L'.TFun ((L'.CApp ((L'.CRel 0, loc), (L'.CRecord (dom, []), loc)), loc),
                                   (L'.TCFun (L'.Explicit, "r", (L'.KRecord dom, loc),
                                              (L'.CApp ((L'.CRel 1, loc), (L'.CRel 0, loc)), loc)), loc)),
                          loc)), loc)), loc)

fun typeof env (e, loc) =
    case e of
        L'.EPrim p => primType env p
      | L'.ERel n => #2 (E.lookupERel env n)
      | L'.ENamed n => #2 (E.lookupENamed env n)
      | L'.EModProj (n, ms, x) =>
        let
            val (_, sgn) = E.lookupStrNamed env n
            val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                       case E.projectStr env {sgn = sgn, str = str, field = m} of
                                           NONE => raise Fail "kindof: Unknown substructure"
                                         | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                             ((L'.StrVar n, loc), sgn) ms
        in
            case E.projectVal env {sgn = sgn, str = str, field = x} of
                NONE => raise Fail "typeof: Unknown val in structure"
              | SOME t => t
        end

      | L'.EApp (e1, _) =>
        (case #1 (typeof env e1) of
             L'.TFun (_, c) => c
           | _ => raise Fail "typeof: Bad EApp")
      | L'.EAbs (_, _, ran, _) => ran
      | L'.ECApp (e1, c) =>
        (case #1 (typeof env e1) of
             L'.TCFun (_, _, _, c1) => subConInCon (0, c) c1
           | _ => raise Fail "typeof: Bad ECApp")
      | L'.ECAbs (expl, x, k, e1) => (L'.TCFun (expl, x, k, typeof (E.pushCRel env x k) e1), loc)

      | L'.ERecord xes => (L'.TRecord (L'.CRecord (ktype, map (fn (x, _, t) => (x, t)) xes), loc), loc)
      | L'.EField (_, _, {field, ...}) => field
      | L'.EFold dom => foldType (dom, loc)

      | L'.EError => cerror

fun elabHead env (e as (_, loc)) t =
    let
        fun unravel (t, e) =
            case hnormCon env t of
                (L'.TCFun (L'.Implicit, x, k, t'), _) =>
                let
                    val u = cunif (loc, k)
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

      | L.EPrim p => ((L'.EPrim p, loc), primType env p)
      | L.EVar ([], s) =>
        (case E.lookupE env s of
             E.NotBound =>
             (expError env (UnboundExp (loc, s));
              (eerror, cerror))
           | E.Rel (n, t) => ((L'.ERel n, loc), t)
           | E.Named (n, t) => ((L'.ENamed n, loc), t))
      | L.EVar (m1 :: ms, s) =>
        (case E.lookupStr env m1 of
             NONE => (expError env (UnboundStrInExp (loc, m1));
                      (eerror, cerror))
           | SOME (n, sgn) =>
             let
                 val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                     case E.projectStr env {sgn = sgn, str = str, field = m} of
                                         NONE => (conError env (UnboundStrInCon (loc, m));
                                                  (strerror, sgnerror))
                                       | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                  ((L'.StrVar n, loc), sgn) ms

                 val t = case E.projectVal env {sgn = sgn, str = str, field = s} of
                             NONE => (expError env (UnboundExp (loc, s));
                                      cerror)
                           | SOME t => t
             in
                 ((L'.EModProj (n, ms, s), loc), t)
             end)

      | L.EApp (e1, e2) =>
        let
            val (e1', t1) = elabExp env e1
            val (e1', t1) = elabHead env e1' t1
            val (e2', t2) = elabExp env e2

            val dom = cunif (loc, ktype)
            val ran = cunif (loc, ktype)
            val t = (L'.TFun (dom, ran), dummy)
        in
            checkCon env e1' t1 t;
            checkCon env e2' t2 dom;
            ((L'.EApp (e1', e2'), loc), ran)
        end
      | L.EAbs (x, to, e) =>
        let
            val t' = case to of
                         NONE => cunif (loc, ktype)
                       | SOME t =>
                         let
                             val (t', tk) = elabCon env t
                         in
                             checkKind env t' tk ktype;
                             t'
                         end
            val (e', et) = elabExp (E.pushERel env x t') e
        in
            ((L'.EAbs (x, t', et, e'), loc),
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
            ((L'.ERecord xes', loc),
             (L'.TRecord (L'.CRecord (ktype, map (fn (x', _, et) => (x', et)) xes'), loc), loc))
        end

      | L.EField (e, c) =>
        let
            val (e', et) = elabExp env e
            val (c', ck) = elabCon env c

            val ft = cunif (loc, ktype)
            val rest = cunif (loc, ktype_record)
        in
            checkKind env c' ck kname;
            checkCon env e' et (L'.TRecord (L'.CConcat ((L'.CRecord (ktype, [(c', ft)]), loc), rest), loc), loc);
            ((L'.EField (e', c', {field = ft, rest = rest}), loc), ft)
        end

      | L.EFold =>
        let
            val dom = kunif loc
        in
            ((L'.EFold dom, loc), foldType (dom, loc))
        end
            

datatype decl_error =
         KunifsRemain of ErrorMsg.span
       | CunifsRemain of ErrorMsg.span

fun declError env err =
    case err of
        KunifsRemain loc =>
        ErrorMsg.errorAt loc "Some kind unification variables are undetermined in declaration"
      | CunifsRemain loc =>
        ErrorMsg.errorAt loc "Some constructor unification variables are undetermined in declaration"

datatype sgn_error =
         UnboundSgn of ErrorMsg.span * string
       | UnmatchedSgi of L'.sgn_item
       | SgiWrongKind of L'.sgn_item * L'.kind * L'.sgn_item * L'.kind * kunify_error
       | SgiWrongCon of L'.sgn_item * L'.con * L'.sgn_item * L'.con * cunify_error
       | SgnWrongForm of L'.sgn * L'.sgn
       | UnWhereable of L'.sgn * string
       | WhereWrongKind of L'.kind * L'.kind * kunify_error
       | NotIncludable of L'.sgn
       | DuplicateCon of ErrorMsg.span * string
       | DuplicateVal of ErrorMsg.span * string
       | DuplicateSgn of ErrorMsg.span * string
       | DuplicateStr of ErrorMsg.span * string

fun sgnError env err =
    case err of
        UnboundSgn (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound signature variable " ^ s)
      | UnmatchedSgi (sgi as (_, loc)) =>
        (ErrorMsg.errorAt loc "Unmatched signature item";
         eprefaces' [("Item", p_sgn_item env sgi)])
      | SgiWrongKind (sgi1, k1, sgi2, k2, kerr) =>
        (ErrorMsg.errorAt (#2 sgi1) "Kind unification failure in signature matching:";
         eprefaces' [("Have", p_sgn_item env sgi1),
                     ("Need", p_sgn_item env sgi2),
                     ("Kind 1", p_kind k1),
                     ("Kind 2", p_kind k2)];
         kunifyError kerr)
      | SgiWrongCon (sgi1, c1, sgi2, c2, cerr) =>
        (ErrorMsg.errorAt (#2 sgi1) "Constructor unification failure in signature matching:";
         eprefaces' [("Have", p_sgn_item env sgi1),
                     ("Need", p_sgn_item env sgi2),
                     ("Con 1", p_con env c1),
                     ("Con 2", p_con env c2)];
         cunifyError env cerr)
      | SgnWrongForm (sgn1, sgn2) =>
        (ErrorMsg.errorAt (#2 sgn1) "Incompatible signatures:";
         eprefaces' [("Sig 1", p_sgn env sgn1),
                     ("Sig 2", p_sgn env sgn2)])
      | UnWhereable (sgn, x) =>
        (ErrorMsg.errorAt (#2 sgn) "Unavailable field for 'where'";
         eprefaces' [("Signature", p_sgn env sgn),
                     ("Field", PD.string x)])
      | WhereWrongKind (k1, k2, kerr) =>
        (ErrorMsg.errorAt (#2 k1) "Wrong kind for 'where'";
         eprefaces' [("Have", p_kind k1),
                     ("Need", p_kind k2)];
         kunifyError kerr)
      | NotIncludable sgn =>
        (ErrorMsg.errorAt (#2 sgn) "Invalid signature to 'include'";
         eprefaces' [("Signature", p_sgn env sgn)])
      | DuplicateCon (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate constructor " ^ s ^ " in signature")
      | DuplicateVal (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate value " ^ s ^ " in signature")
      | DuplicateSgn (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate signature " ^ s ^ " in signature")
      | DuplicateStr (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate structure " ^ s ^ " in signature")

datatype str_error =
         UnboundStr of ErrorMsg.span * string
       | NotFunctor of L'.sgn
       | FunctorRebind of ErrorMsg.span
       | UnOpenable of L'.sgn
       | NotType of L'.kind * (L'.kind * L'.kind * kunify_error)

fun strError env err =
    case err of
        UnboundStr (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound structure variable " ^ s)
      | NotFunctor sgn =>
        (ErrorMsg.errorAt (#2 sgn) "Application of non-functor";
         eprefaces' [("Signature", p_sgn env sgn)])
      | FunctorRebind loc =>
        ErrorMsg.errorAt loc "Attempt to rebind functor"
      | UnOpenable sgn =>
        (ErrorMsg.errorAt (#2 sgn) "Un-openable structure";
         eprefaces' [("Signature", p_sgn env sgn)])
      | NotType (k, (k1, k2, ue)) =>
        (ErrorMsg.errorAt (#2 k) "'val' type kind is not 'Type'";
         eprefaces' [("Kind", p_kind k),
                     ("Subkind 1", p_kind k1),
                     ("Subkind 2", p_kind k2)];
         kunifyError ue)

val hnormSgn = E.hnormSgn

fun elabSgn_item ((sgi, loc), env) =
    case sgi of
        L.SgiConAbs (x, k) =>
        let
            val k' = elabKind k

            val (env', n) = E.pushCNamed env x k' NONE
        in
            ([(L'.SgiConAbs (x, n, k'), loc)], env')
        end

      | L.SgiCon (x, ko, c) =>
        let
            val k' = case ko of
                         NONE => kunif loc
                       | SOME k => elabKind k

            val (c', ck) = elabCon env c
            val (env', n) = E.pushCNamed env x k' (SOME c')
        in
            checkKind env c' ck k';

            ([(L'.SgiCon (x, n, k', c'), loc)], env')
        end

      | L.SgiVal (x, c) =>
        let
            val (c', ck) = elabCon env c

            val (env', n) = E.pushENamed env x c'
        in
            (unifyKinds ck ktype
             handle KUnify ue => strError env (NotType (ck, ue)));

            ([(L'.SgiVal (x, n, c'), loc)], env')
        end

      | L.SgiStr (x, sgn) =>
        let
            val sgn' = elabSgn env sgn
            val (env', n) = E.pushStrNamed env x sgn'
        in
            ([(L'.SgiStr (x, n, sgn'), loc)], env')
        end

      | L.SgiSgn (x, sgn) =>
        let
            val sgn' = elabSgn env sgn
            val (env', n) = E.pushSgnNamed env x sgn'
        in
            ([(L'.SgiSgn (x, n, sgn'), loc)], env')
        end

      | L.SgiInclude sgn =>
        let
            val sgn' = elabSgn env sgn
        in
            case #1 (hnormSgn env sgn') of
                L'.SgnConst sgis =>
                (sgis, foldl (fn (sgi, env) => E.sgiBinds env sgi) env sgis)
              | _ => (sgnError env (NotIncludable sgn');
                      ([], env))
        end

and elabSgn env (sgn, loc) =
    case sgn of
        L.SgnConst sgis =>
        let
            val (sgis', _) = ListUtil.foldlMapConcat elabSgn_item env sgis

            val _ = foldl (fn ((sgi, loc), (cons, vals, sgns, strs)) =>
                              case sgi of
                                  L'.SgiConAbs (x, _, _) =>
                                  (if SS.member (cons, x) then
                                       sgnError env (DuplicateCon (loc, x))
                                   else
                                       ();
                                   (SS.add (cons, x), vals, sgns, strs))
                                | L'.SgiCon (x, _, _, _) =>
                                  (if SS.member (cons, x) then
                                       sgnError env (DuplicateCon (loc, x))
                                   else
                                       ();
                                   (SS.add (cons, x), vals, sgns, strs))
                                | L'.SgiVal (x, _, _) =>
                                  (if SS.member (vals, x) then
                                       sgnError env (DuplicateVal (loc, x))
                                   else
                                       ();
                                   (cons, SS.add (vals, x), sgns, strs))
                                | L'.SgiSgn (x, _, _) =>
                                  (if SS.member (sgns, x) then
                                       sgnError env (DuplicateSgn (loc, x))
                                   else
                                       ();
                                   (cons, vals, SS.add (sgns, x), strs))
                                | L'.SgiStr (x, _, _) =>
                                  (if SS.member (strs, x) then
                                       sgnError env (DuplicateStr (loc, x))
                                   else
                                       ();
                                   (cons, vals, sgns, SS.add (strs, x))))
                    (SS.empty, SS.empty, SS.empty, SS.empty) sgis'
        in
            (L'.SgnConst sgis', loc)
        end
      | L.SgnVar x =>
        (case E.lookupSgn env x of
             NONE =>
             (sgnError env (UnboundSgn (loc, x));
              (L'.SgnError, loc))
           | SOME (n, sgis) => (L'.SgnVar n, loc))
      | L.SgnFun (m, dom, ran) =>
        let
            val dom' = elabSgn env dom
            val (env', n) = E.pushStrNamed env m dom'
            val ran' = elabSgn env' ran
        in
            (L'.SgnFun (m, n, dom', ran'), loc)
        end
      | L.SgnWhere (sgn, x, c) =>
        let
            val sgn' = elabSgn env sgn
            val (c', ck) = elabCon env c
        in
            case #1 (hnormSgn env sgn') of
                L'.SgnError => sgnerror
              | L'.SgnConst sgis =>
                if List.exists (fn (L'.SgiConAbs (x', _, k), _) =>
                                   x' = x andalso
                                   (unifyKinds k ck
                                    handle KUnify x => sgnError env (WhereWrongKind x);
                                    true)
                                 | _ => false) sgis then
                    (L'.SgnWhere (sgn', x, c'), loc)
                else
                    (sgnError env (UnWhereable (sgn', x));
                     sgnerror)
              | _ => (sgnError env (UnWhereable (sgn', x));
                      sgnerror)
        end
      | L.SgnProj (m, ms, x) =>
        (case E.lookupStr env m of
             NONE => (strError env (UnboundStr (loc, m));
                      sgnerror)
           | SOME (n, sgn) =>
             let
                 val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                          case E.projectStr env {sgn = sgn, str = str, field = m} of
                                              NONE => (strError env (UnboundStr (loc, m));
                                                       (strerror, sgnerror))
                                            | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                ((L'.StrVar n, loc), sgn) ms
             in
                 case E.projectSgn env {sgn = sgn, str = str, field = x} of
                     NONE => (sgnError env (UnboundSgn (loc, x));
                              sgnerror)
                   | SOME _ => (L'.SgnProj (n, ms, x), loc)
             end)
                                                              

fun selfify env {str, strs, sgn} =
    case #1 (hnormSgn env sgn) of
        L'.SgnError => sgn
      | L'.SgnVar _ => sgn

      | L'.SgnConst sgis =>
        (L'.SgnConst (map (fn (L'.SgiConAbs (x, n, k), loc) =>
                              (L'.SgiCon (x, n, k, (L'.CModProj (str, strs, x), loc)), loc)
                            | (L'.SgiStr (x, n, sgn), loc) =>
                              (L'.SgiStr (x, n, selfify env {str = str, strs = strs @ [x], sgn = sgn}), loc)
                            | x => x) sgis), #2 sgn)
      | L'.SgnFun _ => sgn
      | L'.SgnWhere _ => sgn
      | L'.SgnProj (m, ms, x) =>
        case E.projectSgn env {str = foldl (fn (m, str) => (L'.StrProj (str, m), #2 sgn))
                                           (L'.StrVar m, #2 sgn) ms,
                               sgn = #2 (E.lookupStrNamed env m),
                               field = x} of
            NONE => raise Fail "Elaborate.selfify: projectSgn returns NONE"
          | SOME sgn => selfify env {str = str, strs = strs, sgn = sgn}

fun selfifyAt env {str, sgn} =
    let
        fun self (str, _) =
            case str of
                L'.StrVar x => SOME (x, [])
              | L'.StrProj (str, x) =>
                (case self str of
                     NONE => NONE
                   | SOME (m, ms) => SOME (m, ms @ [x]))
              | _ => NONE
    in
        case self str of
            NONE => sgn
          | SOME (str, strs) => selfify env {sgn = sgn, str = str, strs = strs}
    end

fun dopen env {str, strs, sgn} =
    let
        val m = foldl (fn (m, str) => (L'.StrProj (str, m), #2 sgn))
                (L'.StrVar str, #2 sgn) strs
    in
        case #1 (hnormSgn env sgn) of
            L'.SgnConst sgis =>
            ListUtil.foldlMap (fn ((sgi, loc), env') =>
                                  case sgi of
                                      L'.SgiConAbs (x, n, k) =>
                                      ((L'.DCon (x, n, k, (L'.CModProj (str, strs, x), loc)), loc),
                                       E.pushCNamedAs env' x n k NONE)
                                    | L'.SgiCon (x, n, k, c) =>
                                      ((L'.DCon (x, n, k, (L'.CModProj (str, strs, x), loc)), loc),
                                       E.pushCNamedAs env' x n k (SOME c))
                                    | L'.SgiVal (x, n, t) =>
                                      ((L'.DVal (x, n, t, (L'.EModProj (str, strs, x), loc)), loc),
                                       E.pushENamedAs env' x n t)
                                    | L'.SgiStr (x, n, sgn) =>
                                      ((L'.DStr (x, n, sgn, (L'.StrProj (m, x), loc)), loc),
                                       E.pushStrNamedAs env' x n sgn)
                                    | L'.SgiSgn (x, n, sgn) =>
                                      ((L'.DSgn (x, n, (L'.SgnProj (str, strs, x), loc)), loc),
                                       E.pushSgnNamedAs env' x n sgn))
                              env sgis
          | _ => (strError env (UnOpenable sgn);
                  ([], env))
    end

fun sgiOfDecl (d, loc) =
    case d of
        L'.DCon (x, n, k, c) => (L'.SgiCon (x, n, k, c), loc)
      | L'.DVal (x, n, t, _) => (L'.SgiVal (x, n, t), loc)
      | L'.DSgn (x, n, sgn) => (L'.SgiSgn (x, n, sgn), loc)
      | L'.DStr (x, n, sgn, _) => (L'.SgiStr (x, n, sgn), loc)
      | L'.DFfiStr (x, n, sgn) => (L'.SgiStr (x, n, sgn), loc)

fun subSgn env sgn1 (sgn2 as (_, loc2)) =
    case (#1 (hnormSgn env sgn1), #1 (hnormSgn env sgn2)) of
        (L'.SgnError, _) => ()
      | (_, L'.SgnError) => ()

      | (L'.SgnConst sgis1, L'.SgnConst sgis2) =>
        let
            fun folder (sgi2All as (sgi, _), env) =
                let
                    fun seek p =
                        let
                            fun seek env ls =
                                case ls of
                                    [] => (sgnError env (UnmatchedSgi sgi2All);
                                           env)
                                  | h :: t =>
                                    case p h of
                                        NONE => seek (E.sgiBinds env h) t
                                      | SOME env => env
                        in
                            seek env sgis1
                        end
                in
                    case sgi of
                        L'.SgiConAbs (x, n2, k2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 let
                                     fun found (x', n1, k1, co1) =
                                         if x = x' then
                                             let
                                                 val () = unifyKinds k1 k2
                                                     handle KUnify (k1, k2, err) =>
                                                            sgnError env (SgiWrongKind (sgi1All, k1, sgi2All, k2, err))
                                                 val env = E.pushCNamedAs env x n1 k1 co1
                                             in
                                                 SOME (if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushCNamedAs env x n2 k2 (SOME (L'.CNamed n1, loc2)))
                                             end
                                         else
                                             NONE
                                 in
                                     case sgi1 of
                                         L'.SgiConAbs (x', n1, k1) => found (x', n1, k1, NONE)
                                       | L'.SgiCon (x', n1, k1, c1) => found (x', n1, k1, SOME c1)
                                       | _ => NONE
                                 end)

                      | L'.SgiCon (x, n2, k2, c2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiCon (x', n1, k1, c1) =>
                                     if x = x' then
                                         let
                                             val () = unifyCons env c1 c2
                                                 handle CUnify (c1, c2, err) =>
                                                        sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err))
                                         in
                                             SOME (E.pushCNamedAs env x n2 k2 (SOME c2))
                                         end
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiVal (x, n2, c2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiVal (x', n1, c1) =>
                                     if x = x' then
                                         let
                                             val () = unifyCons env c1 c2
                                                 handle CUnify (c1, c2, err) =>
                                                        sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err))
                                         in
                                             SOME env
                                         end
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiStr (x, n2, sgn2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiStr (x', n1, sgn1) =>
                                     if x = x' then
                                         let
                                             val () = subSgn env sgn1 sgn2
                                             val env = E.pushStrNamedAs env x n1 sgn1
                                             val env = if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushStrNamedAs env x n2
                                                                            (selfifyAt env {str = (L'.StrVar n1, #2 sgn2),
                                                                                            sgn = sgn2})
                                         in
                                             SOME env
                                         end
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiSgn (x, n2, sgn2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiSgn (x', n1, sgn1) =>
                                     if x = x' then
                                         let
                                             val () = subSgn env sgn1 sgn2
                                             val () = subSgn env sgn2 sgn1

                                             val env = E.pushSgnNamedAs env x n2 sgn2
                                             val env = if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushSgnNamedAs env x n1 sgn2
                                         in
                                             SOME env
                                         end
                                     else
                                         NONE
                                   | _ => NONE)
                end
        in
            ignore (foldl folder env sgis2)
        end

      | (L'.SgnFun (m1, n1, dom1, ran1), L'.SgnFun (m2, n2, dom2, ran2)) =>
        let
            val ran1 =
                if n1 = n2 then
                    ran1
                else
                    subStrInSgn (n1, n2) ran1
        in
            subSgn env dom2 dom1;
            subSgn (E.pushStrNamedAs env m2 n2 dom2) ran1 ran2
        end

      | _ => sgnError env (SgnWrongForm (sgn1, sgn2))


fun elabDecl ((d, loc), env) =
    case d of
        L.DCon (x, ko, c) =>
        let
            val k' = case ko of
                         NONE => kunif loc
                       | SOME k => elabKind k

            val (c', ck) = elabCon env c
            val (env', n) = E.pushCNamed env x k' (SOME c')
        in
            checkKind env c' ck k';

            ([(L'.DCon (x, n, k', c'), loc)], env')
        end
      | L.DVal (x, co, e) =>
        let
            val (c', ck) = case co of
                               NONE => (cunif (loc, ktype), ktype)
                             | SOME c => elabCon env c

            val (e', et) = elabExp env e
            val (env', n) = E.pushENamed env x c'
        in
            checkCon env e' et c';

            ([(L'.DVal (x, n, c', e'), loc)], env')
        end

      | L.DSgn (x, sgn) =>
        let
            val sgn' = elabSgn env sgn
            val (env', n) = E.pushSgnNamed env x sgn'
        in
            ([(L'.DSgn (x, n, sgn'), loc)], env')
        end

      | L.DStr (x, sgno, str) =>
        let
            val formal = Option.map (elabSgn env) sgno

            val (str', sgn') =
                case formal of
                    NONE =>
                    let
                        val (str', actual) = elabStr env str
                    in
                        (str', selfifyAt env {str = str', sgn = actual})
                    end
                  | SOME formal =>
                    let
                        val str =
                            case #1 (hnormSgn env formal) of
                                L'.SgnConst sgis =>
                                (case #1 str of
                                     L.StrConst ds =>
                                     let
                                         val needed = foldl (fn ((sgi, _), needed) =>
                                                                case sgi of
                                                                    L'.SgiConAbs (x, _, _) => SS.add (needed, x)
                                                                  | _ => needed)
                                                            SS.empty sgis
                                                      
                                         val needed = foldl (fn ((d, _), needed) =>
                                                                case d of
                                                                    L.DCon (x, _, _) => (SS.delete (needed, x)
                                                                                         handle NotFound => needed)
                                                                  | L.DOpen _ => SS.empty
                                                                  | _ => needed)
                                                            needed ds
                                     in
                                         case SS.listItems needed of
                                             [] => str
                                           | xs =>
                                             let
                                                 val kwild = (L.KWild, #2 str)
                                                 val cwild = (L.CWild kwild, #2 str)
                                                 val ds' = map (fn x => (L.DCon (x, NONE, cwild), #2 str)) xs
                                             in
                                                 (L.StrConst (ds @ ds'), #2 str)
                                             end
                                     end
                                   | _ => str)
                              | _ => str

                        val (str', actual) = elabStr env str
                    in
                        subSgn env actual formal;
                        (str', formal)
                    end

            val (env', n) = E.pushStrNamed env x sgn'
        in
            case #1 (hnormSgn env sgn') of
                L'.SgnFun _ =>
                (case #1 str' of
                     L'.StrFun _ => ()
                   | _ => strError env (FunctorRebind loc))
              | _ => ();

            ([(L'.DStr (x, n, sgn', str'), loc)], env')
        end

      | L.DFfiStr (x, sgn) =>
        let
            val sgn' = elabSgn env sgn

            val (env', n) = E.pushStrNamed env x sgn'
        in
            ([(L'.DFfiStr (x, n, sgn'), loc)], env')
        end

      | L.DOpen (m, ms) =>
        case E.lookupStr env m of
            NONE => (strError env (UnboundStr (loc, m));
                     ([], env))
          | SOME (n, sgn) =>
            let
                val (_, sgn) = foldl (fn (m, (str, sgn)) =>
                                         case E.projectStr env {str = str, sgn = sgn, field = m} of
                                             NONE => (strError env (UnboundStr (loc, m));
                                                      (strerror, sgnerror))
                                           | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                     ((L'.StrVar n, loc), sgn) ms
            in
                dopen env {str = n, strs = ms, sgn = sgn}
            end

and elabStr env (str, loc) =
    case str of
        L.StrConst ds =>
        let
            val (ds', env') = ListUtil.foldlMapConcat elabDecl env ds
            val sgis = map sgiOfDecl ds'

            val (sgis, _, _, _, _) =
                foldr (fn ((sgi, loc), (sgis, cons, vals, sgns, strs)) =>
                          case sgi of
                              L'.SgiConAbs (x, n, k) =>
                              let
                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiConAbs (x, n, k), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiCon (x, n, k, c) =>
                              let
                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiCon (x, n, k, c), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiVal (x, n, c) =>
                              let
                                  val (vals, x) =
                                      if SS.member (vals, x) then
                                          (vals, "?" ^ x)
                                      else
                                          (SS.add (vals, x), x)
                              in
                                  ((L'.SgiVal (x, n, c), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiSgn (x, n, sgn) =>
                              let
                                  val (sgns, x) =
                                      if SS.member (sgns, x) then
                                          (sgns, "?" ^ x)
                                      else
                                          (SS.add (sgns, x), x)
                              in
                                  ((L'.SgiSgn (x, n, sgn), loc) :: sgis, cons, vals, sgns, strs)
                              end
 
                            | L'.SgiStr (x, n, sgn) =>
                              let
                                  val (strs, x) =
                                      if SS.member (strs, x) then
                                          (strs, "?" ^ x)
                                      else
                                          (SS.add (strs, x), x)
                              in
                                  ((L'.SgiStr (x, n, sgn), loc) :: sgis, cons, vals, sgns, strs)
                              end)

                ([], SS.empty, SS.empty, SS.empty, SS.empty) sgis
        in
            ((L'.StrConst ds', loc), (L'.SgnConst sgis, loc))
        end
      | L.StrVar x =>
        (case E.lookupStr env x of
             NONE =>
             (strError env (UnboundStr (loc, x));
              (strerror, sgnerror))
           | SOME (n, sgn) => ((L'.StrVar n, loc), sgn))
      | L.StrProj (str, x) =>
        let
            val (str', sgn) = elabStr env str
        in
            case E.projectStr env {str = str', sgn = sgn, field = x} of
                NONE => (strError env (UnboundStr (loc, x));
                         (strerror, sgnerror))
              | SOME sgn => ((L'.StrProj (str', x), loc), sgn)
        end
      | L.StrFun (m, dom, ranO, str) =>
        let
            val dom' = elabSgn env dom
            val (env', n) = E.pushStrNamed env m dom'
            val (str', actual) = elabStr env' str

            val formal =
                case ranO of
                    NONE => actual
                  | SOME ran =>
                    let
                        val ran' = elabSgn env' ran
                    in
                        subSgn env' actual ran';
                        ran'
                    end
        in
            ((L'.StrFun (m, n, dom', formal, str'), loc),
             (L'.SgnFun (m, n, dom', formal), loc))
        end
      | L.StrApp (str1, str2) =>
        let
            val (str1', sgn1) = elabStr env str1
            val (str2', sgn2) = elabStr env str2
        in
            case #1 (hnormSgn env sgn1) of
                L'.SgnError => (strerror, sgnerror)
              | L'.SgnFun (m, n, dom, ran) =>
                (subSgn env sgn2 dom;
                 case #1 (hnormSgn env ran) of
                     L'.SgnError => (strerror, sgnerror)
                   | L'.SgnConst sgis =>
                     ((L'.StrApp (str1', str2'), loc),
                      (L'.SgnConst ((L'.SgiStr (m, n, selfifyAt env {str = str2', sgn = sgn2}), loc) :: sgis), loc))
                   | _ => raise Fail "Unable to hnormSgn in functor application")
              | _ => (strError env (NotFunctor sgn1);
                      (strerror, sgnerror))
        end

fun elabFile basis env file =
    let
        val sgn = elabSgn env (L.SgnConst basis, ErrorMsg.dummySpan)
        val (env', basis_n) = E.pushStrNamed env "Basis" sgn

        val (ds, env') = dopen env' {str = basis_n, strs = [], sgn = sgn}

        fun discoverC r x =
            case E.lookupC env' x of
                E.NotBound => raise Fail ("Constructor " ^ x ^ " unbound in Basis")
              | E.Rel _ => raise Fail ("Constructor " ^ x ^ " bound relatively in Basis")
              | E.Named (n, (_, loc)) => r := (L'.CNamed n, loc)

        val () = discoverC int "int"
        val () = discoverC float "float"
        val () = discoverC string "string"

        fun elabDecl' (d, env) =
            let
                val () = resetKunif ()
                val () = resetCunif ()
                val (ds, env) = elabDecl (d, env)
            in
                if ErrorMsg.anyErrors () then
                    ()
                else (
                    if List.exists kunifsInDecl ds then
                        declError env (KunifsRemain (#2 d))
                    else
                        ();
                    
                    case ListUtil.search cunifsInDecl ds of
                        NONE => ()
                      | SOME loc =>
                        declError env (CunifsRemain loc)
                    );

                (ds, env)
            end

        val (file, _) = ListUtil.foldlMapConcat elabDecl' env' file
    in
        (L'.DFfiStr ("Basis", basis_n, sgn), ErrorMsg.dummySpan) :: ds @ file
    end

end
